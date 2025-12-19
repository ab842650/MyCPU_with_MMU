// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package peripheral

import bus.AXI4LiteChannels
import bus.AXI4LiteSlave
import chisel3._
import chisel3.util._
import riscv.Parameters

class UartIO extends DecoupledIO(UInt(8.W))

/**
 * Transmit part of the UART.
 * A minimal version without any additional buffering.
 * Use a ready/valid handshaking.
 */
class Tx(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val txd     = Output(UInt(1.W))
    val channel = Flipped(new UartIO())
  })

  val BIT_CNT = ((frequency + baudRate / 2) / baudRate - 1).U

  val shiftReg = RegInit(0x7ff.U)
  val cntReg   = RegInit(0.U(20.W))
  val bitsReg  = RegInit(0.U(4.W))

  io.channel.ready := (cntReg === 0.U) && (bitsReg === 0.U)
  io.txd           := shiftReg(0)

  when(cntReg === 0.U) {
    cntReg := BIT_CNT
    when(bitsReg =/= 0.U) {
      val shift = shiftReg >> 1
      shiftReg := Cat(1.U, shift(9, 0))
      bitsReg  := bitsReg - 1.U
    }.otherwise {
      when(io.channel.valid) {
        shiftReg := Cat(Cat(3.U, io.channel.bits), 0.U) // two stop bits, data, one start bit
        bitsReg  := 11.U
      }.otherwise {
        shiftReg := 0x7ff.U
      }
    }
  }.otherwise {
    cntReg := cntReg - 1.U
  }
}

/**
 * Receive part of the UART.
 * A minimal version without any additional buffering.
 * Use a ready/valid handshaking.
 */
class Rx(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val rxd     = Input(UInt(1.W))
    val channel = new UartIO()
  })

  val BIT_CNT   = ((frequency + baudRate / 2) / baudRate - 1).U
  val START_CNT = ((3 * frequency / 2 + baudRate / 2) / baudRate - 1).U

  // Sync in the asynchronous RX data
  val rxReg = RegNext(RegNext(io.rxd, 1.U), 1.U)

  val shiftReg = RegInit(0.U(8.W))
  val cntReg   = RegInit(0.U(20.W))
  val bitsReg  = RegInit(0.U(4.W))
  val valReg   = RegInit(false.B)

  when(cntReg =/= 0.U) {
    cntReg := cntReg - 1.U
  }.elsewhen(bitsReg =/= 0.U) {
    cntReg   := BIT_CNT
    shiftReg := Cat(rxReg, shiftReg >> 1)
    bitsReg  := bitsReg - 1.U
    when(bitsReg === 1.U) {
      valReg := true.B
    }
  }.elsewhen(rxReg === 0.U) {
    cntReg  := START_CNT
    bitsReg := 8.U
  }

  when(valReg && io.channel.ready) {
    valReg := false.B
  }

  io.channel.bits  := shiftReg
  io.channel.valid := valReg
}

/**
 * A single byte buffer with a ready/valid interface
 */
class Buffer extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(new UartIO())
    val out = new UartIO()
  })

  val empty :: full :: Nil = Enum(2)
  val stateReg             = RegInit(empty)
  val dataReg              = RegInit(0.U(8.W))

  io.in.ready  := stateReg === empty
  io.out.valid := stateReg === full

  when(stateReg === empty) {
    when(io.in.valid) {
      dataReg  := io.in.bits
      stateReg := full
    }
  }.otherwise {
    when(io.out.ready) {
      stateReg := empty
    }
  }
  io.out.bits := dataReg
}

/**
 * A transmitter with a single buffer.
 */
class BufferedTx(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val txd     = Output(UInt(1.W))
    val channel = Flipped(new UartIO())
  })
  val tx  = Module(new Tx(frequency, baudRate))
  val buf = Module(new Buffer)

  buf.io.in <> io.channel
  tx.io.channel <> buf.io.out
  io.txd <> tx.io.txd
}

/**
 * UART peripheral with AXI4-Lite interface
 *
 * Memory map (offset from base address):
 *   0x00: STATUS    - TX ready (bit 0), RX valid (bit 1) (read-only)
 *   0x04: BAUD_RATE - Configured baud rate (read-only)
 *   0x08: INTERRUPT - Interrupt enable/status (write: enable, read: N/A)
 *   0x0C: RX_DATA   - Received data (read clears interrupt)
 *   0x10: TX_DATA   - Transmit data (write-only)
 *
 * Limitations:
 *   - Single-byte TX buffer: If TX buffer is full when CPU writes to TX_DATA,
 *     the write is silently dropped. Software should poll STATUS.tx_ready
 *     before writing. For production use, consider adding a TX FIFO.
 *   - Single-byte RX buffer: If CPU doesn't read RX_DATA before next byte
 *     arrives, the old byte is overwritten (no overrun interrupt).
 *
 * @param frequency System clock frequency in Hz
 * @param baudRate  Serial baud rate (e.g., 115200)
 */
class Uart(frequency: Int, baudRate: Int) extends Module {
  val io = IO(new Bundle {
    val channels         = Flipped(new AXI4LiteChannels(8, Parameters.DataBits))
    val rxd              = Input(UInt(1.W))
    val txd              = Output(UInt(1.W))
    val signal_interrupt = Output(Bool())
  })

  val interrupt = RegInit(false.B)
  val rxData    = RegInit(0.U)
  val slave     = Module(new AXI4LiteSlave(8, Parameters.DataBits))
  slave.io.channels <> io.channels

  val tx = Module(new BufferedTx(frequency, baudRate))
  val rx = Module(new Rx(frequency, baudRate))

  // MMIO address decode (mask to get offset within peripheral)
  // UART registers at 0x00-0x1F, base address 0x40000000
  val addr           = slave.io.bundle.address & 0xff.U
  val addr_status    = addr === 0x00.U // Status register (read-only)
  val addr_baud_rate = addr === 0x04.U // Baud rate register (read-only)
  val addr_interrupt = addr === 0x08.U // Interrupt control (write-only)
  val addr_rx_data   = addr === 0x0c.U // RX data register (read, clears interrupt)
  val addr_tx_data   = addr === 0x10.U // TX data register (write-only)

  slave.io.bundle.read_data  := 0.U
  slave.io.bundle.read_valid := true.B
  when(slave.io.bundle.read) {
    when(addr_status) {
      // Status register: bit 0 = TX ready, bit 1 = RX data valid
      slave.io.bundle.read_data := Cat(0.U(30.W), rx.io.channel.valid, tx.io.channel.ready)
    }.elsewhen(addr_baud_rate) {
      slave.io.bundle.read_data := baudRate.U
    }.elsewhen(addr_rx_data) {
      slave.io.bundle.read_data := rxData
      interrupt                 := false.B
    }
  }

  // TX channel: only write when buffer is ready (backpressure handling)
  tx.io.channel.valid := false.B
  tx.io.channel.bits  := 0.U
  when(slave.io.bundle.write) {
    when(addr_interrupt) {
      interrupt := slave.io.bundle.write_data =/= 0.U
    }.elsewhen(addr_tx_data && tx.io.channel.ready) {
      // Only write to TX when buffer has space (prevents silent data loss)
      tx.io.channel.valid := true.B
      // Explicit 8-bit slice: UART TX is 8 bits, write_data is 32 bits
      // Standard UART practice: use lower byte, ignore upper bits
      tx.io.channel.bits := slave.io.bundle.write_data(7, 0)
    }
    // Note: If TX buffer full (ready=false), AXI write completes but data is dropped.
    // Production implementation should stall AXI response until buffer ready.
  }

  io.txd    := tx.io.txd
  rx.io.rxd := io.rxd

  io.signal_interrupt := interrupt
  rx.io.channel.ready := false.B
  when(rx.io.channel.valid) {
    rx.io.channel.ready := true.B
    rxData              := rx.io.channel.bits
    interrupt           := true.B
  }
}
