// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util._
import riscv.core.BusBundle
import riscv.Parameters

object MemoryAccessStates extends ChiselEnum {
  val Idle, Read, Write = Value
}

class MemoryAccess extends Module {
  val io = IO(new Bundle() {
    val alu_result          = Input(UInt(Parameters.DataWidth)) // used as memory address
    val reg2_data           = Input(UInt(Parameters.DataWidth))
    val memory_read_enable  = Input(Bool())
    val memory_write_enable = Input(Bool())
    val funct3              = Input(UInt(3.W))
    val regs_write_source   = Input(UInt(2.W))
    val csr_read_data       = Input(UInt(Parameters.DataWidth))
    val instruction_address = Input(UInt(Parameters.AddrWidth)) // For JAL/JALR forwarding (PC+4)

    val wb_memory_read_data = Output(UInt(Parameters.DataWidth))
    val forward_to_ex       = Output(UInt(Parameters.DataWidth))
    val ctrl_stall_flag     = Output(Bool()) // stall when memory access is not finished
    // Output the correct regs_write_source for MEM2WB pipeline register
    // This preserves the load instruction's regs_write_source when stall releases
    val wb_regs_write_source = Output(UInt(2.W))

    val bus = new BusBundle
  })
  val mem_address_index = io.alu_result(log2Up(Parameters.WordSize) - 1, 0).asUInt
  val mem_access_state  = RegInit(MemoryAccessStates.Idle)

  // Register to hold the loaded data across the stall release cycle
  // This ensures the data persists until MEM2WB captures it
  val latched_memory_read_data = RegInit(0.U(Parameters.DataWidth))

  // Capture control signals when entering Read state
  // PipelineRegister has combinational bypass (out := io.in when ~stall), so when
  // stall releases on read_valid, signals from ex2mem immediately change to the NEXT
  // instruction's value. By latching them when we start the read, we preserve the
  // correct values for the load instruction throughout the bus transaction.
  val latched_regs_write_source = RegInit(0.U(2.W))
  val latched_funct3            = RegInit(0.U(3.W))
  val latched_address_index     = RegInit(0.U(log2Up(Parameters.WordSize).W))

  // Track when a read just completed so we can extend the validity of
  // latched control signals for one more cycle. This handles the case where the
  // dependent instruction arrives in EX on the cycle AFTER the load completes.
  // Without this, forward_to_ex would show the wrong value (ALU result instead of
  // loaded data) because effective_regs_write_source switches too early.
  val read_just_completed = RegInit(false.B)

  // Helper for common transaction completion logic (state machine reset only)
  def on_bus_transaction_finished() = {
    mem_access_state   := MemoryAccessStates.Idle
    io.ctrl_stall_flag := false.B
    // NOTE: read_just_completed is set ONLY in Read completion path, NOT here
    // Setting it for Write completions was a bug causing wrong wb_regs_write_source
  }

  // Clear read_just_completed on the cycle after it was set
  // This ensures latched values are used for exactly one cycle after completion
  when(read_just_completed && mem_access_state === MemoryAccessStates.Idle) {
    read_just_completed := false.B
  }

  io.bus.request := false.B
  io.bus.read    := false.B
  io.bus.address := io.alu_result(Parameters.AddrBits - 1, log2Up(Parameters.WordSize)) ## 0.U(
    log2Up(Parameters.WordSize).W
  )
  io.bus.write_data      := 0.U
  io.bus.write_strobe    := VecInit(Seq.fill(Parameters.WordSize)(false.B))
  io.bus.write           := false.B
  io.wb_memory_read_data := latched_memory_read_data // Use latched value
  io.ctrl_stall_flag     := false.B

  // Misaligned access detection
  // RISC-V spec allows implementation-defined behavior for misaligned accesses.
  // Current implementation: No exception raised; hardware handles byte extraction.
  // For strict compliance, uncomment below to detect and signal misaligned access:
  //
  // val is_halfword = io.funct3 === InstructionsTypeL.lh ||
  //                   io.funct3 === InstructionsTypeL.lhu ||
  //                   io.funct3 === InstructionsTypeS.sh
  // val is_word = io.funct3 === InstructionsTypeL.lw ||
  //               io.funct3 === InstructionsTypeS.sw
  // val misaligned = (is_halfword && mem_address_index(0)) ||
  //                  (is_word && mem_address_index =/= 0.U)
  // io.misaligned_exception := misaligned && (io.memory_read_enable || io.memory_write_enable)

  // State machine: handle Read/Write completion FIRST (independent of enable signals)
  // This fixes a critical bug where the state machine would get stuck if the pipeline
  // moved on (enable went low) before the bus transaction completed.
  when(mem_access_state === MemoryAccessStates.Read) {
    // In Read state: wait for read_valid, keep stalling
    io.bus.request     := true.B
    io.ctrl_stall_flag := true.B
    when(io.bus.read_valid) {
      val data = io.bus.read_data
      // Compute the processed data (byte/halfword extraction with sign extension)
      // Use latched_funct3 and latched_address_index instead of io.funct3/mem_address_index
      // because stall releases on this cycle and those signals change to NEXT instruction's values
      val processed_data = MuxLookup(
        latched_funct3,
        0.U,
        IndexedSeq(
          InstructionsTypeL.lb -> MuxLookup(
            latched_address_index,
            Cat(Fill(24, data(31)), data(31, 24)),
            IndexedSeq(
              0.U -> Cat(Fill(24, data(7)), data(7, 0)),
              1.U -> Cat(Fill(24, data(15)), data(15, 8)),
              2.U -> Cat(Fill(24, data(23)), data(23, 16))
            )
          ),
          InstructionsTypeL.lbu -> MuxLookup(
            latched_address_index,
            Cat(Fill(24, 0.U), data(31, 24)),
            IndexedSeq(
              0.U -> Cat(Fill(24, 0.U), data(7, 0)),
              1.U -> Cat(Fill(24, 0.U), data(15, 8)),
              2.U -> Cat(Fill(24, 0.U), data(23, 16))
            )
          ),
          InstructionsTypeL.lh -> Mux(
            latched_address_index === 0.U,
            Cat(Fill(16, data(15)), data(15, 0)),
            Cat(Fill(16, data(31)), data(31, 16))
          ),
          InstructionsTypeL.lhu -> Mux(
            latched_address_index === 0.U,
            Cat(Fill(16, 0.U), data(15, 0)),
            Cat(Fill(16, 0.U), data(31, 16))
          ),
          InstructionsTypeL.lw -> data
        )
      )
      // Store in register for persistence after read_valid goes low
      latched_memory_read_data := processed_data
      // Also output immediately for forwarding on THIS cycle
      // Without this, the forwarding path would see the old latch value (0)
      io.wb_memory_read_data := processed_data
      // Signal that a READ just completed - used by wb_effective_regs_write_source MUX
      // to extend latched control signals for one more cycle
      read_just_completed := true.B
      on_bus_transaction_finished()
    }
  }.elsewhen(mem_access_state === MemoryAccessStates.Write) {
    // In Write state: wait for write_valid (BRESP) to complete transaction
    // CRITICAL: Must keep stall asserted until BRESP received.
    // The posted-write optimization (releasing stall on write_data_accepted)
    // was buggy: while BRESP is pending, the pipeline advances but the state
    // machine remains in Write state, causing any new load/store in MEM stage
    // to be ignored (the .otherwise block only runs in Idle state).
    // Conservative fix: stall until write completion to ensure correctness.
    io.bus.request     := true.B
    io.ctrl_stall_flag := true.B

    when(io.bus.write_valid) {
      on_bus_transaction_finished()
    }
  }.otherwise {
    // Idle state: check enable signals to start new transactions
    when(io.memory_read_enable) {
      // Start the read transaction when the bus is available
      io.ctrl_stall_flag := true.B
      io.bus.read        := true.B
      io.bus.request     := true.B
      // Capture ALL control signals IMMEDIATELY when read starts
      // These must be latched because PipelineRegister's combinational bypass
      // will change them on stall release BEFORE the read_valid processing completes
      latched_regs_write_source := io.regs_write_source
      latched_funct3            := io.funct3
      latched_address_index     := mem_address_index
      when(io.bus.granted) {
        mem_access_state := MemoryAccessStates.Read
      }
    }.elsewhen(io.memory_write_enable) {
      // Start the write transaction when the bus is available
      io.ctrl_stall_flag  := true.B
      io.bus.write_data   := io.reg2_data
      io.bus.write        := true.B
      io.bus.write_strobe := VecInit(Seq.fill(Parameters.WordSize)(false.B))
      when(io.funct3 === InstructionsTypeS.sb) {
        io.bus.write_strobe(mem_address_index) := true.B
        // Fix: Use ByteBits-1 for correct 8-bit slice (was ByteBits which gave 9 bits)
        io.bus.write_data := io.reg2_data(Parameters.ByteBits - 1, 0) << (mem_address_index << log2Up(
          Parameters.ByteBits
        ).U)
      }.elsewhen(io.funct3 === InstructionsTypeS.sh) {
        when(mem_address_index === 0.U) {
          for (i <- 0 until Parameters.WordSize / 2) {
            io.bus.write_strobe(i) := true.B
          }
          // Fix: Use -1 for correct 16-bit slice (was 17 bits)
          io.bus.write_data := io.reg2_data(Parameters.WordSize / 2 * Parameters.ByteBits - 1, 0)
        }.otherwise {
          for (i <- Parameters.WordSize / 2 until Parameters.WordSize) {
            io.bus.write_strobe(i) := true.B
          }
          // Fix: Use -1 for correct 16-bit slice (was 17 bits)
          io.bus.write_data := io.reg2_data(
            Parameters.WordSize / 2 * Parameters.ByteBits - 1,
            0
          ) << (Parameters.WordSize / 2 * Parameters.ByteBits)
        }
      }.elsewhen(io.funct3 === InstructionsTypeS.sw) {
        for (i <- 0 until Parameters.WordSize) {
          io.bus.write_strobe(i) := true.B
        }
      }
      io.bus.request := true.B
      when(io.bus.granted) {
        mem_access_state := MemoryAccessStates.Write
      }
    }
  }

  // Forward to EX stage: Select correct data source for forwarding
  // - Memory loads: forward the loaded data (wb_memory_read_data)
  // - CSR instructions: forward CSR read data
  // - ALU operations: forward ALU result
  //
  // For forwarding to EX, only use latched_regs_write_source when actually
  // IN Read state. When read_just_completed is true, a NEW instruction is in MEM stage,
  // so forwarding must use THAT instruction's regs_write_source (from io.regs_write_source).
  //
  // The previous bug: When read_just_completed was true, we used latched_regs_write_source
  // (FromMemory=1) even though LUI (FromALU=0) was now in MEM stage. This caused forward_to_ex
  // to output stale memory read data instead of the LUI's ALU result.
  val forward_regs_write_source = Mux(
    mem_access_state === MemoryAccessStates.Read,
    latched_regs_write_source,
    io.regs_write_source
  )
  // CRITICAL FIX: Handle JAL/JALR forwarding correctly
  // JAL/JALR instructions set rd = PC + 4, computed in WriteBack stage.
  // When JAL/JALR is in MEM stage, forwarding must provide PC+4, NOT ALU result.
  // Previously, NextInstructionAddress fell through to default (alu_result),
  // causing SW ra to get wrong value (jump target instead of return address).
  io.forward_to_ex := MuxLookup(forward_regs_write_source, io.alu_result)(
    Seq(
      RegWriteSource.Memory                 -> io.wb_memory_read_data,
      RegWriteSource.CSR                    -> io.csr_read_data,
      RegWriteSource.NextInstructionAddress -> (io.instruction_address + 4.U)
    )
  )

  // For writeback, ONLY use latched values while actually IN Read state.
  // When read_just_completed is true, the load has ALREADY completed and a NEW
  // instruction is now in MEM stage. We must use io.regs_write_source for that
  // new instruction, NOT the latched value from the completed load.
  // Bug fix: Previously this included `|| read_just_completed` which corrupted
  // the regs_write_source for instructions following loads (e.g., ADDI after LW).
  val wb_effective_regs_write_source = Mux(
    mem_access_state === MemoryAccessStates.Read,
    latched_regs_write_source,
    io.regs_write_source
  )
  // Pass effective_regs_write_source to MEM2WB pipeline register
  // This ensures WB stage gets the correct regs_write_source even when stall releases
  io.wb_regs_write_source := wb_effective_regs_write_source
}
