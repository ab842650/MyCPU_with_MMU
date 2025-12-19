// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package peripheral

import bus.AXI4LiteChannels
import chisel3._
import riscv.Parameters

// A dummy master that never initiates reads or writes
class DummyMaster extends Module {
  val io = IO(new Bundle {
    val channels = new AXI4LiteChannels(Parameters.AddrBits, Parameters.DataBits)
  })

  io.channels.read_address_channel.ARVALID := false.B
  io.channels.read_address_channel.ARADDR  := 0.U
  io.channels.read_address_channel.ARPROT  := 0.U
  io.channels.read_data_channel.RREADY     := false.B

  io.channels.write_address_channel.AWVALID := false.B
  io.channels.write_address_channel.AWADDR  := 0.U
  io.channels.write_address_channel.AWPROT  := 0.U
  io.channels.write_data_channel.WVALID     := false.B
  io.channels.write_data_channel.WDATA      := 0.U
  io.channels.write_data_channel.WSTRB      := 0.U
  io.channels.write_response_channel.BREADY := false.B
}
