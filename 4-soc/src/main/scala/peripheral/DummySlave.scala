// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package peripheral

import bus.AXI4LiteChannels
import chisel3._
import riscv.Parameters

// A dummy AXI4 slave that doesn't respond to anything
class DummySlave extends Module {
  val io = IO(new Bundle {
    val channels = Flipped(new AXI4LiteChannels(4, Parameters.DataBits))
  })

  io.channels.read_address_channel.ARREADY  := false.B
  io.channels.read_data_channel.RVALID      := false.B
  io.channels.read_data_channel.RDATA       := 0.U
  io.channels.read_data_channel.RRESP       := 0.U
  io.channels.write_address_channel.AWREADY := false.B
  io.channels.write_data_channel.WREADY     := false.B
  io.channels.write_response_channel.BVALID := false.B
  io.channels.write_response_channel.BRESP  := 0.U
}
