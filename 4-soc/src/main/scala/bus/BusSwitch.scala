// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package bus

import chisel3._
import chisel3.util._
import riscv.Parameters

class BusSwitch extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(Parameters.AddrWidth))
    val slaves  = Vec(Parameters.SlaveDeviceCount, new AXI4LiteChannels(Parameters.AddrBits, Parameters.DataBits))
    val master  = Flipped(new AXI4LiteChannels(Parameters.AddrBits, Parameters.DataBits))
  })

  val index = io.address(Parameters.AddrBits - 1, Parameters.AddrBits - Parameters.SlaveDeviceCountBits)
  val sel   = UIntToOH(index, Parameters.SlaveDeviceCount)

  // Drive slaves: only the selected slave sees VALID/READY handshakes.
  for (i <- 0 until Parameters.SlaveDeviceCount) {
    val hit = sel(i)

    io.slaves(i).write_address_channel.AWVALID := io.master.write_address_channel.AWVALID && hit
    io.slaves(i).write_address_channel.AWADDR  := io.master.write_address_channel.AWADDR
    io.slaves(i).write_address_channel.AWPROT  := io.master.write_address_channel.AWPROT

    io.slaves(i).write_data_channel.WVALID := io.master.write_data_channel.WVALID && hit
    io.slaves(i).write_data_channel.WDATA  := io.master.write_data_channel.WDATA
    io.slaves(i).write_data_channel.WSTRB  := io.master.write_data_channel.WSTRB

    io.slaves(i).write_response_channel.BREADY := io.master.write_response_channel.BREADY && hit

    io.slaves(i).read_address_channel.ARVALID := io.master.read_address_channel.ARVALID && hit
    io.slaves(i).read_address_channel.ARADDR  := io.master.read_address_channel.ARADDR
    io.slaves(i).read_address_channel.ARPROT  := io.master.read_address_channel.ARPROT

    io.slaves(i).read_data_channel.RREADY := io.master.read_data_channel.RREADY && hit
  }

  // Multiplex slave responses back to the master
  io.master.write_address_channel.AWREADY := Mux1H(sel, io.slaves.map(_.write_address_channel.AWREADY))
  io.master.write_data_channel.WREADY     := Mux1H(sel, io.slaves.map(_.write_data_channel.WREADY))
  io.master.write_response_channel.BVALID := Mux1H(sel, io.slaves.map(_.write_response_channel.BVALID))
  io.master.write_response_channel.BRESP  := Mux1H(sel, io.slaves.map(_.write_response_channel.BRESP))

  io.master.read_address_channel.ARREADY := Mux1H(sel, io.slaves.map(_.read_address_channel.ARREADY))
  io.master.read_data_channel.RVALID     := Mux1H(sel, io.slaves.map(_.read_data_channel.RVALID))
  io.master.read_data_channel.RDATA      := Mux1H(sel, io.slaves.map(_.read_data_channel.RDATA))
  io.master.read_data_channel.RRESP      := Mux1H(sel, io.slaves.map(_.read_data_channel.RRESP))
}
