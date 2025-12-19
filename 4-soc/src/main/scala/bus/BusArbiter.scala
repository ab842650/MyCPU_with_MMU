// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package bus

import chisel3._
import riscv.Parameters

class BusArbiter extends Module {
  val io = IO(new Bundle {
    val bus_request     = Input(Vec(Parameters.MasterDeviceCount, Bool()))
    val bus_granted     = Output(Vec(Parameters.MasterDeviceCount, Bool()))
    val ctrl_stall_flag = Output(Bool())
  })

  val granted = Wire(UInt())
  // Static Priority Arbitration
  // Higher number = Higher priority
  granted := 0.U
  for (i <- 0 until Parameters.MasterDeviceCount) {
    when(io.bus_request(i.U)) {
      granted := i.U
    }
  }
  for (i <- 0 until Parameters.MasterDeviceCount) {
    io.bus_granted(i.U) := i.U === granted
  }
  io.ctrl_stall_flag := !io.bus_granted(0.U)
}
