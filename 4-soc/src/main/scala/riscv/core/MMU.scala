package riscv.core

import chisel3._
import riscv.Parameters

class MMU extends Module {
  val io = IO(new Bundle {
    val va = Input(UInt(Parameters.AddrWidth))

    val isInst    = Input(Bool())
    val isLoad    = Input(Bool())
    val isStore   = Input(Bool())

    // Control
    val enable    = Input(Bool()) // from satp.mode != 0

    val pa = Output(UInt(Parameters.AddrWidth))
    val stall     = Output(Bool())
    val fault     = Output(Bool())
    val faultType = Output(UInt(2.W)) // inst/load/store

  })

  io.pa    := io.va + 0x100.U
  io.stall := false.B
  io.fault := false.B
  io.faultType := 0.U
}