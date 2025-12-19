package riscv.core

import chisel3._
import riscv.Parameters

class MMU extends Module {
  val io = IO(new Bundle {
    val va = Input(UInt(Parameters.AddrWidth))
    val pa = Output(UInt(Parameters.AddrWidth))
  })

  io.pa := io.va + "h00000100".U
}