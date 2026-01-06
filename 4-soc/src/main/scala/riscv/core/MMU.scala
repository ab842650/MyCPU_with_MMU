// SPDX-License-Identifier: MIT
package riscv.core

import chisel3._
import riscv.Parameters

class MMU extends Module {
  val io = IO(new Bundle {
    /* ========= I-side ========= */
    val i_va    = Input(UInt(Parameters.AddrWidth))
    val i_valid = Input(Bool())

    val i_pa    = Output(UInt(Parameters.AddrWidth))
    val i_stall = Output(Bool())
    val i_fault = Output(Bool())

    /* ========= D-side ========= */
    val d_va      = Input(UInt(Parameters.AddrWidth))
    val d_valid   = Input(Bool())
    val d_isLoad  = Input(Bool())
    val d_isStore = Input(Bool())

    val d_pa    = Output(UInt(Parameters.AddrWidth))
    val d_stall = Output(Bool())
    val d_fault = Output(Bool())

    /* ========= Control ========= */
    val enable = Input(Bool()) // satp.mode != 0
    val satp = Input(UInt(Parameters.DataWidth))
  })
    // =============================
    // Page Table Walker (skeleton)
    // =============================



    // =============================
    // satp decode (Sv32-ready)
    // =============================
    val satp_mode = io.satp(31)        // 1 = Sv32
    val satp_asid = io.satp(30, 22)    // ASID (unused for now)
    val satp_ppn  = io.satp(21, 0)     // Root page table PPN



    // =============================
    // Translation backend (toy for now)
    // =============================
    val i_pa_translated = Wire(UInt(Parameters.AddrWidth))
    val d_pa_translated = Wire(UInt(Parameters.AddrWidth))

    // default: pass-through
    i_pa_translated := io.i_va
    d_pa_translated := io.d_va

    //when(io.enable) {
    // toy translation (will be replaced by PTW)
    //i_pa_translated := io.i_va + 0x100.U
    //d_pa_translated := io.d_va + 0x100.U
    //}

    io.i_pa := i_pa_translated
    io.d_pa := d_pa_translated

  // For now: PTW not implemented, never stall or fault
  io.i_stall := false.B
  io.d_stall := false.B
  io.i_fault := false.B
  io.d_fault := false.B

  /* =============================
   * Debug
   * ============================= */
  when(io.enable && io.i_valid) {
    //printf(p"[I-MMU] VA=0x${Hexadecimal(io.i_va)} PA=0x${Hexadecimal(io.i_pa)}\n")
  }
  when(io.enable && io.d_valid) {
    //printf(p"[D-MMU] VA=0x${Hexadecimal(io.d_va)} PA=0x${Hexadecimal(io.d_pa)}\n")
  }
}