// SPDX-License-Identifier: MIT
package riscv.core

import chisel3._
import chisel3.util._
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

    // default


    // =============================
    // satp decode (Sv32-ready)
    // =============================
    val satp_mode = io.satp(31)        // 1 = Sv32
    val satp_asid = io.satp(30, 22)    // ASID (unused for now)
    val satp_ppn  = io.satp(21, 0)     // Root page table PPN



    //PTW state
    val sIdle :: sL1Req :: sL1Wait :: sDone :: Nil = Enum(4)
    val state = RegInit(sIdle)

    val latched_va  = Reg(UInt(Parameters.AddrWidth))
    val pte1        = Reg(UInt(32.W))
    

    // VA split
    val vpn1 = latched_va(31, 22)
    val vpn0 = latched_va(21, 12)
    val off  = latched_va(11, 0)

    // defaults
    io.i_pa    := io.i_va
    io.d_pa    := io.d_va
    io.i_stall := false.B
    io.d_stall := false.B
    io.i_fault := false.B
    io.d_fault := false.B

    // io.ptw_req_valid := false.B
    // io.ptw_req_addr  := 0.U

    // L1 PTE addr = root_base + vpn1*4

    val root_base = satp_ppn << 12
    val pte1_addr = root_base + (vpn1 << 2)

    //FSM
    switch(state) {
    is(sIdle) {
      when(io.enable && satp_mode === 1.U && io.i_valid) {
        latched_va := io.i_va
        state := sL1Req
      }
    }

    is(sL1Req) {
      io.i_stall := true.B

      state := sL1Wait
    }

    is(sL1Wait) {
      io.i_stall := true.B

    }

    is(sDone) {
      // 先不翻譯、不 fault，讓它先放行
      io.i_stall := false.B
      state := sIdle
    }
  }

}