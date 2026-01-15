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
    val i_fault = Output(Bool())

    /* ========= D-side (v0 tie-off) ========= */
    val d_va      = Input(UInt(Parameters.AddrWidth))
    val d_valid   = Input(Bool())
    val d_isLoad  = Input(Bool())
    val d_isStore = Input(Bool())

    val d_pa    = Output(UInt(Parameters.AddrWidth))
    val d_fault = Output(Bool())

    /* ========= Control ========= */
    val enable = Input(Bool()) // satp.mode != 0
    val satp   = Input(UInt(Parameters.DataWidth))
    val ptw_stall = Output(Bool())

    /* ========= PTW memory port (MMU -> Bus -> MMU) ========= */
    val ptw_req_valid  = Output(Bool())
    val ptw_req_addr   = Output(UInt(Parameters.AddrWidth)) // PA
    val ptw_resp_valid = Input(Bool())
    val ptw_resp_data  = Input(UInt(Parameters.DataWidth))  // contains PTE in low 32 bits
    val ptw_active     = Output(Bool())
  })

  // ----------------------------------------
  // Basic decode
  // ----------------------------------------
  val satp_mode = io.satp(31)        // 1 = Sv32
  val satp_ppn  = io.satp(21, 0)     // root page table PPN
  val sv32_on   = io.enable && (satp_mode === 1.U)

  // ----------------------------------------
  // 1-entry I-TLB (v0)
  // ----------------------------------------
  val itlb_valid = RegInit(false.B)
  val itlb_vpn   = Reg(UInt(20.W))   // VA[31:12]
  val itlb_ppn   = Reg(UInt(20.W))   // PPN[19:0]
  val itlb_tag   = Reg(UInt(30.W))   // VA[31:2]  (每條指令不同)

  val cur_tag = io.i_va(31, 2)

  val cur_vpn = io.i_va(31, 12)
  val cur_off = io.i_va(11, 0)

  val itlb_hit = itlb_valid && (cur_tag === itlb_tag)
  val itlb_pa  = Cat(itlb_ppn, cur_off)

  // D-side tag (故意做得很細：幾乎每次都不同)
  val d_tag = io.d_va
  val d_off = io.d_va(11, 0)

  // 1-entry D-TLB (tag -> ppn)
  val dtlb_valid = RegInit(false.B)
  val dtlb_tag   = Reg(UInt(30.W))
  val dtlb_ppn   = Reg(UInt(20.W))   // PA[31:12]

  val dtlb_hit = dtlb_valid && (d_tag === dtlb_tag)
  val dtlb_pa  = Cat(dtlb_ppn, d_off)

  // ----------------------------------------
  // PTW FSM (v0: single-level PTE fetch only)
  // ----------------------------------------
  val sIdle :: sL1Req :: sL1Wait :: sFault :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // latch the VA that caused the miss
  val latched_va = Reg(UInt(Parameters.AddrWidth))
  val pte1       = Reg(UInt(32.W))

  val vpn1 = latched_va(31, 22)
  val root_base = satp_ppn << 12
  val pte1_addr = root_base + (vpn1 << 2)

  // PTE helpers (v0 minimal)
  def pteV(p: UInt) = p(0)
  def pteR(p: UInt) = p(1)
  def pteW(p: UInt) = p(2)
  def pteX(p: UInt) = p(3)
  def ptePPN(p: UInt) = p(31, 10) // 20 bits for Sv32 PPN

  def superpagePPN20(p: UInt, va: UInt): UInt = {
    val ppn1_for_pa = p(29, 20)     // 10 bits -> PA[31:22]
    val vpn0        = va(21, 12)    // 10 bits -> PA[21:12]
    Cat(ppn1_for_pa, vpn0)          // 20 bits -> PA[31:12]
  }

  // Leaf heuristic (v0): V && (R || X)
  // NOTE: v0 單層版本「只把 L1 當 leaf 來用」（像 superpage / demo 用）
  def isLeaf(p: UInt) = pteV(p) && (pteR(p) || pteX(p))

  // ----------------------------------------
  // Defaults
  // ----------------------------------------
  io.ptw_req_valid := false.B
  io.ptw_req_addr  := 0.U
  io.ptw_active    := (state =/= sIdle)

  io.i_pa    := 0.U
  io.ptw_stall := false.B
  io.i_fault := false.B

  // D-side tie-off (v0)
  io.d_pa    := 0.U
  io.d_fault := false.B

  // ----------------------------------------
  // Start condition: Sv32 enabled + valid fetch + idle + TLB miss
  // ----------------------------------------
  val i_miss = sv32_on && io.i_valid && !itlb_hit
  val d_miss = sv32_on && io.d_valid && !dtlb_hit

  // 同時 miss 時：i 優先
  val choose_i = i_miss
  val choose_d = !i_miss && d_miss



  val start_walk =  (state === sIdle) && (choose_i || choose_d)
  val latched_isD = RegInit(false.B)

  // Stall policy:
  // - Sv32 off: never stall
  // - PTW busy: stall
  // - idle but miss on valid fetch: stall (because we'll start PTW)
  
  when(!sv32_on) {
    io.i_pa := io.i_va
    io.d_pa := io.d_va
    io.ptw_stall := false.B

  }.elsewhen(itlb_hit && (!io.d_valid || dtlb_hit)) {
    io.i_pa := itlb_pa
    io.d_pa := dtlb_pa
    io.ptw_stall := false.B

  }.otherwise {
    // ★ TLB miss / PTW active：PA 強制 0
    io.i_pa := 0.U
    io.d_pa := 0.U
    io.ptw_stall := true.B
  }

  // ----------------------------------------
  // FSM
  // ----------------------------------------
  switch(state) {
    is(sIdle) {
      when(start_walk) {
        latched_va  := Mux(choose_d, io.d_va, io.i_va)
        latched_isD := choose_d  // true: D-side, false: I-side
        state := sL1Req
      }
    }

    is(sL1Req) {
      io.ptw_req_valid := true.B
      io.ptw_req_addr  := pte1_addr
      state := sL1Wait
    }

    is(sL1Wait) {
      io.ptw_req_valid := true.B
      io.ptw_req_addr  := pte1_addr

      when(io.ptw_resp_valid) {
        val p = io.ptw_resp_data(31, 0)
        pte1 := p

        when(!pteV(p)) {
          state := sFault
        }.elsewhen(!pteX(p)) {  
          // No execute permission for instruction fetch
          state := sFault
        }.elsewhen(isLeaf(p)) {
          // v0: treat L1 leaf as translation result and fill TLB
          when(latched_isD) {
            dtlb_valid := true.B
            dtlb_tag   := latched_va   
            dtlb_ppn   := latched_va(31, 12)   // 故意 VA=PA
          }.otherwise {
            itlb_valid := true.B
            itlb_tag   := latched_va(31, 2)
            itlb_ppn   := latched_va(31, 12)   // 故意 VA=PA
          }
          state := sIdle
        }.otherwise {
          // v0: no L0 support yet
          state := sFault
        }
      }
    }

    is(sFault) {
      io.i_fault := true.B
      state := sIdle
    }
  }
}