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

  val fault_pending = RegInit(false.B)

  // ----------------------------------------
  // Basic decode
  // ----------------------------------------
  val satp_mode = io.satp(31)        // 1 = Sv32
  val satp_ppn  = io.satp(21, 0)     // root page table PPN
  val sv32_on   = io.enable && (satp_mode === 1.U)

  // ----------------------------------------
  // I-tlb 2 way asscoiate 8 set
  // ----------------------------------------
  val nSets = 8
  val nWays = 2


  val itlb_valid  = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))
  val itlb_tag    = Reg(Vec(nSets, Vec(nWays, UInt(17.W))))
  val itlb_ppn    = Reg(Vec(nSets, Vec(nWays, UInt(20.W))))   // PA[31:12] for now
  val itlb_victim = RegInit(VecInit(Seq.fill(nSets)(0.U(1.W)))) // RR victim per set
  val itlb_isSuper = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B))))) 


  // ===== decode VA =====
  val i_vpn1 = io.i_va(31,22)
  val i_vpn0 = io.i_va(21,12)
  val i_off  = io.i_va(11,0)

  // 4KB key
  val i_set4k = i_vpn0(2,0)          // VA[14:12]
  val i_tag4k = io.i_va(31,15)       // 17b

  // superpage(4MB) key
  val i_setSp = i_vpn1(2,0)                          // VA[24:22]
  val i_tagSp = Cat(0.U(10.W), i_vpn1(9,3))          // 17b (only 7 bits meaningful)

  // ===== per-way hit =====
  // 4KB: must NOT be super entry
  val i4k_hit_w0 = itlb_valid(i_set4k)(0) && !itlb_isSuper(i_set4k)(0) && (itlb_tag(i_set4k)(0) === i_tag4k)
  val i4k_hit_w1 = itlb_valid(i_set4k)(1) && !itlb_isSuper(i_set4k)(1) && (itlb_tag(i_set4k)(1) === i_tag4k)

  // superpage: must be super entry
  val isp_hit_w0 = itlb_valid(i_setSp)(0) &&  itlb_isSuper(i_setSp)(0) && (itlb_tag(i_setSp)(0) === i_tagSp)
  val isp_hit_w1 = itlb_valid(i_setSp)(1) &&  itlb_isSuper(i_setSp)(1) && (itlb_tag(i_setSp)(1) === i_tagSp)

  val i4k_hit = i4k_hit_w0 || i4k_hit_w1
  val isp_hit = isp_hit_w0 || isp_hit_w1

  val itlb_hit = i4k_hit || isp_hit

  // ===== select ppn (4KB priority) =====
  val i4k_ppn_sel = Mux(i4k_hit_w0, itlb_ppn(i_set4k)(0), itlb_ppn(i_set4k)(1))
  val isp_ppn_sel = Mux(isp_hit_w0, itlb_ppn(i_setSp)(0), itlb_ppn(i_setSp)(1))

  val itlb_ppn_sel = Mux(i4k_hit, i4k_ppn_sel, isp_ppn_sel)
  val itlb_pa      = Cat(itlb_ppn_sel, i_off)

  // ----------------------------------------
  // D-tlb 2 way associate 8 set
  // ----------------------------------------
  val dtlb_valid  = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))
  val dtlb_tag    = Reg(Vec(nSets, Vec(nWays, UInt(17.W))))
  val dtlb_ppn    = Reg(Vec(nSets, Vec(nWays, UInt(20.W))))   // PA[31:12] for now
  val dtlb_victim = RegInit(VecInit(Seq.fill(nSets)(0.U(1.W)))) // RR victim per set
  val dtlb_isSuper = RegInit(VecInit(Seq.fill(nSets)(VecInit(Seq.fill(nWays)(false.B)))))



  val d_vpn1 = io.d_va(31,22)
  val d_vpn0 = io.d_va(21,12)
  val d_off  = io.d_va(11,0)

  val d_set4k = d_vpn0(2,0)              // VA[14:12]
  val d_tag4k = io.d_va(31,15)           // 17b = vpn1(9,0) ## vpn0(9,3)

  // superpage(4MB) key
  val d_setSp = d_vpn1(2,0)              // VA[24:22]
  val d_tagSp = Cat(0.U(10.W), d_vpn1(9,3))  // 17b，

  // ===== per-way hit =====
  // 4KB hit: !isSuper
  val d4k_hit_w0 = dtlb_valid(d_set4k)(0) && !dtlb_isSuper(d_set4k)(0) && (dtlb_tag(d_set4k)(0) === d_tag4k)
  val d4k_hit_w1 = dtlb_valid(d_set4k)(1) && !dtlb_isSuper(d_set4k)(1) && (dtlb_tag(d_set4k)(1) === d_tag4k)

  // super hit: isSuper
  val dsp_hit_w0 = dtlb_valid(d_setSp)(0) &&  dtlb_isSuper(d_setSp)(0) && (dtlb_tag(d_setSp)(0) === d_tagSp)
  val dsp_hit_w1 = dtlb_valid(d_setSp)(1) &&  dtlb_isSuper(d_setSp)(1) && (dtlb_tag(d_setSp)(1) === d_tagSp)

  val d4k_hit = d4k_hit_w0 || d4k_hit_w1
  val dsp_hit = dsp_hit_w0 || dsp_hit_w1

  // ===== choose result =====
  // prioritzie 4KB
  val dtlb_hit = d4k_hit || dsp_hit

  val d4k_ppn_sel = Mux(d4k_hit_w0, dtlb_ppn(d_set4k)(0), dtlb_ppn(d_set4k)(1))
  val dsp_ppn_sel = Mux(dsp_hit_w0, dtlb_ppn(d_setSp)(0), dtlb_ppn(d_setSp)(1))

  val dtlb_ppn_sel = Mux(d4k_hit, d4k_ppn_sel, dsp_ppn_sel)
  val dtlb_pa      = Cat(dtlb_ppn_sel, d_off)

  // ----------------------------------------
  // PTW FSM 
  // ----------------------------------------
  val sIdle :: sL1Req :: sL1Wait :: sL0Req :: sL0Wait :: sLeaf :: sFault :: Nil = Enum(7)
  val state = RegInit(sIdle)

  // PTE helpers 
  def pteV(p: UInt) = p(0) 
  def pteR(p: UInt) = p(1) 
  def pteW(p: UInt) = p(2)
  def pteX(p: UInt) = p(3)
  def pteA(p: UInt) = p(6)
  def pteD(p: UInt) = p(7)
  def ptePPN(p: UInt) = p(29, 10) // 20 bits for Sv32 PPN

  // latch the VA that caused the miss
  val latched_va = Reg(UInt(Parameters.AddrWidth))
  val pte1       = Reg(UInt(32.W))
  val pte0 = Reg(UInt(32.W))

  val vpn1 = latched_va(31, 22)
  val root_base = satp_ppn << 12
  val pte1_addr = root_base + (vpn1 << 2)

  val vpn0 = latched_va(21, 12)
  // L0 base = pte1.ppn * PAGESIZE
  val l0_base = ptePPN(pte1) << 12
  val pte0_addr = l0_base + (vpn0 << 2)

  val latched_isLoad  = RegInit(false.B)
	val latched_isStore = RegInit(false.B)

  val ptw_addr_r  = RegInit(0.U(Parameters.AddrWidth))
  val ptw_valid_r = RegInit(false.B)

  


  def superpagePPN20(p: UInt, va: UInt): UInt = {
    val ppn1_for_pa = p(29, 20)     // 10 bits -> PA[31:22]
    val vpn0        = va(21, 12)    // 10 bits -> PA[21:12]
    Cat(ppn1_for_pa, vpn0)          // 20 bits -> PA[31:12]
  }

  // Leaf heuristic (v0): V && (R || X)
  def isLeaf(p: UInt) = pteV(p) && (pteR(p) || pteX(p))

  val leaf_pte   = Reg(UInt(32.W))
  val leaf_level = Reg(UInt(1.W))  // 1: L1 leaf(superpage), 0: L0 leaf(4KB)

  // ----------------------------------------
  // Defaults
  // ----------------------------------------
  io.ptw_req_valid := false.B
  io.ptw_req_addr  := 0.U
  io.ptw_active    := (state =/= sIdle)
  io.ptw_stall := false.B

  io.i_pa    := 0.U
  io.i_fault := false.B

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

        when (choose_d) {
          latched_isLoad  := io.d_isLoad
          latched_isStore := io.d_isStore
        } .otherwise {
          latched_isLoad  := false.B
          latched_isStore := false.B
        }

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
        val invalid_pte = !pteV(p) || (!pteR(p) && pteW(p))
        val superpage_misaligned = (p(19,10) =/= 0.U)   // ppn0 != 0
        pte1 := p
        when(invalid_pte) {
          state := sFault // not valid
        }.elsewhen(isLeaf(p)) {
          when(superpage_misaligned) {
            state := sFault
          }.otherwise {
            leaf_pte   := p
            leaf_level := 1.U
            state      := sLeaf
          }          
        }.otherwise {
          // valid but non-leaf
          state := sL0Req
        }
        io.ptw_req_valid := false.B
      }
    }
    is(sL0Req) {
      io.ptw_req_valid := true.B
      io.ptw_req_addr  := pte0_addr
      state := sL0Wait
    }
    is(sL0Wait) {
      io.ptw_req_valid := true.B
      io.ptw_req_addr  := pte0_addr

      when(io.ptw_resp_valid) {
        val p = io.ptw_resp_data(31, 0)
        pte0 := p

        val invalid_pte = !pteV(p) || (!pteR(p) && pteW(p))

        when(invalid_pte) {
          state := sFault
        }.elsewhen(isLeaf(p)) {
          // L0 leaf: normal 4KB
          leaf_pte   := p
          leaf_level := 0.U
          state      := sLeaf
        }.otherwise {
          // L0 non-leaf => page fault
          state := sFault
        }
        io.ptw_req_valid := false.B
      }
    }

    is(sLeaf){
      val isFetch = !latched_isD
      val isLoad  = latched_isD && latched_isLoad
      val isStore = latched_isD && latched_isStore

      val perm_ok = WireDefault(false.B)

      when (isFetch) {
        perm_ok := pteX(leaf_pte)
      }.elsewhen (isLoad) {
        perm_ok := pteR(leaf_pte)
      }.elsewhen (isStore) {
        perm_ok := pteW(leaf_pte)
      }.otherwise {
        perm_ok := false.B // unknown access type
      }
      when (!perm_ok) {
        state := sFault
      }.otherwise {
        // A/D handling (future)
        // TLB fill
        val isSuper_fill = (leaf_level === 1.U)

        val vpn1_fill = latched_va(31,22)  // 10b
        val vpn0_fill = latched_va(21,12)  // 10b

        
        val set_fill = Mux(isSuper_fill,
          vpn1_fill(2,0),   // super: VA[24:22]
          vpn0_fill(2,0)    // 4KB:   VA[14:12]
        )
        
        val tag_fill = Mux(isSuper_fill,
          Cat(0.U(10.W), vpn1_fill(9,3)),  // super: tag in vpn1
          latched_va(31,15)                // 4KB: tag17 = VA[31:15]
        )

        val final_ppn = Mux(isSuper_fill,
          superpagePPN20(leaf_pte, latched_va),
          ptePPN(leaf_pte)
        )

        when (latched_isD) {
          // ---- DTLB fill ----
          val d_w0_inv = !dtlb_valid(set_fill)(0)
          val d_w1_inv = !dtlb_valid(set_fill)(1)
          val d_way = Mux(d_w0_inv, 0.U, Mux(d_w1_inv, 1.U, dtlb_victim(set_fill)))

          dtlb_valid(set_fill)(d_way) := true.B
          dtlb_tag  (set_fill)(d_way) := tag_fill
          dtlb_ppn  (set_fill)(d_way) := final_ppn
          dtlb_isSuper(set_fill)(d_way) := isSuper_fill

          // round-robin toggle (2-way)
          dtlb_victim(set_fill) := d_way ^ 1.U
        }.otherwise {
          // ---- ITLB fill ----
          val i_w0_inv = !itlb_valid(set_fill)(0)
          val i_w1_inv = !itlb_valid(set_fill)(1)
          val i_way = Mux(i_w0_inv, 0.U, Mux(i_w1_inv, 1.U, itlb_victim(set_fill)))

          itlb_valid(set_fill)(i_way) := true.B
          itlb_tag  (set_fill)(i_way) := tag_fill
          itlb_ppn  (set_fill)(i_way) := final_ppn
          itlb_isSuper(set_fill)(i_way) := isSuper_fill

          itlb_victim(set_fill) := i_way ^ 1.U
        }
        state := sIdle
      }
    }

    is(sFault) {
      io.ptw_stall := false.B
      io.i_fault := !latched_isD
      io.d_fault :=  latched_isD
      state := sIdle
    }
  }
}