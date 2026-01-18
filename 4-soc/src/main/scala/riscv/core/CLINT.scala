// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util.MuxLookup
import riscv.Parameters

object InterruptStatus {
  val None   = 0x0.U(8.W)
  val Timer0 = 0x1.U(8.W)
  val Ret    = 0xff.U(8.W)
}

// Core Local Interrupt Controller
// CSRDirectAccessBundle is defined in CSR.scala
class CLINT extends Module {
  val io = IO(new Bundle {
    val interrupt_flag = Input(UInt(Parameters.InterruptFlagWidth))

    val instruction_id         = Input(UInt(Parameters.InstructionWidth))
    val instruction_address_if = Input(UInt(Parameters.AddrWidth))

    val jump_flag    = Input(Bool())
    val jump_address = Input(UInt(Parameters.AddrWidth))

    val id_interrupt_handler_address = Output(UInt(Parameters.AddrWidth))
    val id_interrupt_assert          = Output(Bool())

    val priv_mode = Input(UInt(2.W)) //current mode



    val csr_bundle = new CSRDirectAccessBundle
  })
  val interrupt_enable_global   = io.csr_bundle.mstatus(3) // MIE bit (global enable)
  val interrupt_enable_timer    = io.csr_bundle.mie(7)     // MTIE bit (timer enable)
  val interrupt_enable_external = io.csr_bundle.mie(11)    // MEIE bit (external enable)


      //page fault
  val is_if_page_fault = false.B
  val if_pf_cause     = 12.U  // Instruction page fault

  //val cur_priv = Mux(io.priv_mode === 0.U, PrivMode.M, io.priv_mode) // for testing

  val cur_priv = io.priv_mode

  val instruction_address = Mux(
    io.jump_flag,
    io.jump_address,
    io.instruction_address_if,
  )

  // Encode previous privilege into MPP (00=U, 01=S, 11=M)
  // only implement S/M, so map: S->01, M->11
  val prev_mpp = Mux(cur_priv === PrivMode.S, "b01".U(2.W), "b11".U(2.W))

  // Trap entry to M-mode:
  // MPP  <= prev privilege
  // MPIE <= old MIE
  // MIE  <= 0
  val mstatus_disable_interrupt =
    io.csr_bundle.mstatus(31, 13) ##      // keep [31:13]
    prev_mpp ##                           // MPP[12:11] <- prev mode
    io.csr_bundle.mstatus(10, 8) ##       // keep [10:8]
    io.csr_bundle.mstatus(3) ##           // MPIE (bit 7) <- old MIE (bit 3)
    io.csr_bundle.mstatus(6, 4) ##        // keep [6:4]
    0.U(1.W) ##                           // MIE  (bit 3) <- 0
    io.csr_bundle.mstatus(2, 0)           // keep [2:0]
  

  // mret: MIE <- MPIE, MPIE <- 1, and clear MPP (bits 12:11) to 00
  val mstatus_recover_interrupt =
    io.csr_bundle.mstatus(31, 13) ##           // keep [31:13]
    0.U(2.W) ##                                // MPP[12:11] <- 00
    io.csr_bundle.mstatus(10, 8) ##            // keep [10:8]
    1.U(1.W) ##                                // MPIE (bit 7) <- 1
    io.csr_bundle.mstatus(6, 4) ##             // keep [6:4]
    io.csr_bundle.mstatus(7) ##                // MIE (bit 3) <- old MPIE (bit 7)
    io.csr_bundle.mstatus(2, 0)                // keep [2:0]

    // mpp for mret
  val mpp = io.csr_bundle.mstatus(12,11)
  // next priv for mpp
  val nextPriv = Mux(mpp === "b01".U, PrivMode.S, PrivMode.M)

  // Check individual interrupt source enable based on interrupt type
  val interrupt_source_enabled = Mux(
    io.interrupt_flag(0), // Timer interrupt (bit 0)
    interrupt_enable_timer,
    interrupt_enable_external
  )

  // Trap entry to S-mode:
  // SPIE <= SIE, SIE <= 0, SPP <= 1 (prev = S; no U-mode implemented)
  val sstatus_disable_interrupt =
    io.csr_bundle.sstatus(31, 9) ##    // keep [31:9]
    1.U(1.W) ##                         // SPP  (bit 8)  <- 1 (prev = S; no U-mode implemented)
    io.csr_bundle.sstatus(7, 6) ##      // keep [7:6]
    io.csr_bundle.sstatus(1) ##         // SPIE (bit 5)  <- old SIE (bit 1) 
    io.csr_bundle.sstatus(4, 2) ##      // keep [4:2]
    0.U(1.W) ##                         // SIE  (bit 1)  <- 0
    io.csr_bundle.sstatus(0)            // keep bit 0

  // sret:
  // SIE <= SPIE, SPIE <= 1, SPP <= 0
  val sstatus_recover_interrupt =
    io.csr_bundle.sstatus(31, 9) ##    // keep [31:9]
    0.U(1.W) ##                         // SPP  (bit 8)  <- 0
    io.csr_bundle.sstatus(7, 6) ##      // keep [7:6]
    1.U(1.W) ##                         // SPIE (bit 5)  <- 1
    io.csr_bundle.sstatus(4, 2) ##      // keep [4:2]
    io.csr_bundle.sstatus(5) ##         // SIE  (bit 1)  <- old SPIE (bit 5)
    io.csr_bundle.sstatus(0)            // keep bit 0



  // 1) classify events
  val is_mret = (io.instruction_id === InstructionsRet.mret)
  val is_sret = (io.instruction_id === InstructionsRet.sret)

  val has_exc = (io.instruction_id === InstructionsEnv.ecall) || (io.instruction_id === InstructionsEnv.ebreak || is_if_page_fault)
  val excCause = Mux(
    is_if_page_fault,
    if_pf_cause,
    MuxLookup(io.instruction_id, 0.U)(
      IndexedSeq(
        InstructionsEnv.ecall  -> Mux(cur_priv === PrivMode.M, 11.U, 9.U),
        InstructionsEnv.ebreak -> 3.U
      )
    )
  )
  // interrupts (optional; keep your existing gating)
  val has_int = (io.interrupt_flag =/= InterruptStatus.None) && interrupt_enable_global && interrupt_source_enabled
  val intCause = Mux(io.interrupt_flag(0), 0x80000007L.U, 0x8000000bL.U)

  val take_trap = has_exc || has_int

  val trap_is_exc = has_exc
  val trap_cause  = Mux(trap_is_exc, excCause, intCause)

  // 2) delegation decision (for now: only exceptions via medeleg)
  val delegatedToS = (cur_priv =/= PrivMode.M) && trap_is_exc && io.csr_bundle.medeleg(trap_cause(5,0))  // low bits index ok for small causes

  val targetIsS = delegatedToS
  val targetIsM = take_trap && !targetIsS


  // 3) defaults
  io.id_interrupt_assert := false.B
  io.id_interrupt_handler_address := 0.U

  io.csr_bundle.direct_write_enable := false.B
  io.csr_bundle.direct_write_enable_s := false.B
  io.csr_bundle.priv_write_enable := false.B
  io.csr_bundle.priv_write_data := cur_priv

  io.csr_bundle.mstatus_write_data := io.csr_bundle.mstatus
  io.csr_bundle.mepc_write_data    := io.csr_bundle.mepc
  io.csr_bundle.mcause_write_data  := io.csr_bundle.mcause
  io.csr_bundle.mtval_write_data   := io.csr_bundle.mtval

  io.csr_bundle.sstatus_write_data := io.csr_bundle.sstatus
  io.csr_bundle.sepc_write_data    := io.csr_bundle.sepc
  io.csr_bundle.scause_write_data  := io.csr_bundle.scause
  io.csr_bundle.stval_write_data   := io.csr_bundle.stval

  // 4) return path
  when (is_mret) {
    io.csr_bundle.mstatus_write_data := mstatus_recover_interrupt
    io.csr_bundle.direct_write_enable := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.mepc
    io.csr_bundle.priv_write_enable := true.B
    io.csr_bundle.priv_write_data := nextPriv
  } .elsewhen (is_sret) {
    io.csr_bundle.sstatus_write_data := sstatus_recover_interrupt
    io.csr_bundle.direct_write_enable_s := true.B
    io.id_interrupt_assert := true.B
    io.id_interrupt_handler_address := io.csr_bundle.sepc
    io.csr_bundle.priv_write_enable := true.B
    io.csr_bundle.priv_write_data := PrivMode.S  
  } .elsewhen (take_trap) {
    when (targetIsM) {
      io.csr_bundle.mstatus_write_data := mstatus_disable_interrupt
      io.csr_bundle.mepc_write_data    := instruction_address
      io.csr_bundle.mcause_write_data  := trap_cause
      io.csr_bundle.mtval_write_data   := 0.U
      io.csr_bundle.direct_write_enable := true.B

      io.id_interrupt_assert := true.B
      io.id_interrupt_handler_address := io.csr_bundle.mtvec

      io.csr_bundle.priv_write_enable := true.B
      io.csr_bundle.priv_write_data := PrivMode.M
    } .otherwise { // targetIsS
      io.csr_bundle.sstatus_write_data := sstatus_disable_interrupt
      io.csr_bundle.sepc_write_data    := instruction_address
      io.csr_bundle.scause_write_data  := trap_cause
      io.csr_bundle.stval_write_data := Mux(is_if_page_fault, 0.U, 0.U)
      io.csr_bundle.direct_write_enable_s := true.B
      io.id_interrupt_assert := true.B
      io.id_interrupt_handler_address := io.csr_bundle.stvec

      io.csr_bundle.priv_write_enable := true.B
      io.csr_bundle.priv_write_data := PrivMode.S
    }
  }
}
