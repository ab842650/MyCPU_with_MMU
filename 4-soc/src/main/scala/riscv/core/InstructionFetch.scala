// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import chisel3.util.MuxCase
import riscv.Parameters

object ProgramCounter {
  val EntryAddress = Parameters.EntryAddress
}

class InstructionFetch extends Module {
  val io = IO(new Bundle {
    val stall_flag_ctrl   = Input(Bool())
    val jump_flag_id      = Input(Bool())
    val jump_address_id   = Input(UInt(Parameters.AddrWidth))
    val rom_instruction   = Input(UInt(Parameters.DataWidth))
    val instruction_valid = Input(Bool())

    // BTB misprediction correction (from ID stage)
    val btb_mispredict         = Input(Bool())                     // BTB predicted wrong
    val btb_correction_addr    = Input(UInt(Parameters.AddrWidth)) // Correct PC
    val btb_correct_prediction = Input(Bool())                     // BTB predicted correctly - skip PC redirect

    val instruction_address = Output(UInt(Parameters.AddrWidth))
    val id_instruction      = Output(UInt(Parameters.InstructionWidth))

    // BTB prediction info passed to ID stage
    val btb_predicted_taken  = Output(Bool())
    val btb_predicted_target = Output(UInt(Parameters.AddrWidth))

    // BTB update interface (from ID stage)
    val btb_update_valid  = Input(Bool())
    val btb_update_pc     = Input(UInt(Parameters.AddrWidth))
    val btb_update_target = Input(UInt(Parameters.AddrWidth))
    val btb_update_taken  = Input(Bool())
  })
  val pc = RegInit(ProgramCounter.EntryAddress)

  // Branch Target Buffer for branch prediction
  val btb = Module(new BranchTargetBuffer(entries = 16))
  btb.io.pc := pc

  // BTB prediction: use predicted target if BTB predicts taken
  val btb_next_pc = btb.io.predicted_pc
  io.btb_predicted_taken  := btb.io.predicted_taken
  io.btb_predicted_target := btb.io.predicted_pc

  // Latch jump request when stall is active
  // Problem: When mem_stall releases, PipelineRegister's combinational bypass
  // immediately changes IF2ID output, causing jump_flag_id to become false.
  // Solution: Latch jump request during stall so it's taken when stall releases.
  val pending_jump      = RegInit(false.B)
  val pending_jump_addr = RegInit(0.U(Parameters.AddrWidth))

  // Shadow registers to capture jump info BEFORE flush clears ID stage
  // This solves the race condition where:
  // - Cycle N: Jump in ID triggers flush
  // - Cycle N+1: Jump moves to EX, ID flushed, stall begins
  // Without shadow registers, pending_jump can't capture the jump because
  // by the time stall activates, jump_flag_id is already 0 (ID was flushed).
  val prev_jump_flag = RegNext(io.jump_flag_id, false.B)
  val prev_jump_addr = RegNext(io.jump_address_id, 0.U)

  // Latch jump when stall blocks it
  // Check both current cycle (normal case) and previous cycle (flush race case)
  when(io.stall_flag_ctrl && (io.jump_flag_id || prev_jump_flag)) {
    pending_jump      := true.B
    pending_jump_addr := Mux(io.jump_flag_id, io.jump_address_id, prev_jump_addr)
  }.elsewhen(!io.stall_flag_ctrl) {
    // Clear pending jump when we can take it (or when there's no jump)
    pending_jump := false.B
  }

  // Take pending jump (priority) or current jump, respecting stall
  val take_pending = pending_jump && !io.stall_flag_ctrl
  // Skip current jump if BTB already predicted correctly - IF already at correct target
  val take_current = io.jump_flag_id && !io.stall_flag_ctrl && !pending_jump && !io.btb_correct_prediction
  // BTB misprediction correction: redirect to correct PC
  val take_btb_correction = io.btb_mispredict && !io.stall_flag_ctrl

  // Next PC selection priority:
  // 1. Pending jump (deferred from stall)
  // 2. BTB misprediction correction (rollback to sequential PC)
  // 3. Actual jump from ID stage (branch taken / jump)
  // 4. BTB prediction (speculative)
  // 5. Sequential PC+4 (default)
  val next_pc = MuxCase(
    Mux(btb.io.predicted_taken, btb_next_pc, pc + 4.U), // BTB prediction or sequential
    IndexedSeq(
      take_pending                                  -> pending_jump_addr,
      take_btb_correction                           -> io.btb_correction_addr,
      take_current                                  -> io.jump_address_id,
      (io.stall_flag_ctrl || !io.instruction_valid) -> pc
    )
  )

  pc := next_pc

  io.instruction_address := pc
  io.id_instruction      := Mux(io.instruction_valid, io.rom_instruction, InstructionsNop.nop)

  // BTB update interface - connect external update signals to BTB
  btb.io.update_valid  := io.btb_update_valid
  btb.io.update_pc     := io.btb_update_pc
  btb.io.update_target := io.btb_update_target
  btb.io.update_taken  := io.btb_update_taken
}
