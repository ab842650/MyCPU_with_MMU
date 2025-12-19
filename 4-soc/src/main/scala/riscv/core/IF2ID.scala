// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv.core

import chisel3._
import riscv.core.PipelineRegister
import riscv.Parameters

class IF2ID extends Module {
  val io = IO(new Bundle {
    val stall                = Input(Bool())
    val flush                = Input(Bool())
    val instruction          = Input(UInt(Parameters.InstructionWidth))
    val instruction_address  = Input(UInt(Parameters.AddrWidth))
    val interrupt_flag       = Input(UInt(Parameters.InterruptFlagWidth))
    val btb_predicted_taken  = Input(Bool())                     // BTB prediction from IF stage
    val btb_predicted_target = Input(UInt(Parameters.AddrWidth)) // BTB predicted target

    val output_instruction          = Output(UInt(Parameters.DataWidth))
    val output_instruction_address  = Output(UInt(Parameters.AddrWidth))
    val output_interrupt_flag       = Output(UInt(Parameters.InterruptFlagWidth))
    val output_btb_predicted_taken  = Output(Bool())                     // BTB prediction to ID stage
    val output_btb_predicted_target = Output(UInt(Parameters.AddrWidth)) // BTB target to ID stage
  })

  val instruction = Module(new PipelineRegister(defaultValue = InstructionsNop.nop))
  instruction.io.in     := io.instruction
  instruction.io.stall  := io.stall
  instruction.io.flush  := io.flush
  io.output_instruction := instruction.io.out

  val instruction_address = Module(new PipelineRegister(defaultValue = ProgramCounter.EntryAddress))
  instruction_address.io.in     := io.instruction_address
  instruction_address.io.stall  := io.stall
  instruction_address.io.flush  := io.flush
  io.output_instruction_address := instruction_address.io.out

  val interrupt_flag = Module(new PipelineRegister(Parameters.InterruptFlagBits))
  interrupt_flag.io.in     := io.interrupt_flag
  interrupt_flag.io.stall  := io.stall
  interrupt_flag.io.flush  := io.flush
  io.output_interrupt_flag := interrupt_flag.io.out

  // BTB prediction passed through pipeline
  val btb_predicted_taken = Module(new PipelineRegister(1))
  btb_predicted_taken.io.in     := io.btb_predicted_taken
  btb_predicted_taken.io.stall  := io.stall
  btb_predicted_taken.io.flush  := io.flush
  io.output_btb_predicted_taken := btb_predicted_taken.io.out.asBool

  val btb_predicted_target = Module(new PipelineRegister(Parameters.AddrBits))
  btb_predicted_target.io.in     := io.btb_predicted_target
  btb_predicted_target.io.stall  := io.stall
  btb_predicted_target.io.flush  := io.flush
  io.output_btb_predicted_target := btb_predicted_target.io.out
}
