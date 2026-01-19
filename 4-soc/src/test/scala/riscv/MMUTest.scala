// SPDX-License-Identifier: MIT
package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MMUSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MMU"

  // Helper: build a 32-bit PTE in Sv32 format (only fields you currently use)
  // Bits:
  //  [0] V, [1] R, [2] W, [3] X, [4] U (unused by your MMU now but kept),
  //  [29:10] PPN (20 bits)
  def makePTE(
      ppn: Int,
      v: Boolean = true,
      r: Boolean = false,
      w: Boolean = false,
      x: Boolean = false,
      u: Boolean = false
  ): Int = {
    var pte = 0
    if (v) pte |= (1 << 0)
    if (r) pte |= (1 << 1)
    if (w) pte |= (1 << 2)
    if (x) pte |= (1 << 3)
    if (u) pte |= (1 << 4)
    pte |= ((ppn & 0xFFFFF) << 10)
    pte
  }

  // Helper: SATP for Sv32: mode at bit31, ppn at [21:0]
  def makeSATP(mode: Int, ppn: Int): Long = {
    (mode.toLong << 31) | (ppn.toLong & 0x3FFFFF)
  }

  // Strongly recommended: tie off D-side so it won't affect stall logic
  def tieOffD(dut: MMU): Unit = {
    dut.io.d_valid.poke(false.B)
    dut.io.d_va.poke(0.U)
    dut.io.d_isLoad.poke(false.B)
    dut.io.d_isStore.poke(false.B)
  }

  it should "passthrough addresses when MMU is disabled" in {
    test(new MMU) { dut =>
      tieOffD(dut)

      dut.io.enable.poke(false.B)
      dut.io.satp.poke(0.U)

      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(0x12345678L.U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_resp_data.poke(0.U)

      dut.clock.step(1)

      dut.io.i_pa.expect(0x12345678L.U)
      dut.io.ptw_stall.expect(false.B)
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "passthrough when satp.mode = 0 even if enable = true" in {
    test(new MMU) { dut =>
      tieOffD(dut)

      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(0, 0x5).U) // bare

      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(0x12345678L.U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_resp_data.poke(0.U)

      dut.clock.step(1)

      dut.io.i_pa.expect(0x12345678L.U)
      dut.io.ptw_stall.expect(false.B)
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "do a 2-level PTW on I-side miss (L1 pointer -> L0 leaf) and then hit after refill" in {
    test(new MMU) { dut =>
      tieOffD(dut)

      // Match your asm/waveform style: root at 0x5000 (ppn=0x5), L0 at 0x6000 (ppn=0x6)
      val rootPPN = 0x5
      val l0PPN   = 0x6

      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, rootPPN).U)

      // PC/VA = 0x000012B4 => vpn1=0, vpn0=1, off=0x2B4
      val va = 0x000012B4L

      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va.U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_resp_data.poke(0.U)

      // After enabling paging, first access should stall and start PTW
      dut.clock.step(1)
      dut.io.ptw_stall.expect(true.B)
      dut.io.ptw_active.expect(true.B)

      // L1Req cycle: should request pte1_addr = (rootPPN<<12) + vpn1*4 = 0x5000
      dut.io.ptw_req_valid.expect(true.B)
      dut.io.ptw_req_addr.expect(0x5000.U)

      // Move to L1Wait
      dut.clock.step(1)
      dut.io.ptw_req_valid.expect(true.B)
      dut.io.ptw_req_addr.expect(0x5000.U)

      // Provide L1 pointer PTE: PPN=l0PPN, V=1, R/W/X=0
      val l1Pointer = makePTE(ppn = l0PPN, v = true, r = false, w = false, x = false)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(l1Pointer.U)

      // Consume L1 response -> should go to L0Req next
      dut.clock.step(1)
      dut.io.ptw_resp_valid.poke(false.B)

      // L0Req: pte0_addr = (l0PPN<<12) + vpn0*4 = 0x6000 + 1*4 = 0x6004
      dut.io.ptw_req_valid.expect(true.B)
      dut.io.ptw_req_addr.expect(0x6004.U)

      // Move to L0Wait
      dut.clock.step(1)
      dut.io.ptw_req_valid.expect(true.B)
      dut.io.ptw_req_addr.expect(0x6004.U)

      // Provide L0 leaf PTE for identity mapping: PPN=1, X=1 (fetch), V=1
      val l0Leaf = makePTE(ppn = 0x1, v = true, x = true)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(l0Leaf.U)

      // Consume L0 response -> will go to sLeaf then fill TLB and eventually return idle
      dut.clock.step(1)
      dut.io.ptw_resp_valid.poke(false.B)

      // Give it a couple cycles to settle back to idle
      dut.clock.step(2)

      dut.io.ptw_active.expect(false.B)
      dut.io.ptw_stall.expect(false.B)

      // Re-issue same VA (or same page different offset) => should hit, no PTW
      val va2 = va + 0x100
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va2.U)

      dut.clock.step(1)

      dut.io.ptw_stall.expect(false.B)
      dut.io.ptw_req_valid.expect(false.B)

      // expected PA = (PPN<<12) | off = (1<<12) | (0x2B4+0x100) = 0x000013B4
      dut.io.i_pa.expect(0x000013B4L.U)
      dut.io.i_fault.expect(false.B)
    }
  }

  it should "do a 1-level PTW on superpage (L1 leaf) and then hit after refill" in {
    test(new MMU) { dut =>
      tieOffD(dut)

      val rootPPN = 0x5
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, rootPPN).U)

      // VA = 0x00401000 => vpn1=1, vpn0=1, off=0
      val va = 0x00401000L

      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va.U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_resp_data.poke(0.U)

      dut.clock.step(1)
      dut.io.ptw_stall.expect(true.B)
      dut.io.ptw_active.expect(true.B)

      // For vpn1=1: pte1_addr = 0x5000 + 1*4 = 0x5004
      dut.io.ptw_req_valid.expect(true.B)
      dut.io.ptw_req_addr.expect(0x5004.U)

      dut.clock.step(1) // L1Wait
      dut.io.ptw_req_valid.expect(true.B)
      dut.io.ptw_req_addr.expect(0x5004.U)

      // Superpage leaf requires PPN0=0 => choose PPN = 0x400 (PPN1=1, PPN0=0)
      val l1SuperLeaf = makePTE(ppn = 0x400, v = true, x = true)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(l1SuperLeaf.U)

      dut.clock.step(1)
      dut.io.ptw_resp_valid.poke(false.B)

      dut.clock.step(2)
      dut.io.ptw_active.expect(false.B)
      dut.io.ptw_stall.expect(false.B)

      // Re-issue => should hit and return PA=0x00401000 (identity in superpage region)
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va.U)
      dut.clock.step(1)

      dut.io.ptw_req_valid.expect(false.B)
      dut.io.ptw_stall.expect(false.B)
      dut.io.i_pa.expect(0x00401000L.U)
      dut.io.i_fault.expect(false.B)
    }
  }

  it should "fault on invalid L1 PTE (V=0)" in {
    test(new MMU) { dut =>
      tieOffD(dut)

      val rootPPN = 0x5
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, rootPPN).U)

      val va = 0x000012B4L // vpn1=0 => L1 addr 0x5000
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va.U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_resp_data.poke(0.U)

      dut.clock.step(2) // reach L1Wait
      dut.io.ptw_req_addr.expect(0x5000.U)

      val invalid = makePTE(ppn = 0x0, v = false)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(invalid.U)

      dut.clock.step(1) // state becomes sFault
      dut.io.ptw_resp_valid.poke(false.B)

      // In sFault cycle, i_fault is asserted for 1 cycle
      dut.io.i_fault.expect(true.B)

      dut.clock.step(1) // back to idle
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "fault on non-executable leaf for instruction fetch" in {
    test(new MMU) { dut =>
      tieOffD(dut)

      val rootPPN = 0x5
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, rootPPN).U)

      val va = 0x00401000L // vpn1=1 => L1 addr 0x5004
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va.U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_resp_data.poke(0.U)

      dut.clock.step(2) // reach L1Wait
      dut.io.ptw_req_addr.expect(0x5004.U)

      // Leaf but X=0 (R=1 makes it "leaf" under your heuristic), should fail perm check in sLeaf
      val nonExecLeaf = makePTE(ppn = 0x400, v = true, r = true, x = false)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(nonExecLeaf.U)

      dut.clock.step(1) // move into sLeaf
      dut.io.ptw_resp_valid.poke(false.B)

      dut.clock.step(1) // sLeaf decides perm_ok=false -> sFault next
      dut.io.i_fault.expect(true.B)

      dut.clock.step(1)
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_active.expect(false.B)
    }
  }
}