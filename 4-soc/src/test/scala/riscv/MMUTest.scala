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
      u: Boolean = false,
      a: Boolean = true, // MMU faults on A=0 (no hardware A/D update)
      d: Boolean = true  // ... and on stores with D=0
  ): Int = {
    var pte = 0
    if (v) pte |= (1 << 0)
    if (r) pte |= (1 << 1)
    if (w) pte |= (1 << 2)
    if (x) pte |= (1 << 3)
    if (u) pte |= (1 << 4)
    if (a) pte |= (1 << 6)
    if (d) pte |= (1 << 7)
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

  // After the faulting walk fills the ITLB: i_fault stays asserted (TLB hit, no stall,
  // no re-walk) while IF keeps presenting the faulting VA; it clears when the VA changes.
  def expectHeldIFault(dut: MMU, va: Long): Unit = {
    for (_ <- 0 until 3) {
      dut.clock.step(1)
      dut.io.i_fault.expect(true.B)
      dut.io.ptw_stall.expect(false.B)
      dut.io.ptw_active.expect(false.B)
      dut.io.ptw_req_valid.expect(false.B)
    }
    dut.io.i_va.poke((va + 0x400000).U) // different 4MB region (entry may be a superpage)
    dut.io.i_fault.expect(false.B)
    dut.clock.step(1)
    dut.io.i_fault.expect(false.B)
  }

  // Walk a 1-level superpage for the D-side and return once the TLB is filled
  def fillDSuperpage(dut: MMU, va: Long, leaf: Int, isStore: Boolean): Unit = {
    dut.io.d_valid.poke(true.B)
    dut.io.d_va.poke(va.U)
    dut.io.d_isLoad.poke((!isStore).B)
    dut.io.d_isStore.poke(isStore.B)
    dut.clock.step(2) // L1Req -> L1Wait
    dut.io.ptw_resp_valid.poke(true.B)
    dut.io.ptw_resp_data.poke(leaf.U)
    dut.clock.step(1) // -> sLeaf
    dut.io.ptw_resp_valid.poke(false.B)
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

      dut.clock.step(1) // sFault: fault entry written
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_stall.expect(true.B)

      expectHeldIFault(dut, va)
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

      dut.clock.step(1) // sLeaf: entry cached with X=0
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.ptw_stall.expect(true.B)

      expectHeldIFault(dut, va)
    }
  }

  it should "fault a store to a read-only page after the walk, without stalling" in {
    test(new MMU) { dut =>
      dut.io.i_valid.poke(false.B)
      dut.io.i_va.poke(0.U)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.ptw_resp_valid.poke(false.B)

      val va = 0x00401234L
      fillDSuperpage(dut, va, makePTE(ppn = 0x400, r = true), isStore = true)

      // sLeaf: entry cached with R=1, W=0
      dut.io.d_fault.expect(false.B)
      dut.io.ptw_stall.expect(true.B)
      dut.clock.step(1)
      // DTLB hit, W=0 -> fault, pipeline released so the trap can be taken
      dut.io.d_fault.expect(true.B)
      dut.io.ptw_stall.expect(false.B)

      // a load to the same page is fine and needs no walk (permissions are cached)
      dut.io.d_isStore.poke(false.B)
      dut.io.d_isLoad.poke(true.B)
      dut.io.d_fault.expect(false.B)
      dut.io.ptw_stall.expect(false.B)
      dut.io.d_pa.expect(va.U)
      dut.clock.step(1)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "fault on a DTLB hit when a store hits an entry filled by a load (R only)" in {
    test(new MMU) { dut =>
      dut.io.i_valid.poke(false.B)
      dut.io.i_va.poke(0.U)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.ptw_resp_valid.poke(false.B)

      val va = 0x00401234L
      fillDSuperpage(dut, va, makePTE(ppn = 0x400, r = true), isStore = false)
      dut.clock.step(1) // sLeaf fills DTLB
      dut.io.ptw_stall.expect(false.B)
      dut.io.d_fault.expect(false.B)
      dut.io.d_pa.expect(va.U)

      // Same page, now a store -> permission fault straight from the TLB, no walk
      dut.io.d_isLoad.poke(false.B)
      dut.io.d_isStore.poke(true.B)
      dut.io.d_fault.expect(true.B)
      dut.io.ptw_stall.expect(false.B)
      dut.clock.step(1)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "fault on a store to a writable page whose D bit is clear" in {
    test(new MMU) { dut =>
      dut.io.i_valid.poke(false.B)
      dut.io.i_va.poke(0.U)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.ptw_resp_valid.poke(false.B)

      fillDSuperpage(dut, 0x00400000L, makePTE(ppn = 0x400, r = true, w = true, d = false), isStore = true)
      dut.clock.step(1)
      dut.io.d_fault.expect(true.B)
    }
  }

  it should "not translate in M-mode even when satp.mode = Sv32" in {
    test(new MMU) { dut =>
      tieOffD(dut)
      dut.io.priv.poke(PrivMode.M)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(0x12345678L.U)
      dut.io.ptw_resp_valid.poke(false.B)

      dut.io.i_pa.expect(0x12345678L.U)
      dut.io.ptw_stall.expect(false.B)
      dut.clock.step(1)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "cache an invalid L0 PTE as a 4KB fault entry (no second walk) until sfence" in {
    test(new MMU) { dut =>
      dut.io.i_valid.poke(false.B)
      dut.io.i_va.poke(0.U)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.tlb_flush.poke(false.B)

      val va = 0x00007010L // vpn1=0 -> L1 pointer, vpn0=7 -> invalid
      def walkToInvalidL0(): Unit = {
        dut.io.d_valid.poke(true.B)
        dut.io.d_va.poke(va.U)
        dut.io.d_isLoad.poke(true.B)
        dut.io.d_isStore.poke(false.B)
        dut.clock.step(2) // L1Req -> L1Wait
        dut.io.ptw_req_addr.expect(0x5000.U)
        dut.io.ptw_resp_valid.poke(true.B)
        dut.io.ptw_resp_data.poke(makePTE(ppn = 0x6).U) // pointer
        dut.clock.step(1) // L0Req
        dut.io.ptw_resp_valid.poke(false.B)
        dut.io.ptw_req_addr.expect((0x6000 + 7 * 4).U)
        dut.clock.step(1) // L0Wait
        dut.io.ptw_resp_valid.poke(true.B)
        dut.io.ptw_resp_data.poke(makePTE(ppn = 0, v = false).U)
        dut.clock.step(1) // sFault (fault entry)
        dut.io.ptw_resp_valid.poke(false.B)
        dut.clock.step(1) // idle, entry valid
      }

      walkToInvalidL0()
      dut.io.d_fault.expect(true.B)
      dut.io.ptw_stall.expect(false.B)

      // access leaves, then comes back to the same page (store this time, other offset)
      dut.io.d_valid.poke(false.B)
      dut.clock.step(1)
      dut.io.d_valid.poke(true.B)
      dut.io.d_va.poke((va + 0x20).U)
      dut.io.d_isLoad.poke(false.B)
      dut.io.d_isStore.poke(true.B)
      dut.io.d_fault.expect(true.B)      // straight from the fault entry
      dut.io.ptw_stall.expect(false.B)
      dut.clock.step(1)
      dut.io.ptw_active.expect(false.B)  // no walk started

      // the fault entry is 4KB only: the neighbouring page still walks
      dut.io.d_va.poke((va + 0x1000).U)
      dut.io.d_fault.expect(false.B)
      dut.io.ptw_stall.expect(true.B)
      dut.io.d_valid.poke(false.B)
      dut.clock.step(1)
      dut.clock.step(1) // let the started walk sit; flush clears everything anyway

      // sfence.vma drops the fault entry -> next access walks again
      dut.io.tlb_flush.poke(true.B)
      dut.clock.step(1)
      dut.io.tlb_flush.poke(false.B)
    }
  }

  it should "cache an invalid L1 PTE as a 4MB fault entry for the I-side" in {
    test(new MMU) { dut =>
      tieOffD(dut)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.tlb_flush.poke(false.B)

      val va = 0x00800000L // vpn1=2
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va.U)
      dut.clock.step(2)
      dut.io.ptw_req_addr.expect(0x5008.U)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(makePTE(ppn = 0, v = false).U)
      dut.clock.step(1) // sFault (superpage fault entry)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.clock.step(1)
      dut.io.i_fault.expect(true.B)

      // any VA inside the same 4MB region hits the same fault entry, no walk
      dut.io.i_va.poke((va + 0x123400).U)
      dut.io.i_fault.expect(true.B)
      dut.io.ptw_stall.expect(false.B)
      dut.clock.step(1)
      dut.io.ptw_active.expect(false.B)

      // after sfence.vma the region is walked again
      dut.io.tlb_flush.poke(true.B)
      dut.clock.step(1)
      dut.io.tlb_flush.poke(false.B)
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_stall.expect(true.B)
      dut.clock.step(1)
      dut.io.ptw_active.expect(true.B)
    }
  }

  it should "translate every 4KB page of a superpage with its own VPN[0]" in {
    test(new MMU) { dut =>
      tieOffD(dut)
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x5).U)
      dut.io.ptw_resp_valid.poke(false.B)

      // VA 0x00808e98 (vpn1=2, vpn0=8) fills the entry; PTE maps the region to PA 0x00800000
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(0x00808e98L.U)
      dut.clock.step(2)
      dut.io.ptw_req_addr.expect(0x5008.U)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(makePTE(ppn = 0x800, r = true, x = true).U)
      dut.clock.step(1)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.clock.step(1)
      dut.io.i_pa.expect(0x00808e98L.U)

      // another page of the same 4MB region hits the same entry but must keep its own vpn0
      dut.io.i_va.poke(0x00804100L.U)
      dut.io.ptw_stall.expect(false.B)
      dut.io.i_pa.expect(0x00804100L.U)
      dut.io.i_va.poke(0x00bff004L.U)
      dut.io.i_pa.expect(0x00bff004L.U)
    }
  }
}
