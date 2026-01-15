// SPDX-License-Identifier: MIT
package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class MMUSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MMU"

  // Helper to create a PTE
  def makePTE(ppn: Int, v: Boolean = true, r: Boolean = false, 
              w: Boolean = false, x: Boolean = false, u: Boolean = false): Int = {
    var pte = 0
    if (v) pte |= (1 << 0)
    if (r) pte |= (1 << 1)
    if (w) pte |= (1 << 2)
    if (x) pte |= (1 << 3)
    if (u) pte |= (1 << 4)
    pte |= (ppn << 10)
    pte
  }

  // Helper to create SATP register value
  def makeSATP(mode: Int, ppn: Int): Long = {
    ((mode.toLong << 31) | (ppn & 0x3FFFFF))
  }

  it should "passthrough addresses when MMU is disabled" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(false.B)
      dut.io.satp.poke(0.U)
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(0x12345678L.U)
      
      dut.clock.step(1)
      
      dut.io.i_pa.expect(0x12345678L.U)
      dut.io.i_stall.expect(false.B)
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "passthrough when satp.mode = 0 even if enable = true" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(0, 0x1000).U) // mode = 0 (bare)
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(0x12345678L.U)
      
      dut.clock.step(1)
      
      dut.io.i_pa.expect(0x12345678L.U)
      dut.io.i_stall.expect(false.B)
      dut.io.i_fault.expect(false.B)
    }
  }

  it should "perform page table walk on TLB miss" in {
    test(new MMU) { dut =>
      // Setup: Sv32 mode, root page table at PPN 0x1000
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      // Access VA 0x10400000 (VPN[1] = 0x41, VPN[0] = 0x000)
      val testVA = 0x10400000L
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      
      // Initially no response
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(1)
      
      // Should start PTW and stall
      dut.io.i_stall.expect(true.B)
      dut.io.ptw_active.expect(true.B)
      
      dut.clock.step(1)
      
      // Should request L1 PTE
      // L1 PTE addr = root_base + (VPN[1] << 2)
      // VPN[1] = VA[31:22] = 0x10400000 >> 22 = 0x41
      //             = 0x1000000 + (0x41 << 2) = 0x1000104
      dut.io.ptw_req_valid.expect(true.B)
      val expectedL1Addr = (0x1000 << 12) + (0x41 << 2)
      dut.io.ptw_req_addr.expect(expectedL1Addr.U)
      
      dut.clock.step(1)
      
      // Provide PTE response: map to PPN 0x2000, with V=1, X=1 (executable leaf)
      val pte = makePTE(ppn = 0x2000, v = true, x = true)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(pte.U)
      
      dut.clock.step(1)
      
      // Should complete and fill TLB
      dut.io.i_stall.expect(false.B)
      dut.io.ptw_active.expect(false.B)
      
      // Check translation: PA = {PPN, offset} = {0x2000, 0x000} = 0x02000000
      val expectedPA = 0x2000 << 12
      dut.io.i_pa.expect(expectedPA.U)
      
      // Clear response
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(1)
      
      // Access same page again - should hit TLB (no stall)
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke((testVA + 0x100).U) // Same page, different offset
      
      dut.clock.step(1)
      
      dut.io.i_stall.expect(false.B)
      dut.io.ptw_req_valid.expect(false.B)
      val expectedPA2 = (0x2000 << 12) + 0x100
      dut.io.i_pa.expect(expectedPA2.U)
    }
  }

  it should "fault on invalid PTE (V=0)" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      val testVA = 0x10400000L
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(2) // Move to sL1Wait
      
      // Provide invalid PTE (V=0)
      val invalidPTE = makePTE(ppn = 0x2000, v = false)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(invalidPTE.U)
      
      dut.clock.step(1)
      
      // Should fault
      dut.io.i_fault.expect(true.B)
      
      dut.clock.step(1)
      
      // Should return to idle
      dut.io.i_fault.expect(false.B)
      dut.io.ptw_active.expect(false.B)
    }
  }

  it should "fault on non-executable PTE for instruction fetch" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      val testVA = 0x10400000L
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(2) // Move to sL1Wait
      
      // Provide PTE with V=1 but X=0, R=1 (readable but not executable)
      val nonExecPTE = makePTE(ppn = 0x2000, v = true, r = true, x = false)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(nonExecPTE.U)
      
      dut.clock.step(1)
      
      // Should fault (no execute permission)
      dut.io.i_fault.expect(true.B)
      
      dut.clock.step(1)
      
      dut.io.i_fault.expect(false.B)
    }
  }

  it should "handle back-to-back accesses to different pages" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      // First access to page 1
      val va1 = 0x10400000L
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(va1.U)
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(2)
      
      val pte1 = makePTE(ppn = 0x2000, v = true, x = true)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(pte1.U)
      
      dut.clock.step(2)
      
      dut.io.ptw_resp_valid.poke(false.B)
      
      // Access to page 2 (different page = TLB miss with 1-entry TLB)
      val va2 = 0x10800000L
      dut.io.i_va.poke(va2.U)
      
      dut.clock.step(1)
      
      // Should miss and start new PTW
      dut.io.i_stall.expect(true.B)
      dut.io.ptw_active.expect(true.B)
      
      dut.clock.step(1)
      
      val pte2 = makePTE(ppn = 0x3000, v = true, x = true)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(pte2.U)
      
      dut.clock.step(1)
      
      // Should complete with new translation
      dut.io.i_stall.expect(false.B)
      val expectedPA2 = 0x3000 << 12
      dut.io.i_pa.expect(expectedPA2.U)
    }
  }

  it should "not start PTW when i_valid is false" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      // Invalid request
      dut.io.i_valid.poke(false.B)
      dut.io.i_va.poke(0x10400000L.U)
      
      dut.clock.step(5)
      
      // Should never activate PTW
      dut.io.ptw_active.expect(false.B)
      dut.io.ptw_req_valid.expect(false.B)
      dut.io.i_stall.expect(false.B)
    }
  }

  it should "correctly compute L1 PTE address" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      
      // SATP: mode=1, PPN=0xABCDE
      val rootPPN = 0x1234
      dut.io.satp.poke(makeSATP(1, rootPPN).U)
      
      // VA with VPN[1] = 0x123
      val testVA = 0x48C00000L // VPN[1] = 0x123
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(2)
      
      // L1 PTE address = (rootPPN << 12) + (VPN[1] << 2)
      val expectedAddr = (rootPPN << 12) + (0x123 << 2)
      dut.io.ptw_req_addr.expect(expectedAddr.U)
    }
  }

  it should "fault when PTE is pointer (not leaf) in v0" in {
    test(new MMU) { dut =>
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      val testVA = 0x10400000L
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(2)
      
      // Provide pointer PTE (V=1, but R=W=X=0 = not a leaf)
      val pointerPTE = makePTE(ppn = 0x2000, v = true, r = false, w = false, x = false)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(pointerPTE.U)
      
      dut.clock.step(1)
      
      // v0 doesn't support L0 walk, so should fault
      dut.io.i_fault.expect(true.B)
    }
  }

  it should "invalidate TLB when MMU is disabled" in {
    test(new MMU) { dut =>
      // First, fill the TLB with Sv32 enabled
      dut.io.enable.poke(true.B)
      dut.io.satp.poke(makeSATP(1, 0x1000).U)
      
      val testVA = 0x10400000L
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      dut.io.ptw_resp_valid.poke(false.B)
      
      dut.clock.step(2)
      
      val pte = makePTE(ppn = 0x2000, v = true, x = true)
      dut.io.ptw_resp_valid.poke(true.B)
      dut.io.ptw_resp_data.poke(pte.U)
      
      dut.clock.step(2)
      
      // Verify TLB hit (no stall)
      dut.io.ptw_resp_valid.poke(false.B)
      dut.io.i_stall.expect(false.B)
      
      dut.clock.step(1)
      
      // Now disable MMU (this should invalidate TLB)
      dut.io.enable.poke(false.B)
      
      dut.clock.step(1)
      
      // Re-enable MMU
      dut.io.enable.poke(true.B)
      
      // Access same address - should miss and start PTW (TLB was invalidated)
      dut.io.i_valid.poke(true.B)
      dut.io.i_va.poke(testVA.U)
      
      dut.clock.step(1)
      
      dut.io.i_stall.expect(true.B)
      dut.io.ptw_active.expect(true.B)
    }
  }
}