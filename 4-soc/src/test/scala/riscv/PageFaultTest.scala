// SPDX-License-Identifier: MIT
// MyCPU is freely redistributable under the MIT License. See the file
// "LICENSE" for information on usage and redistribution of this file.

package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

// End-to-end Sv32 page faults delegated to S-mode (csrc/page_fault_test.S).
// The program checks itself; this test reads its pass bits and trap records.
class PageFaultTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Sv32 page fault handling")

  it should "trap load/store/instruction page faults precisely into S-mode" in {
    test(new TestTopModule("page_fault_test.asmbin")).withAnnotations(TestAnnotations.annos) { dut =>
      dut.clock.setTimeout(0)
      dut.io.interrupt_flag.poke(0.U)

      def readMem(addr: Int): BigInt = {
        dut.io.mem_debug_read_address.poke(addr.U)
        dut.clock.step()
        dut.io.mem_debug_read_data.peekInt()
      }

      var done   = BigInt(0)
      var cycles = 0
      while (done != BigInt(0xcafef00dL) && cycles < 2000000) {
        dut.clock.step(1000)
        cycles += 1000
        done = readMem(0x100)
      }
      val result = if (done == BigInt(0xcafef00dL)) readMem(0x104) else BigInt(0)

      // on failure, show the recorded vs expected traps
      if (result != 0xf) {
        info(s"trap count = ${readMem(0x4200)}")
        for (k <- 0 until 6) {
          val got = (0 until 3).map(i => readMem(0x4000 + 12 * k + 4 * i).toString(16))
          val exp = (0 until 3).map(i => readMem(0x4300 + 12 * k + 4 * i).toString(16))
          info(s"trap $k: got ${got.mkString(",")} expected ${exp.mkString(",")}")
        }
      }
      assert(done == BigInt(0xcafef00dL), s"program did not finish after $cycles cycles")
      assert(result != 0xbad, "M-mode caught an undelegated trap")

      val names = Seq("trap records", "precise squash", "read-only page", "fix-and-retry")
      for ((name, bit) <- names.zipWithIndex)
        assert(result.testBit(bit), s"check failed: $name (result=0x${result.toString(16)})")
    }
  }
}
