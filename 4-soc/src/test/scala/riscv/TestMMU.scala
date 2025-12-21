// src/test/scala/riscv/RunMMU.scala
package riscv

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class mmutest extends AnyFlatSpec with ChiselScalatestTester {
  behavior.of("Run MMU")

  it should "just run mmutest and print MMU logs" in {
    test(new TestTopModule("mmutest.asmbin")).withAnnotations(TestAnnotations.annos) { dut =>
      dut.clock.setTimeout(0)

      // 跑久一點，讓 store 發生
      dut.clock.step(1000)

      // 不 assert，不檢查，結束
    }
  }
}