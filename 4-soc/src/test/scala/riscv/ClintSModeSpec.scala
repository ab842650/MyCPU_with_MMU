package riscv.core

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import riscv.Parameters

class ClintSModeSpec extends AnyFreeSpec with ChiselScalatestTester {

  "S-mode sret should jump to sepc" in {
    test(new CLINT) { dut =>
      // ---- defaults ----
      dut.io.interrupt_flag.poke(InterruptStatus.None)
      dut.io.jump_flag.poke(false.B)
      dut.io.jump_address.poke(0.U)
      dut.io.instruction_address_if.poke(0.U)

      // force S-mode (不要從 CSR priv_mode_out 取)
      dut.io.priv_mode.poke(PrivMode.S)

      // 設定 supervisor CSR 輸入（CLINT 的 csr_bundle 是 Input）
      dut.io.csr_bundle.sstatus.poke(0.U)
      dut.io.csr_bundle.sepc.poke("h80000100".U)
      dut.io.csr_bundle.scause.poke(0.U)
      dut.io.csr_bundle.stvec.poke("h80000080".U)
      dut.io.csr_bundle.stval.poke(0.U)
      dut.io.csr_bundle.sscratch.poke(0.U)

      // M-mode 相關也給個值避免 X
      dut.io.csr_bundle.mstatus.poke(0.U)
      dut.io.csr_bundle.mie.poke(0.U)
      dut.io.csr_bundle.mtvec.poke(0.U)
      dut.io.csr_bundle.mepc.poke(0.U)
      dut.io.csr_bundle.mcause.poke(0.U)
      dut.io.csr_bundle.mtval.poke(0.U)

      // issue sret
      dut.io.instruction_id.poke(InstructionsRet.sret)
      dut.clock.step(1)

      // expect jump to sepc
      dut.io.id_interrupt_assert.expect(true.B)
      dut.io.id_interrupt_handler_address.expect("h80000100".U)

      // CLINT 應該要寫 sstatus（恢復/更新），並拉 direct_write_enable_s
      dut.io.csr_bundle.direct_write_enable_s.expect(true.B)
    }
  }
}