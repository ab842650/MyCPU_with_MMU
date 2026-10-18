# M-mode firmware for running rv32emu's tests/system/mmu (vm.elf) on MyCPU.
#
# rv32emu runs vm.elf with its ELF loader, which also services ecall syscalls.
# MyCPU instead boots a flat image at PA 0x1000, so mkimage.py builds one with:
#   PA 0x1000      : boot stub (lui t0,0x100; jr t0) -> fw_start. This overwrites
#                    the first 8 bytes of vm.elf's .mystring, restored below.
#   PA 0x100000    : this firmware, followed by a copy of PA 0x0..0x1007 of vm.elf
#   everything else: vm.elf as linked
#
# Syscalls (from S-mode, not delegated -> trap here):
#   write (a7=64): send a2 bytes at a1 to the UART; a0 <- a2
#   exit  (a7=93): report to sim.cpp: 0x104 <- 0xF if a0 == 0, else 0xBAD00000|a0,
#                  then 0xCAFEF00D -> 0x100 (sim.cpp prints TEST PASSED/FAILED)
# Any other trap reaches M-mode is reported as 0xDEAD0000|mcause.
#
# Buffers are read as physical addresses: vm.elf keeps its stacks identity-mapped
# (rv32emu's syscall handler makes the same assumption).
# mepc: MyCPU's CLINT records the address after the ecall, so no +4 here.

.equ VM_ENTRY,   0x00800004      # vm.elf _start
.equ LO_LEN,     0x1008          # bytes of PA 0x0.. saved by mkimage.py
.equ UART_BASE,  0x40000000
.equ SIM_DONE,   0x100
.equ SIM_RESULT, 0x104

.section .text
.globl fw_start
fw_start:
  # restore PA 0x0..LO_LEN-1 (the boot stub overwrote part of it)
  la   t0, saved_lo
  li   t1, 0
  li   t2, LO_LEN
1:
  lw   t3, 0(t0)
  sw   t3, 0(t1)
  addi t0, t0, 4
  addi t1, t1, 4
  blt  t1, t2, 1b

  la   t0, fw_trap
  csrw mtvec, t0
  li   t0, VM_ENTRY
  jr   t0

.align 4
fw_trap:
  csrw mscratch, t0              # vm.elf has consumed mscratch by now
  la   t0, save_area
  sw   t1, 0(t0)
  sw   t2, 4(t0)
  sw   t3, 8(t0)
  sw   a1, 12(t0)
  sw   a2, 16(t0)

  csrr t1, mcause
  li   t2, 8                     # ecall from U
  beq  t1, t2, ecall
  li   t2, 9                     # ecall from S
  beq  t1, t2, ecall
  li   t2, 11                    # ecall from M
  beq  t1, t2, ecall
  li   t2, 0xDEAD0000
  or   t1, t1, t2
  j    report

ecall:
  li   t2, 64
  beq  a7, t2, sys_write
  li   t2, 93
  beq  a7, t2, sys_exit
  li   t1, 0xDEAD0000            # unknown syscall: 0xDEAD0000 | a7 << 8
  slli t2, a7, 8
  or   t1, t1, t2
  j    report

sys_write:
  li   t1, UART_BASE
  mv   a0, a2
2:
  beqz a2, ret_from_trap
3:
  lw   t2, 0(t1)                 # UART_STATUS bit0: TX ready
  andi t2, t2, 1
  beqz t2, 3b
  lbu  t3, 0(a1)
  sw   t3, 0x10(t1)              # UART_SEND
  addi a1, a1, 1
  addi a2, a2, -1
  j    2b

ret_from_trap:
  la   t0, save_area
  lw   t1, 0(t0)
  lw   t2, 4(t0)
  lw   t3, 8(t0)
  lw   a1, 12(t0)
  lw   a2, 16(t0)
  csrr t0, mscratch
  mret

sys_exit:
  li   t1, 0xF
  beqz a0, report
  li   t1, 0xBAD00000
  or   t1, t1, a0
report:
  # let the UART finish shifting out the last byte before sim.cpp stops
  li   t2, UART_BASE
5:
  lw   t3, 0(t2)
  andi t3, t3, 1
  beqz t3, 5b
  li   t3, 4000                  # > 1 byte time (10 bits * 434 cycles)
6:
  addi t3, t3, -1
  bnez t3, 6b
  li   t2, SIM_RESULT
  sw   t1, 0(t2)
  li   t2, SIM_DONE
  li   t1, 0xCAFEF00D
  sw   t1, 0(t2)
7:
  j    7b

.align 4
save_area:
  .space 32

.align 4
saved_lo:
  .incbin "saved_lo.bin"
