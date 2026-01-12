.section .text
.globl main

main:
  # marker: M-mode start
  li  t0, 0x11112222
  csrw mscratch, t0

  # mtvec (optional)
  la  t0, m_trap
  csrw mtvec, t0

  # -------------------------
  # Build Sv32 L1 page table at PA 0x00005000
  # -------------------------
  li  t0, 0x00005000      # L1 base (must be 4KB aligned)

  # flags: V|R|W|X|A|D = 0xCF
  li  t1, 0x000000CF

  # entry 0: VPN1=0 superpage identity (ppn1=0)
  sw  t1, 0(t0)

  # entry 1: VPN1=1 superpage identity for 0x00400000 region
  # superpage needs ppn0 = 0, ppn1 = 1
  li  t2, (1 << 20)       # ppn1=1 goes into PTE[31:20]
  or  t2, t2, t1
  sw  t2, 4(t0)

  # (optional) clear a few more entries just to be safe
  sw  zero, 8(t0)
  sw  zero, 12(t0)


  # -------------------------
  # Enter S-mode
  # -------------------------
  csrr t1, mstatus
  li   t0, ~(3 << 11)      # clear MPP
  and  t1, t1, t0
  li   t0, (1 << 11)       # MPP = S
  or   t1, t1, t0
  csrw mstatus, t1

  la  t0, s_main
  csrw mepc, t0
  mret

# -------------------------
# M-mode trap (should not happen for now)
# -------------------------
.align 4
m_trap:
  li  t0, 0xDEAD0001
  csrw mscratch, t0
  j   m_trap

# =====================================================
# S-mode main
# =====================================================
.align 4
s_main:
  # marker: entered S-mode
  li  t0, 0x33334444
  csrw sscratch, t0

  # set stack (in mapped region: VPN1=1 covers 0x00400000)
  li  sp, 0x00400000

  # -------------------------
  # Enable paging: satp = Sv32 | PPN(0x5000>>12 = 0x5)
  # -------------------------
  li  t0, 0x80000005
  csrw satp, t0

  lw  t3, 0(t0)      # 讀回 entry0
  csrw mscratch, t3  # 你波形可觀察

  lw  t4, 4(t0)      # 讀回 entry1
  csrw mscratch, t4

  # marker: paging enabled
  nop
  nop
  nop

  # -------------------------------------------------
  # Your load/store stress test (unchanged logic)
  # -------------------------------------------------
  li  t6, 0              # accumulator

  li  t2, 0xAABBCCDD
  sw  t2, 0(sp)
  xor t6, t6, t2

  li  t2, 0x11223344
  sw  t2, 4(sp)
  add t6, t6, t2

  li  t2, 0x55667788
  sw  t2, 8(sp)
  xor t6, t6, t2

  li  t2, 0xDEADBEEF
  sw  t2, 12(sp)
  add t6, t6, t2

  li  t2, 0x0F0F0F0F
  sw  t2, 16(sp)
  xor t6, t6, t2

  li  t2, 0x13579BDF
  sw  t2, 20(sp)
  add t6, t6, t2

  li  t2, 0x2468ACE0
  sw  t2, 24(sp)
  xor t6, t6, t2

  li  t2, 0xCAFEBABE
  sw  t2, 28(sp)
  add t6, t6, t2

  li  t0, 0x55556666
  csrw sscratch, t0

  lw  t3, 0(sp)
  lw  t4, 4(sp)
  xor t6, t6, t3
  add t6, t6, t4

  lw  t3, 8(sp)
  lw  t4, 12(sp)
  xor t6, t6, t3
  add t6, t6, t4

  lw  t3, 16(sp)
  lw  t4, 20(sp)
  xor t6, t6, t3
  add t6, t6, t4

  lw  t3, 24(sp)
  lw  t4, 28(sp)
  xor t6, t6, t3
  add t6, t6, t4

  li  t0, 0x66667777
  csrw sscratch, t0

  li  t5, 16
loop_rmw:
  lw  t2, 0(sp)
  add t2, t2, t6
  sw  t2, 0(sp)
  lw  t3, 0(sp)
  xor t6, t6, t3

  addi sp, sp, 4
  addi t5, t5, -1
  bnez t5, loop_rmw

  # publish checksum
  csrw sscratch, t6

done:
  j done