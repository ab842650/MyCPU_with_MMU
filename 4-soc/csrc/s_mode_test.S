.section .text
.globl main

.equ L1_PT_PA,   0x00005000      # 4KB aligned
.equ L0_PT0_PA,  0x00006000      # vpn1=0  -> VA 0x0000_0000 ~ 0x003F_FFFF

.equ PTE_PTR,    0x001           # V=1, R=W=X=0  (pointer)
.equ PTE_LEAF,   0x0CF           # V|R|W|X|A|D

main:
  # 1) mtvec point to trap entry
  la   t0, __trap_entry
  csrw mtvec, t0

  # ---------------------------------------------------
  # 2) clear L1 + L0 tables (only tables we use)
  # ---------------------------------------------------
  li   t2, 0

  # clear L1 page table @ L1_PT_PA
  li   t0, L1_PT_PA
  li   t1, 1024
1:
  sw   t2, 0(t0)
  addi t0, t0, 4
  addi t1, t1, -1
  bnez t1, 1b

  # clear L0_PT0 @ L0_PT0_PA
  li   t0, L0_PT0_PA
  li   t1, 1024
2:
  sw   t2, 0(t0)
  addi t0, t0, 4
  addi t1, t1, -1
  bnez t1, 2b

  # ---------------------------------------------------
  # 3) L1[0] -> L0_PT0 (pointer)
  #    L1[1] = superpage leaf (4MB) for VA 0x0040_0000~0x007F_FFFF
  # ---------------------------------------------------
  li   t0, L1_PT_PA

  # L1[0] pointer -> L0_PT0
  li   t1, (L0_PT0_PA >> 12)
  slli t1, t1, 10
  ori  t1, t1, PTE_PTR
  sw   t1, 0(t0)              # entry vpn1=0

  # L1[1] superpage leaf:
  # PPN1 = 1, and PPN0 must be 0 => just put (1<<20) into PTE[31:20]
  li   t2, (1 << 20)          # PPN1 goes to bits [31:20]
  ori  t2, t2, PTE_LEAF
  sw   t2, 4(t0)              # entry vpn1=1 (superpage leaf)

  # ---------------------------------------------------
  # 4) Fill L0_PT0: identity map 0~4MB (vpn1=0 chunk)
  # ---------------------------------------------------
  li   t0, L0_PT0_PA
  li   t1, 0                  # vpn0
4:
  slli t2, t1, 12             # va within vpn1=0 region
  srli t3, t2, 12             # ppn = va>>12 (identity)
  slli t3, t3, 10
  ori  t3, t3, PTE_LEAF
  sw   t3, 0(t0)

  addi t0, t0, 4
  addi t1, t1, 1
  li   t4, 1024
  blt  t1, t4, 4b

  # ---------------------------------------------------
  # 5) Enter S-mode
  # ---------------------------------------------------
  csrr t0, mstatus
  li   t1, ~(3 << 11)
  and  t0, t0, t1
  li   t1, (1 << 11)          # MPP=S
  or   t0, t0, t1
  csrw mstatus, t0

  la   t0, s_main
  csrw mepc, t0
  mret

# =====================================================
# S-mode main (sfence.vma test)
# =====================================================
.align 4
s_main:
  # enable Sv32
  li   t0, (1 << 31) | (L1_PT_PA >> 12)
  csrw satp, t0

  # ---------------------------------------------------
  # Target VA for sfence test
  # ---------------------------------------------------
  li   t1, 0x00001000

  # ---------------------------------------------------
  # (1) First access -> expect TLB miss + PTW
  # ---------------------------------------------------
  li   t2, 0x11111111
  sw   t2, 0(t1)
  lw   t3, 0(t1)

  # ---------------------------------------------------
  # (2) Second access (same VA) -> expect TLB hit
  # ---------------------------------------------------
  li   t2, 0x22222222
  sw   t2, 0(t1)
  lw   t3, 0(t1)

  # ---------------------------------------------------
  # (3) sfence.vma (flush all)
  # ---------------------------------------------------
  sfence.vma x0, x0

  # ---------------------------------------------------
  # (4) Third access after sfence
  #     -> MUST miss again and re-walk PTW
  # ---------------------------------------------------
  li   t2, 0x33333333
  sw   t2, 0(t1)
  lw   t3, 0(t1)

done:
  j done