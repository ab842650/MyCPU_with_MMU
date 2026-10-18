# Page fault end-to-end test (Sv32, faults delegated to S-mode)
#
# VA map (L0 table covers VA 0x0000_0000 ~ 0x003F_FFFF):
#   0x0000 ~ 0x6FFF : identity, V|R|W|X|A|D   (code, results, page tables)
#   0x7000          : invalid                 -> load page fault  (13)
#   0x8000          : -> PA 0x4000, V|R|A     -> store page fault (15)
#   0x9000          : invalid                 -> instruction page fault (12)
#   0xA000          : invalid at first; the handler maps it (-> PA 0x4000, R|W)
#                     and the load is retried (fix-and-retry)
#
# The S-mode handler appends {scause, stval, sepc} to RESULTS for every trap.
#   - load/store faults resume at sepc+4 (skip), except the 0xA000 page
#   - 0xA000 load fault: 1st trap fixes the PTE but does NOT sfence -> the retry hits
#     the fault entry cached in the DTLB and faults again; 2nd trap does sfence.vma
#     -> the retry walks the new PTE and succeeds
#   - fetch fault resumes at s1
#
# RESULTS layout (PA == VA):
#   0x4000 + 12*k : scause, stval, sepc of trap k
#   0x4100        : 0x1234 (read through the read-only mapping 0x8100)
#   0x4180        : 0x5A5A (read through the fixed mapping 0xA180)
#   0x4200        : trap count
#   0x4300 + 12*k : expected scause, stval, sepc of trap k
#
# Result (sim.cpp harness): 0x104 = pass bits, then 0xCAFEF00D -> 0x100
#   bit0 : all 6 trap records match
#   bit1 : precise: faulting load not written back, younger instructions ran once
#   bit2 : read-only page: load works, faulting stores did not reach memory
#   bit3 : fix-and-retry: fault entry cached until sfence.vma, retry load correct
# 0xF = pass. 0xBAD at 0x104 if M-mode caught a trap.

.equ RESULTS,    0x00004000
.equ EXPECT,     0x00004300
.equ L1_PT_PA,   0x00005000
.equ L0_PT0_PA,  0x00006000
.equ NTRAPS,     6

.equ PTE_PTR,    0x001           # V=1, R=W=X=0 (pointer)
.equ PTE_LEAF,   0x0CF           # V|R|W|X|A|D
.equ PTE_RO,     0x043           # V|R|A
.equ PTE_RW,     0x0C7           # V|R|W|A|D

.equ SIM_DONE,   0x100
.equ SIM_RESULT, 0x104

.section .text.init
.globl _start
_start:
  la   t0, m_trap
  csrw mtvec, t0
  la   t0, s_trap
  csrw stvec, t0
  # delegate instruction/load/store page faults to S-mode
  li   t0, (1 << 12) | (1 << 13) | (1 << 15)
  csrw medeleg, t0

  # clear what we use: RESULTS (0x400 B), L1[0..15], L0[0..15]
  li   t0, RESULTS
  li   t1, RESULTS + 0x400
1:
  sw   zero, 0(t0)
  addi t0, t0, 4
  blt  t0, t1, 1b
  li   t0, L1_PT_PA
  li   t2, L0_PT0_PA
  li   t1, 16
1:
  sw   zero, 0(t0)
  sw   zero, 0(t2)
  addi t0, t0, 4
  addi t2, t2, 4
  addi t1, t1, -1
  bnez t1, 1b

  # L1[0] -> L0_PT0
  li   t0, L1_PT_PA
  li   t1, ((L0_PT0_PA >> 12) << 10) | PTE_PTR
  sw   t1, 0(t0)

  # L0[0..6] identity leaves
  li   t0, L0_PT0_PA
  li   t1, 0
  li   t4, 7
2:
  slli t3, t1, 10
  ori  t3, t3, PTE_LEAF
  sw   t3, 0(t0)
  addi t0, t0, 4
  addi t1, t1, 1
  blt  t1, t4, 2b

  # L0[8] : VA 0x8000 -> PA 0x4000, read-only
  li   t0, L0_PT0_PA + 8 * 4
  li   t1, ((RESULTS >> 12) << 10) | PTE_RO
  sw   t1, 0(t0)

  # data read back through the mappings
  li   t0, RESULTS
  li   t1, 0x1234
  sw   t1, 0x100(t0)
  li   t1, 0x5A5A
  sw   t1, 0x180(t0)

  # expected trap records {scause, stval, sepc}
  li   t0, EXPECT
  la   t2, pf_load
  li   t1, 13
  sw   t1, 0(t0)
  li   t1, 0x7000
  sw   t1, 4(t0)
  sw   t2, 8(t0)
  la   t2, pf_store
  li   t1, 15
  sw   t1, 12(t0)
  li   t1, 0x8100
  sw   t1, 16(t0)
  sw   t2, 20(t0)
  la   t2, pf_store2
  li   t1, 15
  sw   t1, 24(t0)
  li   t1, 0x8104
  sw   t1, 28(t0)
  sw   t2, 32(t0)
  li   t1, 12
  sw   t1, 36(t0)
  li   t1, 0x9000
  sw   t1, 40(t0)
  sw   t1, 44(t0)
  la   t2, pf_retry
  li   t1, 13
  sw   t1, 48(t0)                # retry, 1st trap (PTE fixed, no sfence)
  li   t1, 0xA180
  sw   t1, 52(t0)
  sw   t2, 56(t0)
  li   t1, 13
  sw   t1, 60(t0)                # retry, 2nd trap (cached fault entry)
  li   t1, 0xA180
  sw   t1, 64(t0)
  sw   t2, 68(t0)

  li   s0, 0                     # trap count
  li   s2, 0                     # fix-and-retry state

  # enter S-mode at s_main
  csrr t0, mstatus
  li   t1, ~(3 << 11)
  and  t0, t0, t1
  li   t1, (1 << 11)             # MPP = S
  or   t0, t0, t1
  csrw mstatus, t0
  la   t0, s_main
  csrw mepc, t0
  mret

# =====================================================
.align 4
s_main:
  li   t0, (1 << 31) | (L1_PT_PA >> 12)
  csrw satp, t0
  sfence.vma x0, x0

  li   t3, 0x55
  li   t5, 0
  li   t6, 0

  # (1) load from unmapped page -> scause 13
  li   a0, 0x7000
pf_load:
  lw   t3, 0(a0)
  addi t5, t5, 1                 # younger instruction: must execute exactly once

  # (2) store to read-only page (PTW leaf check) -> scause 15
  li   a1, 0x8000
  li   t2, 0xDEAD
pf_store:
  sw   t2, 0x100(a1)
  addi t6, t6, 1

  # (3) load through read-only page: OK (DTLB entry R=1, W=0)
  lw   a2, 0x100(a1)

  # (4) store again: DTLB hit, permission fault -> scause 15
pf_store2:
  sw   t2, 0x104(a1)
  addi t6, t6, 1

  # (5) jump to unmapped code -> scause 12, sepc = stval = 0x9000
  la   s1, after_ifault
  li   t0, 0x9000
  jalr zero, 0(t0)

after_ifault:
  # (6) fix-and-retry: faults twice, then reads PA 0x4180
  li   a7, 0xA000
  li   a6, 0
pf_retry:
  lw   a6, 0x180(a7)

# ---------------- self check ----------------
  li   s3, 0                     # pass bits

  # bit0: trap records
  li   t0, RESULTS
  li   t1, EXPECT
  li   t2, NTRAPS * 3
7:
  lw   t4, 0(t0)
  lw   a0, 0(t1)
  bne  t4, a0, 8f
  addi t0, t0, 4
  addi t1, t1, 4
  addi t2, t2, -1
  bnez t2, 7b
  li   t4, RESULTS + 0x200
  lw   t4, 0(t4)
  li   a0, NTRAPS
  bne  t4, a0, 8f
  ori  s3, s3, 1
8:
  # bit1: precise exceptions
  li   a0, 0x55
  bne  t3, a0, 9f
  li   a0, 1
  bne  t5, a0, 9f
  li   a0, 2
  bne  t6, a0, 9f
  ori  s3, s3, 2
9:
  # bit2: read-only page
  li   a0, 0x1234
  bne  a2, a0, 10f
  li   t0, RESULTS
  lw   t4, 0x100(t0)
  bne  t4, a0, 10f
  lw   t4, 0x104(t0)
  bnez t4, 10f
  ori  s3, s3, 4
10:
  # bit3: fix-and-retry
  li   a0, 0x5A5A
  bne  a6, a0, 11f
  li   a0, 2
  bne  s2, a0, 11f
  ori  s3, s3, 8
11:
  li   t0, SIM_RESULT
  sw   s3, 0(t0)
  li   t0, SIM_DONE
  li   t1, 0xCAFEF00D
  sw   t1, 0(t0)
done:
  j    done

# =====================================================
# S-mode trap handler (runs translated; identity-mapped)
.align 4
s_trap:
  li   a3, 12
  # a4 = RESULTS + 12 * s0 (no M extension: add in a loop)
  li   a4, RESULTS
  mv   a5, s0
3:
  beqz a5, 4f
  add  a4, a4, a3
  addi a5, a5, -1
  j    3b
4:
  csrr a5, scause
  sw   a5, 0(a4)
  csrr a5, stval
  sw   a5, 4(a4)
  csrr a5, sepc
  sw   a5, 8(a4)
  addi s0, s0, 1
  li   a4, RESULTS + 0x200
  sw   s0, 0(a4)

  csrr a5, scause
  li   a4, 12
  beq  a5, a4, 5f
  # fault on the 0xA000 page -> fix-and-retry
  csrr a5, stval
  srli a5, a5, 12
  li   a4, 0xA
  beq  a5, a4, 12f
  # other load/store fault: skip the faulting instruction
  csrr a5, sepc
  addi a5, a5, 4
  csrw sepc, a5
  sret
5:
  # instruction fault: resume at the recovery address
  csrw sepc, s1
  sret
12:
  bnez s2, 13f
  # 1st: map VA 0xA000 -> PA 0x4000 (R|W) but deliberately skip sfence.vma
  li   a4, L0_PT0_PA + 0xA * 4
  li   a5, ((RESULTS >> 12) << 10) | PTE_RW
  sw   a5, 0(a4)
  li   s2, 1
  sret                           # retry -> faults again on the cached fault entry
13:
  # 2nd: now flush the TLB (per-address form, as kernels use), retry succeeds
  csrr a4, stval
  sfence.vma a4, zero
  li   s2, 2
  sret

# =====================================================
.align 4
m_trap:
  li   t0, SIM_RESULT
  li   t1, 0xBAD
  sw   t1, 0(t0)
  li   t0, SIM_DONE
  li   t1, 0xCAFEF00D
  sw   t1, 0(t0)
6:
  j    6b
