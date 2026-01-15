.section .text
.globl main

.equ L1_PT_PA, 0x00005000     # physical address of L1 page table (4KB aligned)

# PTE flags: V|R|W|X|A|D = 0xCF
.equ PTE_FLAGS, 0x000000CF

main:
  # marker: M-mode start
  li  t0, 0x11112222
  csrw mscratch, t0

  # mtvec (optional)
  la  t0, m_trap
  csrw mtvec, t0

  # ---------------------------------------------------
  # 1) Clear L1 page table (1024 entries * 4 = 4096 bytes)
  # ---------------------------------------------------
  li   t0, L1_PT_PA
  li   t1, 1024
  li   t2, 0
1:
  sw   t2, 0(t0)
  addi t0, t0, 4
  addi t1, t1, -1
  bnez t1, 1b

  # ---------------------------------------------------
  # 2) Fill needed L1 leaf superpages (4MB each)
  #    Sv32 L1 leaf: PTE.V=1 and (R|X)=1
  #    For superpage: PPN0 must be 0 (it will be, since we only set bits [31:20])
  # ---------------------------------------------------
  li  t0, L1_PT_PA
  li  t1, PTE_FLAGS

  # VPN1 = 0  => VA 0x0000_0000 ~ 0x003F_FFFF  identity
  # PPN1 = 0  => PA 0x0000_0000 ~ 0x003F_FFFF
  sw  t1, 0(t0)

  # VPN1 = 1  => VA 0x0040_0000 ~ 0x007F_FFFF identity
  li  t2, (1 << 20)          # PPN1=1 goes to PTE[31:20]
  or  t2, t2, t1
  sw  t2, 4(t0)

  # OPTIONAL but strongly recommended:
  # Map a high superpage for stack if your sp is high (common: 0x8000_0000+)
  # Example: map VPN1=0x200 (VA 0x8000_0000) identity to PA 0x8000_0000
  # vpn1 = 0x8000_0000 >> 22 = 0x200
  # entry_addr = L1_PT_PA + vpn1*4
  li  t3, 0x200
  slli t3, t3, 2              # vpn1*4
  li  t0, L1_PT_PA
  add  t0, t0, t3
  li  t4, (0x200 << 20)        # PPN1 = 0x200 for PA 0x8000_0000
  or  t4, t4, t1
  sw  t4, 0(t0)

  # ---------------------------------------------------
  # 3) Enter S-mode
  # ---------------------------------------------------
  csrr t1, mstatus
  li   t0, ~(3 << 11)         # clear MPP
  and  t1, t1, t0
  li   t0, (1 << 11)          # MPP = S
  or   t1, t1, t0
  csrw mstatus, t1

  la  t0, s_main
  csrw mepc, t0
  mret

# -------------------------
# M-mode trap
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
  li  t0, 0x33334444
  lw t1,0(sp)
  csrw sscratch, t0

  # ---------------------------------------------------
  # 4) Enable paging: satp.MODE=1 (Sv32), PPN = L1_PT_PA >> 12 = 0x5
  # satp = (1<<31) | PPN
  # ---------------------------------------------------
  li  t0, (1 << 31) | (L1_PT_PA >> 12)   # 0x8000_0005
  csrw satp, t0

  # keep pipeline busy
  xor t1, t0, t0
  add t2, t0, t1
  sll t2, t2, t1
  xor t3, t2, t0

  # store -> load
  li  t4, 0xAABBCCDD
  sw  t4, 0(sp)
  nop
  nop
  nop
  nop
  lw  t5, 0(sp)

  # diff = expected ^ actual
  xor t6, t4, t5
  csrw sscratch, t6

done:
  j done