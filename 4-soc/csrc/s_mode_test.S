    .section .text
    .globl main
    .option norvc

main:
    # =========================
    # M-mode init
    # =========================
    li      t0, 0x4D300001
    csrw    mscratch, t0

    # (1) set mtvec -> m_trap
    la      t0, m_trap
    csrw    mtvec, t0

    # (2) delegate instruction page fault (cause=12) to S-mode
    #     medeleg[12] = 1
    li      t0, (1 << 12)
    csrw    medeleg, t0

    li      t0, 0x4D300002
    csrw    mscratch, t0

    # (3) enter S-mode via mret: set MPP = S, mepc = s_main
    csrr    t1, mstatus
    li      t0, ~(3 << 11)      # clear MPP[12:11]
    and     t1, t1, t0
    li      t0, (1 << 11)       # MPP = 01 (S)
    or      t1, t1, t0
    csrw    mstatus, t1

    la      t0, s_main
    csrw    mepc, t0

    li      t0, 0x4D300003
    csrw    mscratch, t0

    li      t0, 0x80000000       # bit31=1
    csrw    satp, t0

    mret


# =========================
# M-mode trap handler
# 觀察點：如果「沒被 delegated」，page fault 會跳到這裡
# =========================
    .align 4
m_trap:
    li      t0, 0x4D300F01
    csrw    mscratch, t0

    # 讀 mcause 看是不是 12
    csrr    t2, mcause
    csrw    mscratch, t2      # 或者存到你方便看的地方

    # skip faulting instr to avoid infinite trap (best-effort)
    csrr    t1, mepc
    addi    t1, t1, 4
    csrw    mepc, t1

    li      t0, 0x4D30F002
    csrw    mscratch, t0
    mret


# =========================
# S-mode trap handler
# 觀察點：page fault delegated 會跳到這裡
# =========================
    .align 4
s_trap:
    li      t0, 0x53300F01
    csrw    sscratch, t0

    # 讀 scause 應該是 12
    csrr    t2, scause
    csrw    sscratch, t2

    # 讀 stval 應該是 fault VA (=0x2000)
    csrr    t3, stval
    csrw    sscratch, t3

    # skip faulting instruction so we don't trap forever
    csrr    t1, sepc
    addi    t1, t1, 4
    csrw    sepc, t1

    li      t0, 0x53300F02
    csrw    sscratch, t0
    sret


# =========================
# S-mode main
# =========================
    .align 4
s_main:
    # marker: we're in S main
    li      t0, 0x53300001
    csrw    sscratch, t0

    # set stvec -> s_trap
    la      t0, s_trap
    csrw    stvec, t0

    li      t0, 0x53300002
    csrw    sscratch, t0

    # ---- trigger instruction page fault ----
    # Jump to VA 0x00002000 (MMU will fault only for this fetch)
    li      t0, 0x00002000
    jr      t0

after_pf:
    li      t0, 0x53300003
    csrw    sscratch, t0

done:
    j       done