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

    # (2) set medeleg: delegate ECALL from S (cause=9) to S-mode
    #     medeleg[9] = 1  => ECALL in S goes to stvec (S trap)
    #
    # ---- 切換測試點：----
    # delegated 測試：li t0, (1<<9)
    # not delegated：li t0, 0
    li      t0, (1<<9)
    csrw    medeleg, t0

    li      t0, 0x4D300002
    csrw    mscratch, t0

    # (3) Prepare to enter S-mode via mret: set MPP = S, mepc = s_main
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

    mret


# =========================
# M-mode trap handler
# 觀察點：如果「沒被 delegated」，S-mode 的 ecall 會跳到這裡 (mtvec)
# =========================
    .align 4
m_trap:
    li      t0, 0x4D30EE01
    csrw    mscratch, t0

    # skip the faulting instr (ecall) so we don't trap forever
    csrr    t1, mepc
    addi    t1, t1, 4
    csrw    mepc, t1

    li      t0, 0x4D30EE02
    csrw    mscratch, t0

    mret


# =========================
# S-mode trap handler
# 觀察點：如果「有 delegated」，S-mode 的 ecall 會跳到這裡 (stvec)
# =========================
    .align 4
s_trap:
    li      t0, 0x5330EE01
    csrw    sscratch, t0

    csrr    t1, sepc
    addi    t1, t1, 4
    csrw    sepc, t1

    li      t0, 0x5330EE02
    csrw    sscratch, t0

    sret


# --- 拉開距離讓 PC 跳轉超明顯 ---
    .align 4
    .space  1024


# =========================
# S-mode main
# =========================
    .align 4
s_main:
    # marker: we're in S main
    auipc   t2, 0
    csrw    sscratch, t2

    # set stvec -> s_trap
    la      t0, s_trap
    csrw    stvec, t0

    li      t0, 0x53300002
    csrw    sscratch, t0

    # trigger exception from S
    ecall

after_ecall:
    li      t0, 0x53300003
    csrw    sscratch, t0

done:
    j       done