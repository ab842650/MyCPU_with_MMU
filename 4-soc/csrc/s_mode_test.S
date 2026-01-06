    .section .text
    .globl main
    .option norvc

main:
    # ===== M-mode checkpoint =====
    li      t0, 0x4D300001
    csrw    mscratch, t0

    # ===== 設定 mret 目標與 MPP=S =====
    csrr    t1, mstatus
    li      t0, ~(3 << 11)      # clear MPP[12:11]
    and     t1, t1, t0
    li      t0, (1 << 11)       # MPP = 01 (S)
    or      t1, t1, t0
    csrw    mstatus, t1

    la      t0, s_main
    csrw    mepc, t0

    li      t0, 0x4D300002
    csrw    mscratch, t0

    mret # mret


# =========================
# S-mode trap handler
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

    sret          # sret


# --- 故意拉開距離，讓 PC 跳轉超明顯 ---
    .align 4
    .space  1024                # 你也可以改 4096 更誇張


# =========================
# S-mode main
# =========================
    .align 4
s_main:
    # 把「當下 PC」寫進 sscratch，波形上很好認
    auipc   t2, 0
    csrw    sscratch, t2        # sscratch = PC(s_main)

    # 設 stvec -> s_trap（這步放 S-mode 做比較安全）
    la      t0, s_trap
    csrw    stvec, t0

    li      t0, 0x53300002
    csrw    sscratch, t0

    ecall

after_ecall:
    li      t0, 0x53300003
    csrw    sscratch, t0

done:
    j       done