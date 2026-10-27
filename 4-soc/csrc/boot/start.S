# Bootloader startup: bring the CPU from reset to a state where C can run,
# then call main(). See boot.lds for the symbols used here.

.section .text.init
.globl _start
_start:
    # 關中斷：mstatus.MIE（bit 3）清掉，mie 整個清 0（reset 值沒有規定）
    csrci mstatus, (1 << 3)
    csrw  mie, zero

    # 例外先導到 boot_trap，避免跳到未定義的 mtvec
    la    t0, boot_trap
    csrw  mtvec, t0

    # stack 在 0x10000 以下，不會被載入到 0x10000 以上的 payload 蓋掉
    la    sp, __stack_top

    # 清 .bss：用 bgeu，就算邊界沒對齊也一定會停
    la    t0, __bss_start
    la    t1, __bss_end
bss_clear_loop:
    bgeu  t0, t1, bss_clear_end
    sw    zero, 0(t0)
    addi  t0, t0, 4
    j     bss_clear_loop
bss_clear_end:

    call  main

    # main 不應該 return；萬一 return 了就停在這裡
1:  j     1b

# bootloader 自己發生例外時跳到這裡
.align 2                     # mtvec 要求 4-byte 對齊
boot_trap:
    j     boot_trap          # 先停在原地（之後可改成印出 mcause/mepc）
