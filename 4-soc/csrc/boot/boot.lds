OUTPUT_ARCH("riscv")
ENTRY(_start)

SECTIONS
{
  . = 0x00001000;                 /* sim 從 0x1000 開始取指令 */
  .text : {
    *(.text.init)                 /* start.S 一定要排第一個，0x1000 必須是 _start */
    *(.text*)
  }
  .rodata : { *(.rodata*) *(.srodata*) }
  .data   : { *(.data*) *(.sdata*) }
  .bss    : {
    . = ALIGN(4);
    __bss_start = .;
    *(.bss*) *(.sbss*) *(COMMON)
    . = ALIGN(4);
    __bss_end = .;
  }
  __boot_end = .;                 /* bootloader 結束的位址，之後拿來檢查有沒有超過 */
  __stack_top = 0x00010000;
}
