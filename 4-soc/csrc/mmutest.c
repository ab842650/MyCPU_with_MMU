// mmutest.c
#include <stdint.h>

static inline void write_csr_satp(uint32_t v) {
  asm volatile ("csrw satp, %0" :: "r"(v));
}

int main(void) {
  // 一開始就開 satp（bit31 = 1）
  write_csr_satp(0x00000000);

  // 隨便一個 VA
  volatile uint32_t *p = (volatile uint32_t *)0x2000;

  // 觸發 D-side 存取
  *p = 0x12345678;

  while (1) {}
}