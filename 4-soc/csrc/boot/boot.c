// UART bootloader for MyCPU.
// start.S sets up the stack and .bss, then calls main() here.

#include <stdint.h>

#include "../mmio.h"

#define UART_TX_READY (1u << 0) /* UART_STATUS bit 0: 可以送下一個字元 */
#define UART_RX_VALID (1u << 1) /* UART_STATUS bit 1: RX FIFO 有資料 */

// 送一個字元
void uart_putc(char c)
{
    // TODO 1: 一直等到 UART_STATUS 的 TX ready 位元是 1
    while(!(*UART_STATUS & UART_TX_READY));
    // TODO 2: 把 c 寫進 UART_SEND
    *UART_SEND = (uint8_t) c;

}

// 送一個字串，直到遇到 '\0'
void uart_puts(const char *s)
{
    // TODO 3: 逐字呼叫 uart_putc
    while(*s!='\0'){
        uart_putc(*s++);

    }
}

// 收一個 byte；沒有資料就一直等
uint8_t uart_getc(void)
{
    // TODO 4: 一直等到 UART_STATUS 的 RX valid 位元是 1
    while(!(*UART_STATUS & UART_RX_VALID));
    // TODO 5: 從 UART_RECV 讀出來回傳（只要低 8 位）
    
    return *UART_RECV & 0xff;
}

// 收 4 個 byte，照 little-endian 組成一個 32-bit 數字
// 例如依序收到 0x78 0x56 0x34 0x12 -> 回傳 0x12345678
uint32_t uart_get_u32(void)
{
    // TODO 6: 呼叫 4 次 uart_getc，第 i 個 byte 左移 8*i 位後 OR 起來
    uint32_t data=0;

    for(int i=0;i<4;++i){
        data |= (uint32_t)uart_getc()<<(8*i);
        
    }
    return data;
}

// 以 8 位 16 進位印出一個數字，例如 0x00010000 -> "00010000"（debug 用）
void uart_put_hex(uint32_t v)
{
    for (int i = 28; i >= 0; i -= 4)
        uart_putc("0123456789abcdef"[(v >> i) & 0xf]);
}

/* ===================== 封包格式 ===================== */

// "MCPU"：little-endian 下 byte 順序是 'M' 'C' 'P' 'U'
#define BOOT_MAGIC 0x5550434Du

// host 先送 header（5 個 little-endian 的 uint32），接著送 length 個 byte 的 payload
struct boot_header {
    uint32_t magic;     // 必須是 BOOT_MAGIC
    uint32_t load_addr; // payload 要寫到 RAM 的哪裡
    uint32_t entry;     // 收完之後跳到哪裡執行
    uint32_t length;    // payload 有幾個 byte
    uint32_t checksum;  // payload 所有 byte 的加總（第 4 步會用到）
};

// 收 header：照上面 struct 的欄位順序，依序填入 5 個欄位
void recv_header(struct boot_header *h)
{
    // TODO 7: 用 uart_get_u32() 依序填 h->magic、h->load_addr、h->entry、
    //         h->length、h->checksum（順序一定要跟 host 送的一樣）
    h->magic = uart_get_u32();
    h->load_addr = uart_get_u32();
    h->entry = uart_get_u32();
    h->length = uart_get_u32();
    h->checksum = uart_get_u32();
}

// 收 payload：收 len 個 byte，依序寫到 dst 開始的記憶體
void recv_payload(uint8_t *dst, uint32_t len)
{
    // TODO 8: 迴圈 len 次，每次用 uart_getc() 收一個 byte 寫進 dst[i]
    for(int i=0;i<len;++i){
        *dst++ = uart_getc();
    }
}

/* ===================== 驗證 ===================== */

// 應用程式可以被載入的範圍：[APP_MIN, APP_LIMIT)
//   0x00001000 - 0x0000ffff: bootloader 自己和它的 stack，不能被蓋掉
//   0x00100000 以上:          應用程式的 .bss 和 stack（照 csrc/link.lds），不能放 payload
#define APP_MIN   0x00010000u
#define APP_LIMIT 0x00100000u

// 檢查 header；沒問題回傳 0（NULL），有問題回傳錯誤訊息字串
const char *check_header(const struct boot_header *h)
{
    // TODO 10: 依序檢查下面幾項，不符合就 return 對應的錯誤訊息，例如
    //          return "bad magic";
    //   (a) magic 必須是 BOOT_MAGIC
    //   (b) length 不能是 0
    //   (c) load_addr 必須 >= APP_MIN
    //   (d) payload 的結尾 load_addr + length 必須 <= APP_LIMIT
    //       注意：load_addr + length 可能溢位繞回很小的數字，想想怎麼寫才不會被騙
    //   (e) entry 必須落在 [load_addr, load_addr + length) 裡面
    //   (f) entry 必須是 4 的倍數（RV32I 沒有壓縮指令，指令一定 4-byte 對齊）
    if (h->magic != BOOT_MAGIC)
        return "bad magic";
    if (h->length == 0)
        return "bad length";
    if (h->load_addr < APP_MIN)
        return "bad load_addr";
    // 用減法比較，避免 load_addr + length 溢位繞回
    if (h->load_addr > APP_LIMIT || h->length > APP_LIMIT - h->load_addr)
        return "payload out of range";
    // 上一項已保證 load_addr + length <= APP_LIMIT，這裡相加不會溢位
    if (h->entry < h->load_addr || h->entry >= h->load_addr + h->length)
        return "entry out of range";
    if ((h->entry & 3u) != 0)
        return "entry not aligned";
    return 0;
}

// 算 payload 的 checksum：所有 byte 加總（跟 mkimg.py 的算法一樣）
uint32_t calc_checksum(const uint8_t *p, uint32_t len)
{
    // TODO 11: 把 p[0] 到 p[len-1] 全部加起來回傳
    //          uint32_t 溢位會自動繞回，剛好等於 Python 的 & 0xFFFFFFFF
    uint32_t check_sum=0;
    for(uint32_t i = 0 ; i< len ;++i){
        check_sum+=p[i];
    }
    return check_sum;
}

// 印出錯誤訊息後停住，不再往下執行
void boot_fail(const char *msg)
{
    uart_puts("ERR: ");
    uart_puts(msg);
    uart_puts("\n");
    for (;;)
        ;
}

/* ===================== 交接 ===================== */

// 跳到應用程式執行，不會回來
void jump_to(uint32_t entry)
{
    // TODO 13: 執行 fence.i，確保剛用 store 寫進記憶體的指令，取指令時拿得到新的
    //   C 語言沒有 fence.i，要用 inline assembly 寫：
    //       __asm__ volatile("fence.i" ::: "memory");
    //   （Makefile 的 -march 已經加上 zifencei，不然組譯器不認得這條指令）
    __asm__ volatile("fence.i" ::: "memory");

    // TODO 14: 把 entry 這個數字轉型成「函式指標」，再呼叫它，就會跳過去執行
    //   函式指標的型別寫法：void (*)(void)，意思是「指向沒參數、沒回傳值的函式」
    //   宣告一個變數 app，型別是這個函式指標，值是 entry 轉型過來的
    void (*app)(void) = (void (*)(void))entry;
    app();

    // 應用程式不應該 return；萬一 return 了就停在這裡
    for (;;)
        ;
}

// 印出 header 的 5 個欄位（debug 用）
void print_header(const struct boot_header *h)
{
    uart_puts("magic=");
    uart_put_hex(h->magic);
    uart_puts(" load=");
    uart_put_hex(h->load_addr);
    uart_puts(" entry=");
    uart_put_hex(h->entry);
    uart_puts(" len=");
    uart_put_hex(h->length);
    uart_puts(" sum=");
    uart_put_hex(h->checksum);
    uart_puts("\n");
}

int main(void)
{
    uart_puts("MyCPU bootloader\n");

    // 從收 header 到收完 payload 之間不能送 UART：host 不會等我們，送一個
    // 字元的時間 RX 就進來一個 byte，而 RX FIFO 只有 4 格，多的會被丟掉
    struct boot_header h;
    recv_header(&h);

    const char *err = check_header(&h);
    if (err) {
        print_header(&h);
        boot_fail(err);
    }
    recv_payload((uint8_t *)h.load_addr, h.length);

    print_header(&h);
    if (calc_checksum((uint8_t *)h.load_addr, h.length) != h.checksum)
        boot_fail("bad checksum");
    uart_puts("OK\n");

    // TODO 15: 印出 "jump to " 加上 entry 位址（用 uart_put_hex），再呼叫 jump_to
    //   這時 payload 已經收完，host 不會再送資料，可以放心印
    uart_puts("jump to ");
    uart_put_hex(h.entry);
    uart_puts("\n");
    jump_to(h.entry);

    for (;;)
        ;
}
