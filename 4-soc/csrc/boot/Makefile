# UART bootloader for MyCPU.
#
#   make       # build boot.asmbin (loaded at 0x1000 by sim)
#   make run   # run it on ../../verilog/verilator/obj_dir/VTop
#   make run IMG=app.img   # ...and send app.img over UART once the banner is out
#   make run-app           # build ../uart.c for 0x10000, load it via the bootloader
#   make boot-test         # good image + broken images, check the bootloader's verdicts

CROSS_COMPILE ?= $(HOME)/riscv-none-elf-gcc/bin/riscv-none-elf-
CC      := $(CROSS_COMPILE)gcc
OBJCOPY := $(CROSS_COMPILE)objcopy
OBJDUMP := $(CROSS_COMPILE)objdump

CFLAGS  = -O2 -Wall -ffreestanding -fno-builtin -nostdlib -march=rv32i_zicsr_zifencei -mabi=ilp32
VTOP    := ../../verilog/verilator/obj_dir/VTop
# Sim cycles are half clock cycles; one UART byte is ~9.5K of them, so a 1.3KB
# image needs ~13M. Give runs with an image more room.
CYCLES  ?= $(if $(IMG),30000000,2000000)
# The sim holds IMG until the CPU prints this: bytes sent before the bootloader
# polls RX overflow the UART's 4-byte FIFO and are lost
BOOT_PROMPT := bootloader

all: boot.asmbin uart_app.img

boot.elf: start.S boot.c boot.lds ../mmio.h
	$(CC) $(CFLAGS) -T boot.lds -Wl,--no-warn-rwx-segments -o $@ start.S boot.c

boot.asmbin: boot.elf
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< $@

boot.dis: boot.elf
	$(OBJDUMP) -d $< > $@

# ---- Programs loaded by the bootloader: linked at 0x10000 with app.lds ----
APP_CFLAGS = -O2 -Wall -fno-builtin -march=rv32i_zicsr -mabi=ilp32

app_init.o: ../init.S
	$(CROSS_COMPILE)as -R -march=rv32i_zicsr -mabi=ilp32 -o $@ $<

uart_app.elf: ../uart.c ../mmio.h app_init.o app.lds
	$(CC) $(APP_CFLAGS) -c -o uart_app.o ../uart.c
	$(CROSS_COMPILE)ld -o $@ -T app.lds --oformat=elf32-littleriscv uart_app.o app_init.o

# Standalone (own _start, no init.o)
page_fault_app.elf: ../page_fault_test.S app.lds
	$(CC) $(APP_CFLAGS) -c -o page_fault_app.o ../page_fault_test.S
	$(CROSS_COMPILE)ld -o $@ -T app.lds --oformat=elf32-littleriscv page_fault_app.o

%.bin: %.elf
	$(OBJCOPY) -O binary -j .text -j .data $< $@

%.img: %.bin mkimg.py
	python3 mkimg.py $< $@

run-app: boot.asmbin uart_app.img
	$(MAKE) run IMG=uart_app.img

boot-test: boot.asmbin uart_app.img
	python3 boot_test.py $(VTOP) boot.asmbin uart_app.img

run: boot.asmbin
	$(VTOP) -i boot.asmbin --headless --max-cycles $(CYCLES) \
		$(if $(IMG),--uart-in $(IMG) --uart-in-wait "$(BOOT_PROMPT)",)

clean:
	rm -f boot.elf boot.asmbin boot.dis *.o *_app.elf *_app.bin *_app.img

.PHONY: all run run-app boot-test clean
