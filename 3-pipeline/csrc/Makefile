CROSS_COMPILE ?= /Users/guxiaozheng/riscv-none-elf-gcc/bin/riscv-none-elf-

ASFLAGS = -march=rv32i_zicsr -mabi=ilp32
CFLAGS = -O0 -Wall -march=rv32i_zicsr -mabi=ilp32
LDFLAGS = --oformat=elf32-littleriscv

AS := $(CROSS_COMPILE)as
CC := $(CROSS_COMPILE)gcc
LD := $(CROSS_COMPILE)ld
OBJCOPY := $(CROSS_COMPILE)objcopy

%.o: %.S
	$(AS) -R $(ASFLAGS) -o $@ $<
%.elf: %.S
	$(AS) -R $(ASFLAGS) -o $(@:.elf=.o) $<
	$(CROSS_COMPILE)ld -o $@ -T link.lds $(LDFLAGS) $(@:.elf=.o)
%.elf: %.c init.o
	$(CC) $(CFLAGS) -c -o $(@:.elf=.o) $<
	$(CROSS_COMPILE)ld -o $@ -T link.lds $(LDFLAGS) $(@:.elf=.o) init.o


%.asmbin: %.elf
	$(OBJCOPY) -O binary -j .text -j .data $< $@

BINS = \
	fibonacci.asmbin \
	hazard.asmbin \
	quicksort.asmbin \
	sb.asmbin \
	uart.asmbin \
	irqtrap.asmbin\
	rsqrt_fast.asmbin

# Clear the .DEFAULT_GOAL special variable, so that the following turns
# to the first target after .DEFAULT_GOAL is not set.
.DEFAULT_GOAL :=

rsqrt_fast_asm.o: rsqrt_fast_asm.S
	$(AS) -R $(ASFLAGS) -o $@ $<

rsqrt_fast.elf: rsqrt_fast.o rsqrt_fast_asm.o init.o
	$(LD) -o $@ -T link.lds $(LDFLAGS) rsqrt_fast.o rsqrt_fast_asm.o init.o


all: $(BINS)

update: $(BINS)
	cp -f $(BINS) ../src/main/resources

clean:
	$(RM) *.o *.elf *.asmbin
