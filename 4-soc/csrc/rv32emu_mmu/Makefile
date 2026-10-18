# Run rv32emu's tests/system/mmu (vm.elf) on MyCPU's Verilator build.
#
#   make fetch   # sparse-clone rv32emu's tests/system/mmu into ./rv32emu
#   make         # build vm.elf (unmodified) + firmware -> vm.asmbin
#   make run     # run on ../../verilog/verilator/obj_dir/VTop (VCD=x.vcd to dump)
#
# See fw.S for how the image is laid out and how syscalls are serviced.

CROSS_COMPILE ?= $(HOME)/riscv-none-elf-gcc/bin/riscv-none-elf-
RV32EMU       ?= rv32emu
MMU_TEST      := $(RV32EMU)/tests/system/mmu
VTOP          := ../../verilog/verilator/obj_dir/VTop

all: vm.asmbin

fetch:
	git clone --depth 1 --filter=blob:none --sparse https://github.com/sysprog21/rv32emu.git $(RV32EMU)
	cd $(RV32EMU) && git sparse-checkout set tests/system/mmu

$(MMU_TEST)/vm.elf:
	$(MAKE) -C $(MMU_TEST) PREFIX=$(CROSS_COMPILE)

vm.bin: $(MMU_TEST)/vm.elf
	$(CROSS_COMPILE)objcopy -O binary $< $@

saved_lo.bin: vm.bin
	head -c $$((0x1008)) $< > $@

fw.bin: fw.S saved_lo.bin
	$(CROSS_COMPILE)gcc -march=rv32i_zicsr -mabi=ilp32 -nostdlib -Wl,-Ttext=0x100000,-e,fw_start -o fw.elf fw.S
	$(CROSS_COMPILE)objcopy -O binary fw.elf $@

vm.asmbin: vm.bin fw.bin mkimage.py
	python3 mkimage.py vm.bin fw.bin $@

run: vm.asmbin
	$(VTOP) -i vm.asmbin --headless $(if $(VCD),--vcd $(VCD),)

clean:
	rm -f vm.bin saved_lo.bin fw.elf fw.bin vm.asmbin

.PHONY: all fetch run clean
