#!/usr/bin/env python3
"""Build a MyCPU flat image (loaded at PA 0x1000) from rv32emu's vm.elf.

usage: mkimage.py vm.bin fw.bin out.asmbin
  vm.bin : objcopy -O binary vm.elf (starts at PA 0x0)
  fw.bin : firmware linked at FW_BASE (see fw.S)
"""
import struct
import sys

LOAD_BASE = 0x1000   # sim.cpp loads the image here; CPU reset PC
FW_BASE = 0x100000
LO_LEN = 0x1008      # PA 0x0.. restored by the firmware (must match fw.S)

vm = bytearray(open(sys.argv[1], "rb").read())
fw = open(sys.argv[2], "rb").read()

if len(vm) < LO_LEN:
    vm += bytes(LO_LEN - len(vm))
if FW_BASE < LO_LEN or any(vm[FW_BASE:FW_BASE + len(fw)]):
    sys.exit("firmware would overlap vm.elf contents")

img = vm + bytes(max(0, FW_BASE + len(fw) - len(vm)))
img[FW_BASE:FW_BASE + len(fw)] = fw

# boot stub at the reset PC: lui t0, FW_BASE>>12 ; jr t0
lui = ((FW_BASE >> 12) << 12) | (5 << 7) | 0x37
jr = (5 << 15) | 0x67
img[LOAD_BASE:LOAD_BASE + 8] = struct.pack("<II", lui, jr)

open(sys.argv[3], "wb").write(img[LOAD_BASE:])
print(f"{sys.argv[3]}: {len(img) - LOAD_BASE} bytes (PA 0x{LOAD_BASE:x}..0x{len(img):x})")
