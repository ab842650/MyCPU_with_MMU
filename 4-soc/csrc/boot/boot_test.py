#!/usr/bin/env python3
"""Integration test for the UART bootloader.

Takes a good image (made by mkimg.py), derives broken variants from it, sends
each one to the bootloader on the Verilator sim, and checks what it prints:
the good image must boot and pass its own test, every broken one must be
rejected with the expected error.

Usage: boot_test.py <VTop> <boot.asmbin> <good.img>
"""

import os
import struct
import subprocess
import sys
import tempfile

HEADER = struct.Struct("<5I")  # magic, load_addr, entry, length, checksum

# Sim cycles are half clock cycles; one UART byte is ~9.5K of them. A header
# error shows up after ~20 bytes, a checksum error only after the whole payload.
SHORT = 3_000_000
LONG = 30_000_000


def with_header(img, **fields):
    """Copy of img with some header fields replaced (payload untouched)."""
    names = ("magic", "load_addr", "entry", "length", "checksum")
    h = dict(zip(names, HEADER.unpack_from(img)))
    h.update(fields)
    return HEADER.pack(*(h[n] for n in names)) + img[HEADER.size:]


def flip_payload_bit(img):
    b = bytearray(img)
    b[HEADER.size + 100] ^= 0x01
    return bytes(b)


def main():
    vtop, boot, good_path = sys.argv[1:4]
    with open(good_path, "rb") as f:
        good = f.read()
    _, load, entry, _, _ = HEADER.unpack_from(good)

    # (name, image, expected output, cycle budget)
    cases = [
        ("good image boots", good, "TEST PASSED", LONG),
        ("bad magic", with_header(good, magic=0x12345678), "ERR: bad magic", SHORT),
        ("bad checksum", flip_payload_bit(good), "ERR: bad checksum", LONG),
        ("load over bootloader", with_header(good, load_addr=0x1000, entry=0x1000),
         "ERR: bad load_addr", SHORT),
        ("length overflow", with_header(good, length=0xFFFFFFF0),
         "ERR: payload out of range", SHORT),
        ("entry past payload", with_header(good, entry=load + (len(good) - HEADER.size)),
         "ERR: entry out of range", SHORT),
        ("entry misaligned", with_header(good, entry=entry + 2),
         "ERR: entry not aligned", SHORT),
    ]

    with tempfile.TemporaryDirectory() as tmp:
        procs = []
        for i, (name, img, expect, cycles) in enumerate(cases):
            path = os.path.join(tmp, f"case{i}.img")
            with open(path, "wb") as f:
                f.write(img)
            cmd = [vtop, "-i", boot, "--headless", "--max-cycles", str(cycles),
                   "--uart-in", path, "--uart-in-wait", "bootloader"]
            procs.append(subprocess.Popen(cmd, stdout=subprocess.PIPE,
                                          stderr=subprocess.STDOUT))

        failed = 0
        for (name, _, expect, _), p in zip(cases, procs):
            out = p.communicate()[0].decode("latin-1")
            ok = expect in out
            failed += not ok
            print(f"  {'PASS' if ok else 'FAIL'}  {name:<22} (expect '{expect}')")
            if not ok:
                print("        output:", " | ".join(out.strip().splitlines()[-4:]))

    print(f"{len(cases) - failed}/{len(cases)} passed")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
