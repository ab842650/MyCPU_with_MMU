#!/usr/bin/env python3
"""Pack a raw binary into an image the MyCPU UART bootloader accepts.

Image layout (all fields little-endian uint32, same order as struct boot_header
in boot.c):

    magic | load_addr | entry | length | checksum | payload (length bytes)

checksum is the sum of all payload bytes, modulo 2^32.

Usage:
    mkimg.py app.bin app.img                  # load and entry at 0x10000
    mkimg.py app.bin app.img --load 0x20000   # entry defaults to load
"""

import argparse
import struct

BOOT_MAGIC = 0x5550434D  # "MCPU"


def checksum(data: bytes) -> int:
    return sum(data) & 0xFFFFFFFF


def main():
    p = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    p.add_argument("input", help="raw binary (objcopy -O binary)")
    p.add_argument("output", help="image to write")
    p.add_argument("--load", type=lambda s: int(s, 0), default=0x10000,
                   help="load address (default 0x10000)")
    p.add_argument("--entry", type=lambda s: int(s, 0), default=None,
                   help="entry address (default: same as --load)")
    args = p.parse_args()

    entry = args.load if args.entry is None else args.entry
    with open(args.input, "rb") as f:
        payload = f.read()

    header = struct.pack("<5I", BOOT_MAGIC, args.load, entry, len(payload),
                         checksum(payload))
    with open(args.output, "wb") as f:
        f.write(header + payload)

    print(f"{args.output}: load=0x{args.load:08x} entry=0x{entry:08x} "
          f"len=0x{len(payload):x} ({len(payload)} bytes) "
          f"sum=0x{checksum(payload):08x}")


if __name__ == "__main__":
    main()
