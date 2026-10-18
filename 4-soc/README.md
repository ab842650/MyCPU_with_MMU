# 4-soc: System-on-Chip with AXI4-Lite Bus

RISC-V RV32I processor with AXI4-Lite bus interface, Sv32 virtual memory, VGA, and UART peripherals.

## Features

- CPU: 5-stage pipelined RISC-V RV32I with forwarding and branch prediction
- Branch Prediction: BTB (32-entry) + RAS (4-entry) + IndirectBTB (8-entry) for reduced penalties
- Privilege: M and S modes, `medeleg` delegation, `mret`/`sret`, S-mode CSRs (`sstatus`, `stvec`, `sepc`, `scause`, `stval`, `sscratch`, `satp`)
- MMU: Sv32 with split I/D TLBs (2-way, 8 sets each, 4KB and 4MB pages), hardware page table walker, precise page faults, `sfence.vma`
- Bus: AXI4-Lite protocol with master/slave state machines
- Peripherals:
  - VGA: 640x480@72Hz with 64x64 framebuffer (6x scaling) and 16-color palette
  - UART: Buffered TX/RX at 115200 baud with status register
  - Memory: 2MB main memory (program loaded at 0x1000)

## Architecture

```
CPU (AXI4-Lite Master) + BTB (32-entry) + RAS (4-entry) + IndirectBTB (8-entry)
  │    MMU: ITLB + DTLB + PTW; the PTW shares the CPU's bus port with MEM
  └─> BusSwitch (Address decoder, bits[31:29], physical addresses)
       ├─> 0x0000_0000: Main Memory (2MB)
       ├─> 0x2000_0000: VGA Controller
       │    ├─> 0x00: ID (RO) - 0x56474131 "VGA1"
       │    ├─> 0x04: STATUS (RO) - vblank, safe_to_swap, curr_frame
       │    ├─> 0x08: INTR_STATUS (W1C) - vblank interrupt
       │    ├─> 0x10: UPLOAD_ADDR (RW) - framebuffer address
       │    ├─> 0x14: STREAM_DATA (WO) - pixel data (auto-increment)
       │    ├─> 0x20: CTRL (RW) - enable, blank, swap, frame_sel
       │    └─> 0x24-0x60: PALETTE[0-15] (RW) - 6-bit RRGGBB
       └─> 0x4000_0000: UART Controller
            ├─> 0x00: STATUS (RO) - bit0=TX ready, bit1=RX valid
            ├─> 0x04: BAUD_RATE (RO) - 115200
            ├─> 0x08: INTERRUPT (WO) - write non-zero to set, zero to clear
            ├─> 0x0C: RX_DATA (RO) - received byte (read clears interrupt)
            └─> 0x10: TX_DATA (WO) - transmit byte
```

## Build & Test

```bash
# Run ChiselTest suite (from project root)
sbt "project soc" test

# Or use make targets (from 4-soc/)
make test

# Generate Verilog and build Verilator simulator
make verilator

# Run VGA test (nyancat demo with SDL2 display)
make check-vga

# Run UART loopback test (no window)
make check-uart

# Run the Sv32 page fault test on Verilator (optionally dump a waveform)
make check-pagefault
make check-pagefault VCD=page_fault_test.vcd

# Cycle-by-cycle checks of every trap in that waveform
python3 scripts/check_pagefault_vcd.py page_fault_test.vcd

# Interactive MyCPU shell (type 'help' for commands)
make shell

# Run simulation with custom binary
make sim BINARY=csrc/nyancat.asmbin

# Run RISCOF compliance tests (RV32I + Zicsr: 119/119 pass)
make compliance

# Stop a simulation after N cycles (e.g. to keep a VCD small)
verilog/verilator/obj_dir/VTop -i csrc/uart.asmbin --headless --max-cycles 100000

# Format code
make indent

# Clean build artifacts
make clean
```

## Virtual Memory (Sv32 MMU)

Translation is active when `satp.MODE = 1` and the hart is not in M-mode
(M-mode accesses are never translated; `mstatus.MPRV` is not implemented).

### TLBs

- ITLB and DTLB: 2-way set-associative, 8 sets, round-robin replacement
- 4KB entries are indexed by VA[14:12]; 4MB superpage entries by VA[24:22]
- Each entry caches the outcome of its walk, so a TLB hit needs no walk:
  - Translation (PPN) and page size
  - Permissions: ITLB `x` (X && A), DTLB `r` (R && A) and `w` (W && A && D)
  - `fault`: a negative entry for an invalid PTE. Every access to that page (4KB) or
    region (4MB) faults until the entry is flushed
- `sfence.vma` (any rs1/rs2) flushes both TLBs entirely when it reaches WB

### Page Table Walker

State machine in `MMU.scala` (`state` in waveforms):

| Value | State | Action |
|-------|-------|--------|
| 0 | `sIdle` | No walk |
| 1 | `sL1Req` | Read L1 PTE at `satp.PPN * 4096 + VPN[1] * 4` |
| 2 | `sL1Wait` | Invalid PTE or misaligned superpage -> `sFault` (4MB); leaf -> `sLeaf` (4MB); pointer -> `sL0Req` |
| 3 | `sL0Req` | Read L0 PTE at `PTE.PPN * 4096 + VPN[0] * 4` |
| 4 | `sL0Wait` | Invalid or non-leaf PTE -> `sFault` (4KB); leaf -> `sLeaf` (4KB) |
| 5 | `sLeaf` | Valid leaf: write translation and permissions into the ITLB/DTLB, return to idle |
| 6 | `sFault` | Invalid PTE: write a fault entry into the ITLB/DTLB, return to idle |

- I-side misses are walked before D-side misses
- The pipeline stalls for the whole walk
- A walk only starts while MEM has no bus transaction in flight, since the PTW and MEM
  share the bus port

### Page Faults

Faults are raised from TLB hits (fault entry or insufficient permission), one cycle
after the walk that filled the entry. They are precise and can be delegated to S-mode
through `medeleg` bits 12/13/15.

| Fault | Cause | Taken in | `xepc` | `xtval` |
|-------|-------|----------|--------|---------|
| Instruction page fault | 12 | ID (the fetch becomes a fault-tagged NOP) | faulting VA | faulting VA |
| Load page fault | 13 | MEM | load PC | data VA |
| Store page fault | 15 | MEM | store PC | data VA |

On a load/store page fault the faulting access issues no bus request and does not write
back. In that cycle IF2ID, ID2EX, and EX2MEM are flushed, a younger CSR write in EX is
dropped, and the next PC is `stvec`/`mtvec`.

A/D bits follow Svade: the MMU never writes PTEs. An access to a page with A=0, or a
store to a page with D=0, raises a page fault and software sets the bits.

Because invalid PTEs are cached as fault entries, a handler that fixes a PTE must run
`sfence.vma` before returning. Otherwise the retry hits the cached fault entry and faults
again. The privileged spec permits this behavior.

### Tests

- `src/test/scala/riscv/MMUTest.scala`: MMU unit tests (walks, superpages, permissions,
  fault entries, `sfence.vma`)
- `csrc/page_fault_test.S` (`PageFaultTest`, `make check-pagefault`): S-mode program that
  takes load, store, and instruction page faults, checks squashing of younger
  instructions, and fixes a PTE and retries. It reports pass bits through the
  `sim.cpp` harness (`0xF` = pass)
- `csrc/rv32emu_mmu/`: runs rv32emu's `tests/system/mmu` demand-paging test
  (unmodified) with a small M-mode firmware for its `write`/`exit` syscalls:
  ```bash
  cd csrc/rv32emu_mmu
  make fetch   # sparse-clone rv32emu's test sources
  make run
  ```
  Current status:

  | Variant | Result |
  |---|---|
  | Unmodified test | Fails: `assert(freelist_head)` before the first test prints |
  | Handler patched to always set PTE.D | All 4 tests pass (fetch, load, store, redirected faults), `exit(0)`, ~441M cycles |

  The unmodified test fails because its fault handler maps a page with D=0 and then
  `memcpy`s into it. rv32emu sets D in hardware (Svadu). Here (Svade) that store faults
  again inside the handler, and the handler allocates a new frame on every retry until
  the free list runs out. The one-line patch does what Svadu hardware would do.

  The pass also depends on two gaps, so it is not full conformance:
  - The "user" code runs in S-mode (no U-mode), and PTE.U / SUM are not checked. A
    spec-compliant S-mode could not execute those U pages
  - The firmware does not add 4 to `mepc` after `ecall`, because the CLINT already
    records the next instruction (see Known Limitations)

  Running this test found two pipeline bugs, both now fixed: a jump taken just before a
  stall was taken again after it (an instruction fetched twice), and a Control stall
  without a bubble copied the ID instruction into EX (`auipc ra; jalr ra,off(ra)`
  jumped to the wrong target)

### Known Limitations

- No U-mode: `sret` always returns to S-mode, and PTE.U / `sstatus.SUM` / `sstatus.MXR`
  are not checked
- No hardware A/D update (Svadu). rv32emu's MMU test needs it (see Tests)
- `ecall` writes the address of the next instruction to `mepc`/`sepc`, not the `ecall`
  itself, so handlers must not add 4
- No ASIDs; `sfence.vma` always flushes the whole TLB
- If the TLB is flushed or `satp` changes while a load/store is waiting on the bus, the
  access is re-issued after the walk. This is harmless for RAM but repeats MMIO side effects

## AXI4-Lite Transaction Flow

### Read Transaction
1. Master asserts `ARVALID` + address
2. Slave asserts `ARREADY` (handshake)
3. Slave provides `RVALID` + `RDATA`
4. Master asserts `RREADY` (handshake)

### Write Transaction
1. Master asserts `AWVALID` + address
2. Slave asserts `AWREADY` (handshake)
3. Master asserts `WVALID` + `WDATA` + `WSTRB`
4. Slave asserts `WREADY` (handshake)
5. Slave asserts `BVALID` + `BRESP`
6. Master asserts `BREADY` (handshake)

## Branch Prediction

### Branch Target Buffer (BTB)

32-entry direct-mapped cache with 2-bit saturating counter:

- Indexing: PC[6:2] selects entry, PC[31:7] as tag
- Counter states: SNT(0) → WNT(1) → WT(2) → ST(3)
- Prediction: Taken when counter >= 2 (WT or ST)
- Allocation: Only taken branches allocate; not-taken never pollutes BTB
- New entries: Initialize to WT(2) for quick loop adaptation
- Power: Operand-gated tag compare reduces switching on invalid entries

Pipeline integration:
- IF stage: Combinational lookup for same-cycle prediction
- ID stage: Registered update when branch resolves

Misprediction handling:
1. Predicted taken but not taken → Redirect to PC+4, decrement counter
2. Hit but wrong target → Redirect to correct target, update entry
3. Miss on taken branch → Allocate new entry with WT counter

### Return Address Stack (RAS)

4-entry stack for JALR return prediction (register-computed targets):

- Push: JAL with rd=ra/t0 pushes PC+4 (function call)
- Pop: JALR with rs1=ra/t0, rd=x0 pops predicted return address
- Overflow: Shift stack down, oldest entry lost
- Underflow: Stack pointer saturates at 0, output marked invalid
- Tail call: Simultaneous push+pop replaces top-of-stack
- Recovery: Restore interface for misprediction rollback

### Indirect Branch Target Buffer (IndirectBTB)

8-entry fully-associative table for non-return JALR prediction:

- Use cases: Function pointers, vtables, computed jumps, switch tables
- Entry format: valid, PC tag, rs1_hash (8-bit XOR-fold), target, age counter
- Lookup: PC-only matching in IF stage (rs1 not yet available)
- Update: Full (PC, rs1_hash) matching in ID stage when JALR resolves
- Replacement: LRU-approximated via age counters
- Complements RAS: Handles JALR patterns that are not function returns

Prediction priority (highest to lowest):
1. RAS - for return patterns (JALR rs1=ra/t0, rd=x0)
2. IndirectBTB - for other JALR (function pointers, vtables)
3. BTB - fallback for branches and direct jumps

## Design Notes

- AXI4-Lite replaces direct memory connections with standardized bus protocol
- Bus switch uses address bits [31:29] to route transactions (supports 8 slaves)
- VGA uses dual-clock CDC (system clock + 31.5 MHz pixel clock)
- VGA framebuffer: 12 frames x 64x64 pixels, 4-bit palette indices (8 pixels/word)
- VGA scaling: Fixed-point multiply-shift for 6x zoom (10923/65536 ≈ 1/6)
- All peripherals implement AXI4-Lite slave interface for uniformity
- MemoryAccess uses latched control signals to handle pipeline stall release timing
- CPU.scala latches full bus address during transactions for stable routing
- CLINT takes ID-stage events (xret, ecall/ebreak, instruction page fault, interrupts)
  only in the cycle the ID instruction advances, so CSR/privilege updates happen exactly
  once and after older instructions have finished their bus accesses
- Verilator harness (`verilog/verilator/sim.cpp`): 16MB memory, 500M-cycle limit in
  batch mode; a write of `0xCAFEF00D` to `0x100` ends the run with the result at `0x104`

## VGA Display Details

- Resolution: 640x480 @ 72Hz (H_TOTAL=832, V_TOTAL=520)
- Framebuffer: 64x64 pixels, centered and scaled 6x to 384x384 display area
- Colors: 16-entry palette, each entry 6-bit RRGGBB (2 bits per channel)
- Double buffering: 12 frames available, software selects via CTRL register
- Vblank interrupt: Edge-triggered, write-1-to-clear acknowledge

## MyCPU Shell

Interactive bare-metal shell for RISC-V processor inspection and debugging.

### Commands

| Command | Description |
|---------|-------------|
| `help`, `?` | Show available commands |
| `info` | Display CPU architecture (MISA) and memory map |
| `csr` | Show CSR register values (mstatus, misa, mvendorid, etc.) |
| `mem <addr>` | Read memory at address (e.g., `mem 0x20000000`) |
| `memw <addr> <val>` | Write value to memory (e.g., `memw 0x20000020 0x01`) |
| `perf` | Show performance counters (mcycle, minstret, CPI) |
| `clear` | Clear terminal screen |
| `reboot` | Software reset (jump to reset vector) |

### Line Editing

- Backspace: Delete character before cursor
- Ctrl-C: Exit shell (host terminal)

### Example Session

```
MyCPU Shell - Type 'help' for commands
MyCPU> info
MyCPU RISC-V Processor
----------------------
Architecture: RV32I
Vendor ID:    0x00000000
...

MyCPU> perf
Performance Counters:
  mcycle:   1234567
  minstret: 456789
  CPI:      2.70
```

## References

- VGA peripheral adapted from 2-mmio-trap with AXI4-Lite interface
- CPU core from 3-pipeline (5-stage with forwarding)
- Pixel clock division: 31.5 MHz (simulated at 1/4 system clock in Verilator)
