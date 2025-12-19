# 4-soc: System-on-Chip with AXI4-Lite Bus

RISC-V RV32I processor with AXI4-Lite bus interface, VGA, and UART peripherals.

## Features

- CPU: 5-stage pipelined RISC-V RV32I with forwarding and BTB
- Branch Prediction: 16-entry Branch Target Buffer for reduced branch penalty
- Bus: AXI4-Lite protocol with master/slave state machines
- Peripherals:
  - VGA: 640x480@72Hz with framebuffer and palette
  - UART: Buffered TX/RX with status register and interrupt support
  - Timer: System timer with interrupt
  - Memory: 32KB SRAM

## Architecture

```
CPU (AXI4-Lite Master) + BTB (16-entry)
  └─> BusSwitch (Address decoder)
       ├─> 0x0000_0000: Main Memory (32KB)
       ├─> 0x4000_0000: UART
       │    ├─> 0x00: STATUS (RO) - bit0=TX ready, bit1=RX valid
       │    ├─> 0x04: BAUD_RATE (RO)
       │    ├─> 0x08: INTERRUPT (WO)
       │    ├─> 0x0C: RX_DATA (RO)
       │    └─> 0x10: TX_DATA (WO)
       └─> 0x2000_0000: VGA (ID=0x56474131 "VGA1")
```

## Build & Test

```bash
# Run ChiselTest suite (from project root)
sbt "project soc" test

# Or use make targets (from 4-soc/)
make test

# Generate Verilog and build Verilator simulator
make verilator

# Run simulation
make sim SIM_ARGS="-instruction fibonacci.asmbin"

# Run RISCOF compliance tests
make compliance

# Format code
make indent

# Clean build artifacts
make clean
```

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

## Branch Target Buffer (BTB)

The BTB provides static branch prediction to reduce branch penalties:

- Structure: 16-entry direct-mapped cache indexed by PC[5:2]
- Prediction: Predict taken on BTB hit (optimistic for loops)
- Update: ID stage updates BTB when branch/jump resolves
- Performance: Reduces branch penalty from 1 cycle to 0 for correct predictions

Misprediction handling:
1. BTB predicted taken but branch not taken: Redirect to sequential PC
2. BTB hit but wrong target: Redirect to correct target
3. Non-branch instruction hit BTB: Invalidate entry

## Design Notes

- AXI4-Lite replaces direct memory connections with standardized bus protocol
- Bus arbiter handles structural hazards (IF/MEM stage conflicts)
- VGA uses dual-clock CDC for proper synchronization between CPU and pixel clocks
- All peripherals implement AXI4-Lite slave interface for uniformity
- ROMLoader converts word indices to byte addresses for InstructionROM compatibility
- MemoryAccess uses latched control signals to handle pipeline stall release timing

## References

- VGA peripheral adapted from 2-mmio-trap with AXI4-Lite interface
- CPU core from 3-pipeline (5-stage with forwarding)
