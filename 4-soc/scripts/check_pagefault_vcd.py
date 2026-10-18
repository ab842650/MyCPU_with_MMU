#!/usr/bin/env python3
"""Cycle-by-cycle checks of Sv32 page-fault traps in a Verilator VCD.

Usage: python3 scripts/check_pagefault_vcd.py page_fault_test.vcd

Generate the VCD with:  make check-pagefault VCD=page_fault_test.vcd

Signals are sampled right before every rising edge of the CPU clock (the value the
flops capture). For every trap it checks the CSR update, the redirect to stvec, the
pipeline squash, and whether a page walk preceded the fault (TLB fault-entry hits
must not walk).
"""
import sys

SIGS = {
    "clk": "cpu.cpu.clock",
    "pc": "cpu.cpu.inst_fetch_io_pc_va",
    "trap_flag": "cpu.cpu.inst_fetch_io_trap_flag",
    "irq_assert": "cpu.cpu.clint_io_id_interrupt_assert",
    "handler": "cpu.cpu.clint_io_id_interrupt_handler_address",
    "stvec": "cpu.cpu.clint_io_csr_bundle_stvec",
    "scause": "cpu.cpu.clint_io_csr_bundle_scause",
    "sepc": "cpu.cpu.clint_io_csr_bundle_sepc",
    "stval": "cpu.cpu.clint_io_csr_bundle_stval",
    "we_m": "cpu.cpu.clint_io_csr_bundle_direct_write_enable",
    "we_s": "cpu.cpu.clint_io_csr_bundle_direct_write_enable_s",
    "scause_w": "cpu.cpu.clint_io_csr_bundle_scause_write_data",
    "sepc_w": "cpu.cpu.clint_io_csr_bundle_sepc_write_data",
    "stval_w": "cpu.cpu.clint_io_csr_bundle_stval_write_data",
    "if_pf": "cpu.cpu.clint_io_if_page_fault",
    "id_pc": "cpu.cpu.if2id_io_output_instruction_address",
    "csr_we_ex": "cpu.cpu.csr_regs_io_reg_write_enable_ex",
    "f_if2id": "cpu.cpu.if2id_io_flush",
    "f_id2ex": "cpu.cpu.id2ex_io_flush",
    "f_ex2mem": "cpu.cpu.ex2mem_io_flush",
    "mem_pc": "cpu.cpu.ex2mem_io_output_instruction_address",
    "mem_store": "cpu.cpu.ex2mem_io_output_memory_write_enable",
    # firtool renames it to bus__request when another io_bus_* port exists
    "bus_req": ("cpu.cpu.mem_io_bus_request", "cpu.cpu.mem_io_bus__request"),
    "wb_we": "cpu.cpu.mem_io_wb_regs_write_enable",
    "d_va": "cpu.cpu.mem_io_d_va",
    "mem_stall": "cpu.cpu.mem_stall",
    "ptw_stall": "cpu.cpu.mmu_io_ptw_stall",
    "d_fault": "cpu.mmu.io_d_fault",
    "i_fault": "cpu.mmu.io_i_fault",
    "state": "cpu.mmu.state",
    "ptw_req": "cpu.mmu.io_ptw_req_valid",
    "tlb_flush": "cpu.mmu.io_tlb_flush",
}


def load(path):
    want = {}
    for k, v in SIGS.items():
        for name in (v if isinstance(v, tuple) else (v,)):
            want[name] = k
    idmap, hist, scope = {}, {k: [] for k in SIGS}, []
    t, hdr = 0, True
    with open(path) as f:
        for line in f:
            if hdr:
                p = line.split()
                if not p:
                    continue
                if p[0] == "$scope":
                    scope.append(p[2])
                elif p[0] == "$upscope":
                    scope.pop()
                elif p[0] == "$var" and len(scope) >= 2:
                    key = want.get(scope[-2] + "." + scope[-1] + "." + p[4])
                    if key and key not in idmap.values():
                        idmap.setdefault(p[3], []).append(key)
                elif p[0] == "$enddefinitions":
                    hdr = False
                continue
            c = line[0]
            if c == "#":
                t = int(line[1:])
                continue
            if c == "b":
                v, i = line[1:].split()
            elif c in "01xz":
                v, i = c, line[1:].strip()
            else:
                continue
            for k in idmap.get(i, ()):
                hist[k].append((t, int(v, 2) if set(v) <= set("01") else 0))
    missing = [str(SIGS[k]) for k in SIGS if not hist[k]]
    if missing:
        sys.exit("missing signals in VCD: " + ", ".join(missing))
    return hist


def sample(hist):
    """One dict per CPU cycle: values right before each rising clock edge."""
    edges = [t for (t, v), (_, pv) in zip(hist["clk"][1:], hist["clk"]) if v == 1 and pv == 0]
    cycles, ptr, cur = [], {k: 0 for k in hist}, {k: 0 for k in hist}
    for e in edges:
        for k, h in hist.items():
            i = ptr[k]
            while i < len(h) and h[i][0] < e:
                cur[k] = h[i][1]
                i += 1
            ptr[k] = i
        row = dict(cur)
        row["t"] = e
        cycles.append(row)
    return cycles


class Report:
    def __init__(self):
        self.fails = 0

    def check(self, ok, msg):
        print(("  ok   " if ok else "  FAIL ") + msg)
        if not ok:
            self.fails += 1


def main(path):
    cyc = sample(load(path))
    r = Report()
    n = len(cyc)
    print(f"{path}: {n} CPU cycles\n")

    # ---------------- D-side traps (MEM stage) ----------------
    d_traps = [i for i in range(n) if cyc[i]["d_fault"]]
    print(f"== D-side page faults: {len(d_traps)}")
    for i in d_traps:
        c, nx = cyc[i], cyc[i + 1]
        cause = 15 if c["mem_store"] else 13
        # walk cycles while this same instruction waited in MEM
        j, walk = i - 1, 0
        while j >= 0 and cyc[j]["mem_pc"] == c["mem_pc"] and cyc[j]["mem_stall"]:
            walk += cyc[j]["state"] != 0
            j -= 1
        kind = f"after a {walk}-cycle page walk" if walk else "TLB fault-entry/permission hit, no walk"
        print(f" t={c['t']} pc=0x{c['mem_pc']:x} {'store' if cause == 15 else 'load'} "
              f"va=0x{c['d_va']:x}: {kind}")
        r.check(c["trap_flag"] and not c["mem_stall"], "trap taken in an advancing cycle (trap_flag, no mem_stall)")
        r.check(not c["bus_req"], "faulting access issues no bus request")
        r.check(not c["wb_we"], "faulting access does not write back")
        r.check(c["f_if2id"] and c["f_id2ex"] and c["f_ex2mem"], "IF2ID/ID2EX/EX2MEM flushed (younger instrs squashed)")
        r.check(not c["csr_we_ex"], "younger CSR write in EX suppressed")
        r.check(c["we_s"] and not c["we_m"], "delegated: S-mode CSRs written, M-mode not")
        r.check(c["scause_w"] == cause, f"scause <- {cause} (got {c['scause_w']})")
        r.check(c["sepc_w"] == c["mem_pc"], f"sepc <- 0x{c['mem_pc']:x} (got 0x{c['sepc_w']:x})")
        r.check(c["stval_w"] == c["d_va"], f"stval <- 0x{c['d_va']:x} (got 0x{c['stval_w']:x})")
        r.check((nx["scause"], nx["sepc"], nx["stval"]) == (c["scause_w"], c["sepc_w"], c["stval_w"]),
                "CSRs hold the new values next cycle")
        r.check(c["handler"] == c["stvec"] and nx["pc"] == c["stvec"],
                f"next PC = stvec 0x{c['stvec']:x} (got 0x{nx['pc']:x})")

    # ---------------- I-side traps (taken in ID) ----------------
    i_takes = [i for i in range(n) if cyc[i]["if_pf"] and cyc[i]["irq_assert"]]
    print(f"\n== I-side page faults taken in ID: {len(i_takes)}")
    for i in i_takes:
        c, nx = cyc[i], cyc[i + 1]
        print(f" t={c['t']} va=0x{c['id_pc']:x}")
        r.check(not c["mem_stall"], "taken once, in an advancing cycle (no mem_stall)")
        r.check(c["we_s"] and c["scause_w"] == 12, f"scause <- 12 (got {c['scause_w']})")
        r.check(c["sepc_w"] == c["id_pc"] and c["stval_w"] == c["id_pc"],
                f"sepc = stval = 0x{c['id_pc']:x} (got 0x{c['sepc_w']:x}, 0x{c['stval_w']:x})")
        r.check(nx["pc"] == c["stvec"], f"next PC = stvec 0x{c['stvec']:x} (got 0x{nx['pc']:x})")
        # while i_fault is held, the PTW must not re-walk
        j, walk, held = i, 0, 0
        while j >= 0 and cyc[j]["i_fault"] or (j == i):
            walk += cyc[j]["state"] != 0
            held += 1
            j -= 1
        r.check(walk == 0, f"i_fault held {held} cycles with no re-walk")

    # ---------------- global invariants ----------------
    print("\n== Global")
    r.check(all(c["trap_flag"] == c["d_fault"] for c in cyc), "trap_flag asserted exactly on d_fault cycles")
    stray = [c["t"] for c in cyc if c["we_s"] and c["if_pf"] and c["mem_stall"]]
    r.check(not stray, "no repeated ID-stage page-fault CSR writes during stalls")
    walks = sum(1 for a, b in zip(cyc, cyc[1:]) if a["state"] == 0 and b["state"] == 1)
    flushes = sum(1 for c in cyc if c["tlb_flush"])
    print(f"  info page walks started: {walks}, tlb_flush cycles: {flushes}")

    # longest stalls and what caused them
    runs, start = [], None
    for i, c in enumerate(cyc + [{"mem_stall": 0}]):
        if c["mem_stall"] and start is None:
            start = i
        elif not c["mem_stall"] and start is not None:
            seg = cyc[start:i]
            runs.append((len(seg), seg[0]["t"], sum(s["state"] != 0 for s in seg)))
            start = None
    runs.sort(reverse=True)
    longest = runs[0][0] if runs else 0
    print("  info longest mem_stall runs (cycles, start t, of which PTW cycles):", runs[:5])
    r.check(longest < 100, f"no stall run >= 100 cycles (longest {longest})")

    print(f"\n{'PASS' if r.fails == 0 else 'FAIL'}: {r.fails} failed check(s)")
    return 1 if r.fails else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else "page_fault_test.vcd"))
