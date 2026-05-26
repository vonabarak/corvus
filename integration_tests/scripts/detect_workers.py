"""Pick a pytest-xdist worker count from host CPU + memory budget.

The integration suite boots one or more nested VMs per worker, so the
default `pytest-xdist -n auto` (one worker per logical CPU) routinely
oversubscribes RAM and OOMs the host. This detector applies the smaller
of two budgets: one worker per 2 logical CPUs, one worker per 3 GiB of
MemAvailable. Linux-only — same constraint the suite already has.

Stdout is always a single integer (the worker count) so it can be
substituted into a `pytest -n …` invocation. `--explain` adds a
one-line breakdown on stderr; the integer on stdout is unchanged.
"""

from __future__ import annotations

import argparse
import os
import sys
from pathlib import Path

CPUS_PER_WORKER = 2
GIB_PER_WORKER = 3
MEMINFO_PATH = Path("/proc/meminfo")


def _logical_cpus() -> int:
    override = os.environ.get("CORVUS_TEST_CPU_COUNT")
    if override is not None:
        return max(1, int(override))
    return os.cpu_count() or 1


def _memavailable_kib() -> int:
    override = os.environ.get("CORVUS_TEST_MEMAVAILABLE_KB")
    if override is not None:
        return max(0, int(override))
    for line in MEMINFO_PATH.read_text().splitlines():
        if line.startswith("MemAvailable:"):
            return int(line.split()[1])
    raise RuntimeError(f"MemAvailable: line not found in {MEMINFO_PATH}")


def compute_workers() -> tuple[int, int, int, int, int]:
    cpus = _logical_cpus()
    mem_kib = _memavailable_kib()
    cpu_budget = max(1, cpus // CPUS_PER_WORKER)
    mem_budget = max(1, mem_kib // (GIB_PER_WORKER * 1024 * 1024))
    workers = max(1, min(cpu_budget, mem_budget))
    return workers, cpu_budget, mem_budget, cpus, mem_kib


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--explain",
        action="store_true",
        help="Print a diagnostic breakdown on stderr.",
    )
    args = parser.parse_args()

    workers, cpu_budget, mem_budget, cpus, mem_kib = compute_workers()
    if args.explain:
        mem_gib = mem_kib / (1024 * 1024)
        print(
            f"workers={workers} cpu_budget={cpu_budget} mem_budget={mem_budget} "
            f"logical_cpus={cpus} mem_available_gib={mem_gib:.1f}",
            file=sys.stderr,
        )
    print(workers)
    return 0


if __name__ == "__main__":
    sys.exit(main())
