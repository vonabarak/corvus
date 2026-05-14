# Corvus integration tests

End-to-end test suite for Corvus that runs a **freshly compiled inner
Corvus daemon** inside a **nested VM** orchestrated by the developer's
existing outer Corvus install. Drives the inner daemon from the host
via the same-version `corvus_client` pycapnp client.

The outer daemon is **not** under test — it only spins up the VMs the
inner daemon lives in. Outer ≠ inner versions is expected.

## Why nested VMs

Several Corvus features can't be tested under the outer's unprivileged
user-namespace deployment:

- **rootful networking** — real TAP, real bridges, namespace-internal NAT
- **multi-node topologies** — two cooperating Corvus daemons
- **real KVM hardware behaviour** — boot timing, console replay, etc.

A nested VM gives us root, real KVM, and isolation.

## Prerequisites

- Outer Corvus installed (`make install` in the repo root)
- Nested KVM enabled on the host:
  ```
  cat /sys/module/kvm_intel/parameters/nested   # or kvm_amd
  Y
  ```
  Enable with the kernel parameter `kvm-intel.nested=1` (or `kvm-amd`).
- A built host binary: `stack build` (the harness reads
  `stack path --local-install-root`).
- The integration-test image, built once on first session run:
  `crv apply tests-integration/images/corvus-integration-test.yml --wait`
  takes 30-60 min cold (kernel + stage3 + emerges).

## Running

```
cd tests-integration
python -m venv .venv
.venv/bin/pip install -e ../python -e .
.venv/bin/pytest tests/test_vm_lifecycle.py -v
```

`pytest -m slow` opts into the heavy scenarios; `pytest -m rootful`
selects the new-feature tests. `-n 4` parallelises via pytest-xdist
(each worker namespaces its VMs).

## Architecture

```
Host                  outer Corvus           Inner test VM
  pytest -------> [crv CLI] ----------> creates VM (template
   |                                     "corvus-integration-test-vm")
   |  pycapnp                                 |
   |   |                                      | virtiofs:
   |   |       socat host TCP ↔ VSOCK          |   /opt/corvus/bin
   |   v                                      v
   |  127.0.0.1:<auto>  ←----- VSOCK -----  inner Corvus daemon
   |                          relay         (TCP 0.0.0.0:9876, root)
   |                                            └─ inner Postgres (uds)
```

The inner Corvus binary at `/opt/corvus/bin/corvus` is the **host's
freshly compiled stack output**, mounted read-only via virtiofs. Per
source change: `stack build` then re-run the tests; no image rebuild.

## File layout

```
tests-integration/
├── conftest.py                  # session/module/function fixtures
├── pyproject.toml
├── corvus_test_harness/         # building blocks the fixtures compose
│   ├── outer.py                 # `crv` CLI driver
│   ├── inner.py                 # pycapnp Client over VSOCK→TCP relay
│   ├── topology.py              # multi-VM declarative scenarios
│   ├── images.py                # `crv apply` the test YAML (idempotent)
│   ├── host_binary.py           # find + freshness-check stack output
│   ├── transport.py             # socat VSOCK⇄TCP relay
│   ├── ssh.py                   # SSH-into-guest for low-level checks
│   ├── postgres.py              # in-VM Postgres readiness
│   └── version.py               # outer version + nested-KVM checks
├── images/
│   ├── corvus-integration-test.yml   # `crv build` pipeline
│   └── systemd/                      # unit files dropped into the image
└── tests/
    └── test_*.py
```

## CI

This suite is not for stock GitHub Actions runners — they lack nested
KVM. Target: self-hosted runners or bare-metal CI with `kvm_*.nested=1`.

## Known caveats

- The image build references the `gentoo-base-headless` disk produced
  by `yaml/gentoo-test/gentoo-test.yml`. If you've never run that, your
  first integration-test session will need a network connection and 30-60
  min to bake the base.
- The harness deletes its VMs on test exit, but not on hard crash.
  Orphan cleanup: `crv vm list | grep ^corvus-it- | xargs -n1 crv vm
  delete --delete-disks`.
- The inner daemon listens on `0.0.0.0:9876` inside its VM. The VM has
  no external network exposure by default; the VSOCK CID is the only
  way in.
