# Corvus integration tests

End-to-end test suite for Corvus. The suite runs a freshly built Corvus
inside nested VMs created by an already-running outer Corvus install, then
drives that inner daemon through the same Python `corvus_client` path that
real clients use.

The outer daemon is not under test. It is the harness provider: it creates
test-node VMs, session networks, shared directories, and disposable overlays.
Outer and inner versions do not need to match.

## Architecture And Terminology

```
Host
  pytest / corvus_test_harness
    |
    | outer crv CLI
    v
Outer Corvus daemon
  creates corvus-it-session-* network
  creates corvus-it-<Class>-<runid>-<node> VMs
    |
    | managed NIC + VSOCK + virtiofs shared dirs
    v
Test node VM
  /opt/corvus/bin     <- host stack install bin dir, read-only
  /home/corvus/VMs/BaseImages <- host ~/VMs/BaseImages, read-only
  corvus.service      <- inner daemon
  corvus-nodeagent.service
  corvus-netd.service
  postgresql
    |
    | inner daemon creates these during tests
    v
Inner VMs / disks / networks
```

- **Host**: the developer machine running `make integration-tests`.
- **Outer daemon**: the installed Corvus daemon reachable by the host `crv`.
  It owns the real test-node VMs.
- **Test node**: an outer VM named
  `corvus-it-<TestClass>-<8-hex-run-id>-<short-name>`, usually `single`,
  `alpha`, `beta`, or `gamma`.
- **Inner daemon**: `/opt/corvus/bin/corvus` inside a test node. This binary
  is the host's current `stack build` output mounted with virtiofs.
- **Inner agents**: `corvus-nodeagent` and `corvus-netd` inside each test node.
- **Test VM** or **Inner VM**: a VM created by a test through the inner daemon.
- **Session network**: one outer managed network named `corvus-it-session-*`,
  shared by all test nodes in a pytest session.
- **Topology**: the class-scoped harness object that owns one or more test
  nodes, their certs, relays, clients, and teardown.

The common test bases are:

- `SingleNodeCase`: one full-stack node, exposed as `self.node` and
  `self.client`.
- `OneDaemonTwoNodesCase`: `alpha` runs the daemon and agents; `beta` runs only
  agents. Alpha's daemon registers and drives beta.
- `TwoDaemonsCase`: two independent full-stack nodes with separate CAs.
- `ThreeNodesCase`: three full-stack nodes for multi-node scenarios.

## Prerequisites

- Linux host with KVM and nested virtualization enabled:

  ```sh
  cat /sys/module/kvm_intel/parameters/nested   # or kvm_amd
  ```

  Expected output is `Y` or `1`.

- A reachable outer Corvus install. `make install` installs `crv` under
  `~/.local/bin`; `CORVUS_CRV=/path/to/crv` overrides the CLI used by the
  harness.
- `stack`, `ssh`, `socat`, `jq`, QEMU/KVM, and the normal Corvus runtime tools.
- A freshly built inner binary:

  ```sh
  stack build
  ```

  The harness checks `<stack local-install-root>/bin/corvus`. If that binary is
  older than Haskell source under `src/`, pytest prints a stale-binary warning.

## Required Images

Pytest does not build images. It checks that required images are already
registered with the outer daemon and fails fast with the relevant make target
when one is missing. This avoids xdist workers racing each other on a cold
image bake.

Build everything once:

```sh
make test-image
```

Useful narrower targets:

- `make test-image-key`: create `integration_tests/keys/corvus-test-key`.
  The public key is baked into the Gentoo test-node image and Alpine test VM.
- `make test-image-node`: build/register `corvus-test-node`, the Gentoo-based
  test-node image used by every topology.
- `make test-image-vm`: build/register `corvus-test-vm`, the Alpine image used
  by most Linux VM tests.
- `make test-image-multi-os`: download/register Debian, Ubuntu, AlmaLinux,
  FreeBSD, and Alpine cloud images used by cloud-init tests.
- `make test-image-windows`: build/register `windows-server-2025-eval`.
- `make test-image-installer`: build/register the tiny synthetic installer ISO
  used by `TestBuildInstaller`.

Image artifacts are registered with the outer daemon and usually live under
`~/VMs/BaseImages`. The test-node VM mounts that host directory at
`/home/corvus/VMs/BaseImages`, so tests can register base images with the inner
daemon without copying image bytes.

To force a rebuild, delete the registered disk/template first, or use the
matching clean target such as `make test-image-node-clean`.

## Running Tests

From the repo root:

```sh
make integration-tests
```

The target builds the Haskell project, creates `integration_tests/.venv` if
needed, installs the root package with the `harness` extra, and runs pytest
against `integration_tests/tests`.

Run a subset:

```sh
make integration-tests MATCH=TestCloudInit
make integration-tests MATCH=test_network_create_records_task
make integration-tests MATCH="lifecycle and not edit"
```

`MATCH` is passed to pytest as `-k`, so it is a substring/boolean expression.
For example, `MATCH=TestCloudInit` also matches `TestCloudInitCli`.

Control parallelism:

```sh
make integration-tests WORKERS=4
```

Without `MATCH`, the Makefile auto-selects the xdist worker count with
`integration_tests/scripts/detect_workers.py`: one worker per 2 logical CPUs
and one worker per 3 GiB of `MemAvailable`, taking the smaller budget.

Direct pytest runs are useful for debugging:

```sh
integration_tests/.venv/bin/pytest integration_tests/tests/test_vm_lifecycle.py -v
integration_tests/.venv/bin/pytest integration_tests/tests -v -k TestDisk -s
```

The repo pytest config uses `--dist=loadscope`; each test class stays on one
worker so its class-scoped topology boots once. The harness also orders test
methods by source line, starts `@pytest.mark.slow` classes early, and skips
remaining methods in a class after the first real failure.

## Failure And Cleanup Behavior

Successful class teardown closes clients, stops VSOCK relays, stops and deletes
test-node VMs, removes ephemeral overlays, and deletes the per-class cert tree.
Successful session teardown stops and deletes the `corvus-it-session-*`
network.

On failure, the harness preserves state for inspection:

- A failed test method leaves that class's test-node VM(s) alive.
- The first failure in a class causes later methods in the same class to skip.
- A class fixture setup failure is recorded and surfaced as a skip reason for
  each method, while any partially booted topology is left inspectable.
- The session network is intentionally leaked if any test failed, so preserved
  nodes remain reachable.
- Per-class host-side certs stay under
  `/tmp/corvus-it-pki-<Class>-<runid>/`.

Hard crashes, `SIGABRT`, or host interruption can also leave resources behind.
Clean all integration-test VMs and networks with:

```sh
make integration-tests-clean
```

Manual cleanup:

```sh
crv -o json vm list | jq -r '.[].name | select(startswith("corvus-it-"))'
crv vm reset <outer-vm-name>
crv vm delete <outer-vm-name>

crv -o json network list | jq -r '.[].name | select(startswith("corvus-it-"))'
crv network stop <network-name> --force
crv network delete <network-name>
```

## Debugging Failed Runs

Start by finding the preserved test node name in pytest output. It looks like:

```text
corvus-it-TestVmLifecycle-a1b2c3d4-single
```

List preserved nodes:

```sh
crv vm list
crv -o json vm list | jq -r '.[].name | select(startswith("corvus-it-"))'
```

Show outer VM details, including VSOCK CID and guest IPs:

```sh
crv vm show corvus-it-TestVmLifecycle-a1b2c3d4-single
crv -o json vm show corvus-it-TestVmLifecycle-a1b2c3d4-single | jq
```

### SSH Into A Test Node

Preferred helper, using the node's outer managed-network IP:

```sh
integration_tests/scripts/ssh-it corvus-it-TestVmLifecycle-a1b2c3d4-single
integration_tests/scripts/ssh-it corvus-it-TestVmLifecycle-a1b2c3d4-single \
  sudo journalctl -u corvus.service --no-pager -n 300
```

Manual VSOCK SSH, useful when DHCP/IP reporting is broken:

```sh
cid=$(crv -o json vm show corvus-it-TestVmLifecycle-a1b2c3d4-single | jq -r .vsock_cid)
ssh \
  -i integration_tests/keys/corvus-test-key \
  -o StrictHostKeyChecking=no \
  -o UserKnownHostsFile=/dev/null \
  -o "ProxyCommand=socat - VSOCK-CONNECT:${cid}:22" \
  corvus@vsock-${cid}
```

Use `root@vsock-${cid}` for root login when needed; most harness operations use
the `corvus` user plus `sudo`.

### Run `crv` Against The Inner Daemon

From the host, use the debug wrapper:

```sh
integration_tests/scripts/crv-it corvus-it-TestVmLifecycle-a1b2c3d4-single status
integration_tests/scripts/crv-it corvus-it-TestVmLifecycle-a1b2c3d4-single vm list
integration_tests/scripts/crv-it corvus-it-TestVmLifecycle-a1b2c3d4-single task list
```

`crv-it` resolves the outer VM, finds the leaked cert directory under `/tmp`,
parses the node's IPv4 address, and runs:

```sh
crv --host <node-ip> --tls-cert-dir <cert-dir> ...
```

It only works while a topology is paused or leaked after failure. Clean teardown
removes the cert directory.

You can also run the inner CLI from inside a node:

```sh
integration_tests/scripts/ssh-it <node> /opt/corvus/bin/crv status
```

This requires client certs in `/home/corvus/.config/corvus`. Some tests install
them explicitly with `install_node_client_certs`; otherwise prefer `crv-it`.

### View Inner Logs

Daemon and agents:

```sh
integration_tests/scripts/ssh-it <node> sudo journalctl -u corvus.service --no-pager -n 300
integration_tests/scripts/ssh-it <node> sudo journalctl -u corvus-nodeagent.service --no-pager -n 300
integration_tests/scripts/ssh-it <node> sudo journalctl -u corvus-netd.service --no-pager -n 300
```

Follow logs live:

```sh
integration_tests/scripts/ssh-it <node> sudo journalctl -u corvus.service -f
```

Boot and system state:

```sh
integration_tests/scripts/ssh-it <node> sudo journalctl -b --no-pager
integration_tests/scripts/ssh-it <node> systemctl status corvus corvus-nodeagent corvus-netd
integration_tests/scripts/ssh-it <node> ip addr
integration_tests/scripts/ssh-it <node> ss -ltnp
integration_tests/scripts/ssh-it <node> mount
```

Outer daemon logs, when the outer daemon is user-systemd managed:

```sh
journalctl --user -u corvus.service --no-pager -n 300
```

### Inspect Mounts And Images

The inner binary mount should expose the host build output:

```sh
integration_tests/scripts/ssh-it <node> ls -la /opt/corvus/bin
integration_tests/scripts/ssh-it <node> /opt/corvus/bin/corvus --version
```

The base-image mount should expose host images:

```sh
integration_tests/scripts/ssh-it <node> ls -la /home/corvus/VMs/BaseImages
integration_tests/scripts/ssh-it <node> mountpoint /home/corvus/VMs/BaseImages
```

## Common Problems

- **Missing `corvus-test-node`**: run `make test-image-node`.
- **Missing Alpine/base image in a VM test**: run `make test-image-vm` or
  `make test-image`.
- **Missing cloud-init distro image**: run `make test-image-multi-os`.
- **Missing Windows image**: run `make test-image-windows`.
- **Missing synthetic installer ISO**: run `make test-image-installer`.
- **Stale binary warning**: run `stack build` before trusting results.
- **No `vsock_cid` on a test node**: check the outer VM is running and the host
  supports VSOCK/KVM; inspect `crv vm show <node>`.
- **Inner daemon timeout**: inspect `corvus.service`, `/opt/corvus/bin`, cert
  deployment under `/etc/corvus`, and mount units.
- **`nodeagent unavailable` from inner daemon**: inspect
  `corvus-nodeagent.service` and confirm the inner node row exists with
  `crv-it <node> node list`.
- **No test-node IP or DHCP problems**: inspect the `corvus-it-session-*`
  network, `corvus-netd.service`, `ip addr`, and `journalctl -u corvus-netd`.
- **SSH public-key failure**: ensure `integration_tests/keys/corvus-test-key`
  exists and that images were rebuilt after generating it.
- **Too many leaked resources or overlapping `10.91.*.0/24` networks**: run
  `make integration-tests-clean`.

## File Layout

```
integration_tests/
├── conftest.py                  # session fixtures, xdist scheduling, failure policy
├── corvus_test_harness/
│   ├── cases.py                 # IntegrationTestCase and topology bases
│   ├── topology.py              # test-node VM creation, cert deploy, teardown
│   ├── images.py                # precondition checks for required outer images
│   ├── base_images.py           # register host BaseImages with inner daemons
│   ├── host_binary.py           # locate/freshness-check stack output
│   ├── inner.py                 # pycapnp client readiness and self-node register
│   ├── transport.py             # host TCP to guest VSOCK relay
│   ├── ssh.py                   # NodeShell and VmShell SSH transports
│   ├── component_deploy.py      # per-class CA and service cert deployment
│   └── vm.py                    # VM lifecycle helpers used by tests
├── scripts/
│   ├── crv-it                   # host-side crv wrapper for leaked inner daemon
│   ├── ssh-it                   # SSH wrapper for leaked test nodes
│   └── detect_workers.py        # xdist worker budget calculator
└── tests/
    └── test_*.py
```

## CI And Capacity Notes

This suite is not suitable for stock GitHub Actions runners because they lack
nested KVM. Use a bare-metal or self-hosted runner with nested virtualization,
enough RAM for the selected worker count, and access to any image-download
sources needed by the `make test-image*` targets.

Most test classes boot one 8 vCPU / 8 GiB test node. Multi-node classes boot
more than one. If the host starts swapping or QEMU processes get killed, lower
parallelism with `WORKERS=N` and clean up leaked nodes before retrying.
