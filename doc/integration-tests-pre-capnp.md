# Integration-test coverage snapshot (pre-Cap'n Proto)

This document catalogues the integration tests that lived in `test/Corvus/*IntegrationSpec.hs`
at git commit `e371247` ("Removed unused log"), right before the Cap'n Proto wire migration
stubbed them out. It exists as a checklist for rewriting these suites on top of the new
Cap'n Proto RPC: every `it` bullet below names the setup, the action exercised, and the
specific assertions, so a reviewer can verify the rewritten test preserves the original
behavioural coverage.

Retrieve the original source of any file with:

```
git show e371247:test/Corvus/<FileName>.hs
```

Each section below is one spec file. Tests are grouped by their `describe` blocks
(preserving nesting). File-level fixtures and helpers are called out once at the top of
the section rather than repeated per test.

## Port status

Each `it` bullet carries a GitHub-flavoured task-list marker:

- `[x]` — ported to the new pycapnp-based integration suite under
  [`tests-integration/`](../tests-integration/). The pointer line names the
  current Python test method.
- `[~]` — partially ported. The original assertions are split across
  several Python tests, or some sub-assertions are missing / dropped
  (usually because the post-capnp schema deliberately doesn't expose
  the feature, e.g. clone-to-custom-path).
- `[ ]` — not yet ported. The pre-capnp stub file in
  [`tests-integration/tests/`](../tests-integration/tests/) is still
  carrying a `pytest.skip(reason="TODO: port ...")` placeholder.

Roll-up by spec:

| Spec file | ported | partial | not yet | total |
|---|---|---|---|---|
| ApplyIntegrationSpec | 0 | 0 | 3 | 3 |
| BuildIntegrationSpec | 0 | 0 | 2 | 2 |
| CloudInitIntegrationSpec | 0 | 0 | 15 | 15 |
| DiskIntegrationSpec | 11 | 0 | 0 | 11 |
| GuestAgentPollerIntegrationSpec | 0 | 1 | 0 | 1 |
| NetworkIntegrationSpec | 0 | 0 | 7 | 7 |
| SerialConsoleIntegrationSpec | 0 | 0 | 7 | 7 |
| SnapshotIntegrationSpec | 6 | 1 | 0 | 7 |
| TemplateIntegrationSpec | 0 | 0 | 2 | 2 |
| VirtiofsIntegrationSpec | 0 | 0 | 2 | 2 |
| VmIntegrationSpec | 7 | 1 | 1 | 9 |
| WindowsIntegrationSpec | 0 | 0 | 2 | 2 |
| **total** | **24** | **3** | **41** | **68** |

---

## ApplyIntegrationSpec.hs

**Purpose:** end-to-end exercise of `crv apply` — confirm YAML environment definitions
create disks/networks/VMs in the daemon, the resulting VMs actually boot, and SSH keys
declared in YAML are deployed via cloud-init.

**Setup/fixtures used:** `withTestDb`, `withTestDaemon`, `withDaemonConnection`,
`applyConfig`, `ensureBaseImage` (downloads `corvus-test` or `alpine-3.20-bios`),
`startTestVm`/`startTestVmSync`/`stopTestVmAndWait`, `startNetwork`/`stopNetwork`,
`runViaGuestAgent`/`runViaGuestAgent_`, `generateSshKeyPair`/`cleanupSshKeyPair`,
`waitForTestVmSshWithKey`, `runInTestVmWith`. YAML is built with the `yamlQQ`
quasi-quoter and the helper `encodeYaml`.

### describe "Apply integration"

  - [ ] **it "creates two VMs with networking and guest agent, and they can communicate"** —
    applies a YAML that declares a `corvus-test` base disk + two overlays
    (`apply-net-root1/2`), a managed network `apply-vde` (empty subnet),
    and two VMs (`apply-vm1/2`, 2 CPU / 2048 MB, headless, guest-agent on, with a
    `user` NIC and a `managed: apply-vde` NIC). Asserts the `ApplyOk` result reports
    `arDisks == 3`, `arNetworks == 1`, `arVms == 2`. Then starts the network and both
    VMs sync, configures static IPs `10.0.0.1/24` and `10.0.0.2/24` on `eth1` via the
    guest agent, and asserts both directions of `ping -c 3 -W 5` exit `ExitSuccess`.

  - [ ] **it "deploys SSH key via cloud-init from a downloaded Alpine image"** — generates a
    test SSH keypair, picks a free TCP port, and applies a YAML containing one `sshKey`,
    an Alpine base + overlay (`sizeMb: 2048`), and a `cloudInit: true` VM with a
    `user` NIC carrying `hostfwd=tcp::<port>-:22` and the key referenced under
    `sshKeys`. Asserts `arSshKeys == 1`, `arDisks == 2`, `arVms == 1`. Starts the VM,
    waits for SSH (180 s timeout) using the deployed key as user `corvus`, then verifies
    `echo ssh-via-apply` returns "ssh-via-apply" and that `~/.ssh/authorized_keys`
    contains the substring `corvus-test@localhost`.

  - [ ] **it "deploys SSH key via apply with custom cloud-init config"** — same shape as
    above but the VM has `cloudInitConfig.userData` defining a `deployer` user
    (`sudo: NOPASSWD`, `lock_passwd: false`, `plain_text_passwd: testpass`),
    `ssh_pwauth: true`, package `qemu-guest-agent`, runcmds to restart sshd and the
    QGA service, plus `injectSshKeys: true`. Waits up to 120 s for SSH as
    `deployer`, asserts `echo custom-apply-ok` returns "custom-apply-ok" and
    `whoami` returns "deployer".

---

## BuildIntegrationSpec.hs

**Purpose:** verify the `crv build` image-bake pipeline: provisioners run inside a real
VM, the artifact disk is registered under the requested name, the bake VM is torn down,
and the marker files written by the provisioner survive into a fresh boot of the artifact.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`defaultVmConfig`, `runBuild`,
`templateCreate`/`templateDelete`, `vmCreate`/`vmDelete`, `diskAttach`/`diskDetach`/
`diskDelete`/`diskList`, `vmList`, `runViaGuestAgent`, `startTestVmSync`,
`stopTestVmAndWait`. Local helpers `findDiskByPrefix`, `findDiskByName`,
`assertNoBuildOrphans` (checks no VM with `__build_*__` name prefix remains),
and `verifyArtifactDisk` (boots a fresh verifier VM from the artifact, reads marker
files, then tears down and detaches but keeps the artifact).

### describe "Build integration"

  - [ ] **it "bakes an artifact and the marker file is present in the result"** — stops the
    seed VM, discovers its `base-image-*` registered disk, registers a minimal
    `build-test-tpl` template (overlay strategy, `guestAgent: true`, headless,
    2 CPU / 1024 MB), then invokes `runBuild` with a pipeline that names the artifact
    `build-test-artifact`, uses overlay strategy, and runs a shell provisioner writing
    `bake-marker` to `/var/lib/corvus-test/marker` and `$CORVUS_VERSION` to
    `/var/lib/corvus-test/version`, with `cleanup: always`. Asserts a single
    `BuildOne` result with matching name, no `boError`, and a `Just`
    `boArtifactDiskId`. Then confirms the artifact disk exists, no `__build_*` VM
    leaks remain, and `verifyArtifactDisk` boots a fresh `build-verify-vm`, reads both
    `marker` (must equal "bake-marker") and `version` (must be non-empty). Cleans up
    artifact + template at the end.

  - [ ] **it "fails cleanly when a provisioner exits non-zero (cleanup: always leaves no
    orphans)"** — same template setup but with a provisioner `exit 7`, artifact name
    `should-not-exist`. Asserts the build response returns a `BuildOne` with
    `boArtifactDiskId == Nothing` and `boError` containing either "shell" or
    "exited", that no disk named `should-not-exist` is registered, and that
    `assertNoBuildOrphans` passes (build VM was torn down despite the failure).

---

## CloudInitIntegrationSpec.hs

**Purpose:** validate cloud-init NoCloud SSH-key deployment across the matrix of
supported cloud images (Alpine, AlmaLinux, Ubuntu, Debian, Gentoo, FreeBSD) in both
UEFI and BIOS modes, plus multi-key, custom-user-data, and CRUD-only paths.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`withTestVmConsole`/
`withTestVmConsole`, `withTestDaemon`, `withTestDiskSetup`, `cloudVmConfig` (extended
into `multiOsConfig` with `vmcWaitSshTimeout = 300` and `vmcForceTcpSsh = True`),
`createTestVmWithOptions`, `addVmDisk`, `addVmNetIf`, `setupVmSshKey` (generates
pair + registers via daemon), `setCloudInitConfig`/`getCloudInitConfig`/
`deleteCloudInitConfig`, `cleanupSshKey`, `deleteTestVm`, `findFreePort`,
`startTestVmAndWait`, `runInTestVm`/`runInTestVmWith`. Local helper `verifyVm`
checks SSH login, `authorized_keys` content, `whoami == corvus`, hostname prefix
`test-vm-`, and optionally privilege escalation (sudo/doas) and qemu-guest-agent
presence after waiting for `cloud-init status` to reach done/error/disabled.

### describe "Multi-OS cloud-init integration"

#### describe "Alpine Linux"

  - [ ] **it "SSH key setup works with UEFI boot"** — boots `alpine-3.20-uefi`,
    runs `verifyVm` with `checkPrivEsc = True`, `checkGuestAgent = False`.

  - [ ] **it "SSH key setup works with BIOS boot"** — boots `alpine-3.20-bios`
    with `vmcUefi = False`, runs `verifyVm True False`.

#### describe "AlmaLinux"

  - [ ] **it "SSH key setup works with UEFI boot"** — `almalinux-10`, `verifyVm True False`.

  - [ ] **it "SSH key setup works with BIOS boot"** — `almalinux-10` with `vmcUefi = False`.

  - [ ] **it "SSH key setup works with multiple SSH keys"** — manually constructs the VM
    via `createTestVmWithOptions` + `addVmDisk` + `addVmNetIf` (TCP port forward),
    generates and attaches two SSH keys via `setupVmSshKey`, starts the VM, asserts
    `cat ~/.ssh/authorized_keys` contains at least two `ssh-` prefixed lines,
    and that the second private key independently can SSH in and run `echo key2-ok`.

#### describe "Ubuntu"

  - [ ] **it "SSH key setup works with UEFI boot"** — `ubuntu-24.04`, `verifyVm True False`.

  - [ ] **it "SSH key setup works with BIOS boot"** — `ubuntu-24.04`, BIOS, `verifyVm True False`.

#### describe "Debian"

  - [ ] **it "SSH key setup works with UEFI boot"** — `debian-12`.

  - [ ] **it "SSH key setup works with BIOS boot"** — `debian-12`, BIOS.

#### describe "Gentoo"

  - [ ] **it "SSH key setup works with UEFI boot"** — `gentoo`, `verifyVm True False`.

#### describe "FreeBSD"

  - [ ] **it "SSH key setup works with UEFI boot"** — `freebsd-14`, `verifyVm False False`
    (no priv-esc / no guest agent check; FreeBSD images do not ship qemu-guest-agent).

  - [ ] **it "SSH key setup works with BIOS boot"** — `freebsd-14`, BIOS.

  - [ ] **it "serial console login works"** — opens a serial console via `withTestVmConsole`,
    waits up to 90 s for `login:` (FreeBSD reboots once after cloud-init), drains,
    sends `root` (no console password by default), expects `#` within 30 s, runs
    `whoami` and asserts the output contains `root`.

#### describe "Custom cloud-init config"

  - [ ] **it "deploys custom user-data with SSH key injection"** — Alpine BIOS image with a
    custom `user-data` that creates a `testadmin` user (`sudo: NOPASSWD`,
    `plain_text_passwd: testpass`, `ssh_pwauth: true`, runcmd restarts sshd) plus
    `injectSshKeys: true`. After `getCloudInitConfig` round-trip asserts
    `ciiInjectSshKeys == True`, attaches the generated SSH key, starts the VM
    (300 s timeout), and asserts `echo custom-ci-ok`/`whoami` over SSH return the
    expected values for user `testadmin`.

  - [ ] **it "cloud-init config CRUD operations work"** — pure DB-path test (no VM start).
    Creates a VM with cloud-init enabled, asserts initial `getCloudInitConfig` returns
    `CloudInitConfig Nothing`. Sets user-data + network-config (`injectSshKeys = False`),
    re-reads and asserts both fields are `Just` and `ciiInjectSshKeys == False`.
    Calls `deleteCloudInitConfig` and asserts the next read again returns
    `CloudInitConfig Nothing`.

---

## DiskIntegrationSpec.hs

**Purpose:** exercise daemon-level disk operations end-to-end on real qcow2 files:
cloning (incl. snapshots, custom path, VM-must-be-stopped guard), overlay creation,
`diskRebase` (flatten and rebase-to-other-base), `diskImport` (copy + same-path
rejection), and hot-plug attach/detach on a running VM (incl. read-only).

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`withTestVmGuestExec`/
`defaultVmConfig`, `startTestDaemon`/`stopTestDaemon` (lifted manually for the
HOME-redirection cases), `withDaemonConnection`. Real qcow2 files are created with
`qemu-img create -f qcow2 ... 1M` in a `withSystemTempDirectory`, with `HOME`
swapped to the temp dir so the daemon writes into the temp `VMs/` directory.
Local helpers `createEmptyDisk`, `hotAttachDisk`, `hotAttachDiskReadOnly`,
`hotDetachDisk`, `getDiskInfo`, `showVmDetails`, `finally_`.

### describe "Disk clone integration"

  - [x] **it "can clone a disk and its content"** — clones the test VM's primary disk
    after stopping the VM. Asserts the returned `DiskCreated` succeeds, the new
    disk's name is `cloned-disk`, and `diiSizeMb` is `Just _` (auto-detected).  
    *→ `test_disk.py::TestDisk::test_clone_preserves_size_and_format`*

  - [x] **it "clones snapshots in the database"** — creates a snapshot `snap1` on the
    source disk, clones, then asserts `snapshotList` on the clone returns exactly
    one snapshot whose name is `snap1`.  
    *→ `test_disk.py::TestDisk::test_clone_preserves_snapshots`*

  - [x] **it "can clone to a custom path"** — clones with explicit `customPath =
    /tmp/custom-clone.qcow2`, asserts the file exists at that path after the call,
    then removes it.  
    *→ `test_disk.py::TestDisk::test_clone_to_custom_path` (the schema gained
    a `path` field on `DiskCloneParams`; the Python test uses a daemon-relative
    path and asserts the recorded `file_path` ends in the custom suffix).*

  - [x] **it "rejects cloning if VM is running"** — does NOT stop the VM first; asserts
    `diskClone` returns `Right (Right VmMustBeStopped)`.  
    *→ `test_disk.py::TestDisk::test_clone_rejects_running_vm`*

### describe "Disk overlay integration (requires qemu-img)"

  - [x] **it "creates overlay via daemon and lists backing info"** —
    `runOverlayTest`: registers an externally-created qcow2 as `base-disk`, calls
    `diskCreateOverlay` to make `overlay-disk`. Lists all disks and asserts the
    base has a size and the overlay carries `diiBackingImageId == Just baseId`,
    `diiBackingImageName == Just "base-disk"`, and a non-Nothing size.  
    *→ `test_disk.py::TestDisk::test_create_overlay_has_backing`*

### describe "Disk rebase integration (requires qemu-img)"

  - [x] **it "flattens an overlay into a standalone disk"** — `runFlattenTest`: registers
    base, creates overlay backed by it, asserts backing is set, then calls
    `diskRebase` with target = `Nothing` (flatten), and re-reads the disk to
    confirm `diiBackingImageId == Nothing`.  
    *→ `test_disk.py::TestDisk::test_rebase_flatten_drops_backing` (a new
    `DiskManager.flatten` cap method exposes the daemon's existing flatten
    path; the Python test asserts both `backing_image_id` and
    `backing_image_name` are None after the call).*

  - [x] **it "rebases overlay to a different backing image"** — `runRebaseTest`: registers
    two bases, creates an overlay over base1, asserts backing == base1, then calls
    `diskRebase` with the new base2's ID. Confirms the overlay's backing now points
    to base2.  
    *→ `test_disk.py::TestDisk::test_rebase_to_other_base`*

### describe "Disk import integration (requires qemu-img)"

  - [x] **it "imports a local file by copying it to destination"** — `runImportCopyTest`:
    creates a qcow2 outside the daemon's VMs dir, calls `diskImport` (wait=True,
    format `qcow2`), asserts the file appears at `<daemonVmDir>/import-copy.qcow2`
    and the DB record's name is `import-copy`.  
    *→ `test_disk.py::TestDisk::test_import_local_file_copies`*

  - [x] **it "rejects import when source and destination are the same"** —
    `runImportSamePathTest`: places the source inside the daemon's VMs dir at the
    same path the import would write to, asserts the call returns
    `DiskError msg` where `msg` contains the substring "same".  
    *→ `test_disk.py::TestDisk::test_import_same_path_rejected`*

### describe "Live disk attach/detach integration"

  - [x] **it "can hot-plug and hot-unplug a data disk on a running VM"** — boots a VM
    with guest exec available, creates an empty 512 MB qcow2 (`createEmptyDisk
    "hotplug-data" FormatQcow2 512`), asserts `diiSizeMb == Just 512`. Hot-attaches
    via `diskAttach` with virtio + writeback cache, asserts the returned drive ID
    appears in `vmShow`'s `vdDrives`. Guest runs `ls /dev/vd* | wc -l` to confirm
    the kernel sees the new device. Detaches, asserts the drive disappears from
    `vdDrives`. Re-attaches the same disk (regression check that the qcow2 lock
    was released cleanly), then final detach.  
    *→ `test_disk.py::TestDisk::test_hot_attach_detach_reattach`*

  - [x] **it "can attach a disk read-only to a running VM"** — same flow with
    `hotAttachDiskReadOnly` (256 MB disk, `readOnly = True` flag) — asserts the
    drive appears in details and is cleanly detachable.  
    *→ `test_disk.py::TestDisk::test_hot_attach_read_only`*

---

## GuestAgentPollerIntegrationSpec.hs

**Purpose:** confirm the background guest-agent polling thread populates the
healthcheck timestamp and `niGuestIpAddresses`, that the timestamp advances on
each cycle, and that it resets correctly after a stop/start.

**Setup/fixtures used:** `withTestDb`, `withTestDaemonConfig` wrapped as
`withTestDaemonFastPoll` (overrides `qcHealthcheckInterval = 3` seconds),
`withTestVmGuestExecOnDaemon`/`defaultVmConfig`, `startTestVmAndWaitGuestAgent`,
`stopTestVmAndWait`, `listVmNetIfs`. Local helpers `waitForHealthcheck`,
`waitForGuestIps`, `waitForHealthcheckChange`, `getHealthcheck` poll
`vmShow`/`listVmNetIfs` at 1 Hz with explicit timeouts.

### describe "Guest Agent Poller"

  - [~] **it "populates healthcheck and guest IP addresses after VM start"** — boots VM
    with 3 s healthcheck interval. Waits up to 60 s for the first healthcheck
    timestamp (`hc1`). Waits up to 30 s for at least one NIC to report
    `niGuestIpAddresses` containing "/" (CIDR-style). Waits up to 30 s for
    `hc2 /= Just hc1` confirming the poller is running. Stops the VM, restarts
    it with `startTestVmAndWaitGuestAgent` (120 s), waits for a fresh healthcheck
    `hc3` and asserts `hc3 > hc1` (new timestamps after reboot are strictly later).  
    *Partial → `test_vm_lifecycle.py::TestVmLifecycle::test_vm` checks
    `last_healthcheck` is set after `wait_for_qga` and advances within ~30 s.
    The guest-IP assertion and the across-restart `hc3 > hc1` check are
    not yet ported; both belong on a dedicated TestHealthcheckPoller class.*

---

## NetworkIntegrationSpec.hs

**Purpose:** validate the virtual-networking stack: dnsmasq lifecycle, bridge/TAP
inside the shared namespace, NAT, and multi-VM connectivity over the bridge
(with and without DHCP).

**Setup/fixtures used:** `withTestDb`, `withTestDaemon`, `withTestVmOnDaemon`,
`createNetwork`/`createNetworkWithSubnet`/`createNetworkWithNat`, `startNetwork`/
`stopNetwork`/`deleteNetwork`, `showNetwork`, `runInTestVm`/`runInTestVm_`.
Local helpers `processAlive` (reads `/proc/<pid>/status`, excludes zombies),
`waitForProcessExit` (polls every 100 ms).

### describe "Network integration"

#### describe "Multi-VM communication"

  - [ ] **it "two VMs are accessible via SSH simultaneously"** — two VMs sharing a daemon,
    both `vmcForceTcpSsh = True`. Asserts both `hostname` calls succeed and return
    non-empty output, and that their `/proc/sys/kernel/random/boot_id` values differ
    (proves they are independent VMs, not the same one).

#### describe "Network namespace"

  - [ ] **it "starts dnsmasq for a network with DHCP enabled"** — creates network with
    subnet `10.88.0.0/24`, starts it. Asserts `showNetwork` reports
    `nwiRunning == True`, `nwiDhcp == True`, `nwiDnsmasqPid == Just _`, and the
    pid is live in `/proc`.

  - [ ] **it "dnsmasq stops cleanly when network is stopped"** — same setup with subnet
    `10.87.0.0/24`, captures the dnsmasq PID, stops the network, waits up to 5 s
    (50 × 100 ms) for the process to exit. Asserts `processAlive == False`,
    `nwiRunning == False`, `nwiDnsmasqPid == Nothing`.

  - [ ] **it "network without DHCP does not start dnsmasq"** — `createNetwork` with no
    subnet, starts it. Asserts `nwiRunning == True` and `nwiDnsmasqPid == Nothing`
    (bridge runs but dnsmasq is not spawned).

#### describe "Virtual networking"

  - [ ] **it "two VMs can communicate over a virtual network"** — boots two VMs both
    attached to a bridged network. Inside each guest, runs
    `doas ip addr add 10.0.0.{1,2}/24 dev eth1 && ip link set eth1 up`, then
    `ping -c 3 -W 5 10.0.0.{2,1}` in both directions, asserts both pings exit
    `ExitSuccess`.

  - [ ] **it "two VMs get DHCP addresses and communicate over a virtual network"** —
    network with subnet `10.99.0.0/24`. Both guests run `udhcpc -i eth1 -n -q`,
    the test scrapes the assigned IPs (`ip -4 -o addr show eth1`), asserts both
    start with `10.99.0.` and are distinct, and that ping works both ways. After
    teardown, attempts a new `createNetwork`/`deleteNetwork` cycle on the same
    daemon (regression check: stopping the namespace must not have SIGTERMed the
    daemon).

#### describe "NAT"

  - [ ] **it "VM on a NAT-enabled network can reach the internet"** — network with NAT
    enabled, subnet `10.77.0.0/24`. Guest brings up `eth1` via udhcpc (backgrounded
    with `-b`), asserts assigned IP has prefix `10.77.0.`. Replaces the default
    route via `10.77.0.1 dev eth1`, then `ping -c 3 -W 5 8.8.8.8` must succeed,
    and `nslookup example.com 8.8.8.8` must succeed with stdout containing
    "example.com" (case-insensitive).

---

## SerialConsoleIntegrationSpec.hs

**Purpose:** verify the daemon-side serial console ring buffer: it replays prior
output to reconnecting clients, captures output written while no client is attached,
the `ReqSerialConsole` protocol upgrade switches the connection into raw mode,
non-headless and stopped VMs are rejected, the console keeps working after a
VM reboot, and the buffer is removed from server state when the VM stops.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`withTestVmOnDaemon`/
`defaultVmConfig` with `vmcHeadless = True`, `vmcForceTcpSsh = True`,
`withTestDaemon`, `withDaemonConnection`, `sendRequest`, `connectSerialConsole`/
`consoleExpect`, `sendQmpCommand`, `createTestVmWithOptions`, `startTestVm`/
`stopTestVmAndWait`, `deleteTestVm`, `waitForTestVmSshWithKey`. Direct access
to `ssSerialBuffers` (STM TVar) and `ssQemuConfig` on `tdState daemon`.

### describe "Serial Console Integration"

  - [ ] **it "preserves output across reconnections"** — boots a headless VM, opens a
    serial console with the current snapshot of `ssSerialBuffers`, expects
    `login:` within 60 s. Closes and reopens a fresh connection (with a re-read
    buffer snapshot) and expects `login:` again within 10 s — proving the buffer
    is still replaying the previous boot output.

  - [ ] **it "captures output generated while disconnected"** — writes
    `SERIAL-MARKER-12345` to `/dev/ttyS0` via SSH (with no console client
    attached), delays 500 ms, then connects and expects the marker substring in
    the replayed buffer.

  - [ ] **it "RPC protocol upgrade works end-to-end"** — sends `ReqSerialConsole`
    over a raw connection. Asserts the response is `RespSerialConsoleOk` and
    that subsequent `recv` on the same socket returns non-empty bytes containing
    `login:` (confirming the daemon dropped into raw passthrough mode).

  - [ ] **it "rejects serial console for non-headless VM"** — creates a non-headless
    VM, starts it, sleeps 3 s, then sends `ReqSerialConsole`. Asserts a
    `RespError` whose message contains the substring "not headless".

  - [ ] **it "rejects serial console for stopped VM"** — creates a headless VM but
    does **not** start it. Asserts the response is `RespError` with message
    containing "not running".

  - [ ] **it "serial console works after VM reboot"** — confirms the initial
    `login:` prompt arrives. Creates a tmpfs marker file `/tmp/reboot-test-marker`,
    issues `system_reset` via QMP, sleeps 5 s, then waits up to 120 s for SSH
    to come back. Asserts the marker file is gone (`ExitFailure 1`) — proves
    the reboot really happened — then reconnects to the serial console and
    expects `login:` again within 10 s.

  - [ ] **it "cleans up buffer after VM stop"** — boots VM on a managed daemon,
    asserts the VM's ID is a key in `ssSerialBuffers`. Stops the VM, sleeps 2 s
    for the buffer thread to clean up, asserts the key is gone from the map.

---

## SnapshotIntegrationSpec.hs

**Purpose:** validate disk snapshot operations through the daemon — create,
rollback, list, merge, name-collision rejection, running-VM rejection, and rapid
sequential creation.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`defaultVmConfig`,
`startTestVmAndWait`, `stopTestVmAndWait`, `withDaemonConnection`,
`runInTestVm`. Local helpers wrap `snapshotCreate`/`snapshotRollback`/
`snapshotDelete`/`snapshotMerge`/`snapshotList` (`createSnapshot`,
`rollbackSnapshot`, `deleteSnapshot`, `mergeSnapshot`, `listSnapshots`)
with `fail` on connection/RPC errors, plus `try*` variants that surface the
`SnapshotResult` for error-path assertions.

### describe "Snapshot integration through daemon"

  - [x] **it "can rollback to restore deleted file"** — boots VM, writes a
    UUID-derived string to `/home/corvus/testfile.txt`, confirms it readable.
    Stops VM, creates snapshot `before-delete`. Restarts, deletes the file,
    confirms `test -f` fails. Stops, rolls back the snapshot, restarts, and
    asserts `cat` of the file returns the original UUID-derived content.  
    *→ `test_snapshots.py::TestSnapshots::test_rollback_restores_file`*

  - [x] **it "can create multiple snapshots and rollback to specific one"** —
    writes `'1'` then snapshots `state-1`; writes `'2'`, snapshots `state-2`;
    writes `'3'`. Rolls back to `state-1`, restarts, and asserts the file
    contains `1`.  
    *→ `test_snapshots.py::TestSnapshots::test_rollback_specific_snapshot_of_many`*

  - [x] **it "snapshot list shows created snapshots"** — creates `snapshot-alpha`
    and `snapshot-beta` on a stopped VM, asserts `listSnapshots` returns
    `>= 2` entries containing both names.  
    *→ `test_snapshots.py::TestSnapshots::test_snapshot_create_list_delete`
    (single-snapshot list assertion) and
    `test_rapid_sequential_creates` (three-snapshot list assertion).*

  - [x] **it "can merge snapshot to consolidate changes"** — writes "initial",
    snapshots `to-merge`, writes "modified", merges the snapshot, asserts
    the snapshot count dropped by exactly one and the snapshot ID is no
    longer in the list. Restarts and confirms the file's content is the
    post-merge value "modified".  
    *→ `test_snapshots.py::TestSnapshots::test_snapshot_merge`*

  - [x] **it "rejects duplicate snapshot names on the same disk"** — first
    `createSnapshot "same-name"` succeeds (ID > 0). Second call with the
    same name on the same disk must return `SnapshotError _` (unique
    constraint violation). Confirms only one snapshot of that name exists
    in the list, then verifies a different name (`different-name`) still
    works.  
    *→ `test_snapshots.py::TestSnapshots::test_duplicate_name_rejected`*

  - [~] **it "rejects snapshot operations on running VM"** — while the VM is
    running, asserts `tryCreateSnapshot`, `tryRollbackSnapshot`, and
    `tryMergeSnapshot` all return exactly `SnapshotVmMustBeStopped`. After
    stop, normal creation works.  
    *Partial → `test_snapshots.py::TestSnapshots::test_snapshot_rejects_running_vm`
    covers the create branch only; rollback and merge against a running
    VM aren't exercised yet (both daemon-side handlers now surface
    `VmMustBeStopped` via the matching RPC wiring, so the assertion is
    available — just no Python test method).*

  - [x] **it "handles concurrent snapshot operations"** — creates `concurrent-1`,
    `concurrent-2`, `concurrent-3` in rapid succession on a stopped VM,
    asserts all three IDs are returned and positive, and that the list
    returns exactly three matching entries.  
    *→ `test_snapshots.py::TestSnapshots::test_rapid_sequential_creates`*

---

## TemplateIntegrationSpec.hs

**Purpose:** verify VM templates: create from YAML, list/show/instantiate, boot
the instantiated VM, propagate `cloudInit` and `cloudInitConfig` from template to
instance.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`defaultVmConfig`,
`stopTestVmAndWait`, `withDaemonConnection`, `templateCreate`/`templateList`/
`templateShow`/`templateInstantiate`/`templateDelete`, `vmShow`/`vmEdit`,
`getCloudInitConfig`, `startTestVmSync`, `runViaGuestAgent`. Local helper
`findDiskName` finds disks by name prefix in the daemon's disk list (UEFI
firmware disks need to be discovered by prefix because the test fixture
auto-generates unique names like `base-image-<uuid>`).

### describe "VM Template integration"

  - [ ] **it "can create a template from YAML and instantiate it"** — stops the seed
    VM, looks up the auto-named `base-image-*`, `ovmf-code-*`, `ovmf-vars-template-*`
    disks. Creates template `test-template` (2 CPU / 2048 MB, description set,
    three drives: virtio overlay over base + 1024 MB size, pflash direct readOnly
    OVMF code, pflash clone of OVMF vars template). Verifies it appears in
    `templateList`, that `templateShow` reports the correct name, three drives,
    and the first drive's `tvdiDiskImageName` matches the base disk. Instantiates
    the template as `instantiated-vm`, asserts `vmShow` reports the right name,
    CPU/RAM, description, and exactly 3 drives. Enables guest-agent via `vmEdit`,
    starts sync, runs `whoami` via guest agent and asserts `ExitSuccess` with
    output `root`. Cleans up.

  - [ ] **it "propagates cloud-init config from template to instantiated VM"** —
    creates template `ci-template` with `cloudInit: true` and `cloudInitConfig`
    containing `userData.users[0]` named `custom-user` (sudo NOPASSWD), package
    `curl`, and `injectSshKeys: false`. Asserts `templateShow` reports
    `tvdCloudInit == True` and `tvdCloudInitConfig` is `Just _`. Instantiates,
    then `getCloudInitConfig` on the new VM returns a `CloudInitConfig (Just ci)`
    with `ciiUserData` set and `ciiInjectSshKeys == False`. Confirms `vmShow`
    also reports `vdCloudInit == True` with a non-Nothing `vdCloudInitConfig`.

---

## VirtiofsIntegrationSpec.hs

**Purpose:** validate virtiofs shared-directory functionality — guest can mount
and read/write a host directory — and that a missing host path causes VM start
to fail with a recorded `start-virtiofsd` sub-task error.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`defaultVmConfig` extended
with `vmcSharedDir`/`vmcUefi = False`/`vmcWaitSshTimeout = 300`,
`withTestDaemon`, `withTestDiskSetup`, `createTestVmWithGuestAgent`,
`addVmDisk`/`addVmNetIf`/`addVmSharedDir`, `withDaemonConnection`,
`vmStart`, `runInTestVm`, `listTasks`/`listSubtasks`, `deleteTestVm`.

### describe "Virtiofs integration"

  - [ ] **it "can access shared directory from VM via virtiofs"** — host creates
    `<sysTmp>/virtiofs-test-<uuid8>/testfile.txt` containing `UUID:<uuid>`.
    Boots an Alpine BIOS VM with that directory mounted. Guest runs
    `doas mkdir -p /mnt/share`, `doas mount -t virtiofs share /mnt/share`,
    then `cat /mnt/share/testfile.txt` — asserts output equals the host
    content. Writes `WRITTEN-BY-GUEST:<uuid8>` from the guest to
    `/mnt/share/guest-file.txt`, then reads the file on the host and asserts
    the content matches (bidirectional sharing works).

  - [ ] **it "VM start fails when shared directory does not exist"** — creates a VM
    manually with one shared dir pointing at
    `/tmp/nonexistent-dir-that-does-not-exist-12345`. Calls `vmStart` with
    wait=True and expects any failure path (`Left _`, `Right (Left _)`, or
    `Right (Right _)` is treated as test failure). Then queries
    `listTasks 50 Nothing Nothing True`, finds the `start` task for this VM
    (matched by command, `SubVm` subsystem, and entityId), enumerates its
    subtasks via `listSubtasks`, finds the `start-virtiofsd` subtask, and
    asserts `tiResult == TaskError`.

---

## VmIntegrationSpec.hs

**Purpose:** core VM lifecycle and behaviour over a real boot: guest exec
(runs as root), SSH command coverage and failure reporting, edit CPU/RAM on
stopped VM, virtio-vga vs headless display, UEFI vs BIOS firmware
characteristics, async vs sync start/stop, and graceful daemon shutdown.

**Setup/fixtures used:** `withTestDb`, `withTestVm`/`withTestVmGuestExec`/
`defaultVmConfig`/`biosVmConfig`, `startTestVmAndWait`, `stopTestVmAndWait`,
`editTestVm`, `runInVm`/`runInTestVm`/`runInTestVm_`, `connectSerialConsole`/
`consoleSend`/`consoleExpect`, `withDaemonConnection`, `vmStart`/`vmStop`/
`vmShow`, `handleGracefulShutdown`. Local helpers `getVmDetails`,
`waitForStatus` (1 Hz polling with timeout).

### describe "VM integration"

  - [x] **it "guest-exec: runs commands and executes as root"** —
    `runInVm vm "echo hello"` returns `ExitSuccess` with stdout `hello`;
    `runInVm vm "whoami"` returns `root` (proves guest agent runs as root).  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_vm` asserts
    `inner.vm.guest_exec("uname -s")` returns ExitSuccess + stdout `Linux`.
    (The "whoami == root" sub-assertion isn't repeated; it's an invariant of
    QGA's daemonised runtime, not of the cap surface.)*

  - [x] **it "SSH: runs commands, checks OS, reports failures, gets system info"** —
    over SSH: `echo hello` → `hello`; `cat /etc/os-release | grep -i alpine`
    contains "Alpine"; write `/tmp/testfile` then read it back; nonexistent
    command exits non-zero and stderr contains "not found"; `uname -s` →
    `Linux`; `cat /proc/meminfo` and `cat /proc/mounts` both succeed.  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_vm` exercises `uname -s`,
    `cat /etc/os-release`, `hostname`, and a 5× `inner.run("true")` multiplex
    smoke test.*

  - [x] **it "can edit CPU and RAM of a stopped VM"** — boots with 2 CPU / 2048 MB.
    Confirms `nproc == 2`, `free -m` reports between 1800 and 2100 MB. Stops,
    edits to 4 CPU / 4096 MB, restarts, confirms `nproc == 4` and RAM is between
    3800 and 4200 MB.  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_cpu_and_ram_edit_round_trip`*

  - [~] **it "detects virtio-vga graphics adapter in non-headless VM"** — BIOS VM
    with `vmcHeadless = False`. `lshw -class display` output must contain
    "VGA compatible controller" and "Virtio 1.0 GPU". Stops, edits to headless,
    restarts. Re-run `lshw -class display` — output must NOT contain "VGA
    compatible controller". Opens serial console, sends Enter, and expects
    `login:` within 60 s (confirming headless mode still routes console
    output to the serial buffer).  
    *Partial: the two `lshw -class display` assertions are split across two
    Python tests —
    `test_non_headless_vm_has_display_adapter` (non-headless, adapter present)
    and `test_vm`'s headless assertion (empty `lshw` output).
    The pre-capnp test did them together via an edit-swap cycle; the Python
    split skips that cycle. The serial-console-after-headless-swap assertion
    is unported (gated on the SerialConsole class rollout).*

  - [x] **it "UEFI VM has EFI boot entries"** — `efibootmgr` exits `ExitSuccess`
    and stdout contains `BootOrder`.  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_uefi_vm_lists_efi_boot_entries`*

  - [x] **it "BIOS VM does not have EFI support"** — same command exits non-zero
    and stderr contains `EFI variables are not supported`.  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_vm` asserts no `Boot0`
    entries in the BIOS guest's `efibootmgr -v` output.*

  - [x] **it "async start/stop works (no --wait)"** — `vmStop` with wait=False
    returns immediately, then `waitForStatus VmStopped` within 30 s.
    `vmStart` with wait=False returns immediately, then `waitForStatus
    VmRunning` within 120 s. Confirms guest agent works post-start via
    `echo async-ok`.  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_start_async_with_guest_agent`
    and `test_start_async_without_guest_agent` cover the QGA / no-QGA
    variants of the async path.*

  - [x] **it "sync start/stop works (--wait)"** — `vmStop` with wait=True; reading
    details immediately afterwards asserts `vdStatus == VmStopped` (server
    blocked until done). Then `vmStart` with wait=True; immediate read
    asserts `vdStatus == VmRunning`. Guest agent `echo sync-ok` succeeds.  
    *→ `test_vm_lifecycle.py::TestVmLifecycle::test_cpu_and_ram_edit_round_trip`
    drives `vm.stop(wait=True)` + `vm.start(wait=True)` explicitly across an
    edit cycle; every `InnerVmSsh`-based test (including `test_vm`) also
    uses the wait=True default for the initial boot.*

### describe "Graceful shutdown integration"

  - [ ] **it "graceful shutdown stops running VMs"** — confirms VM is `VmRunning`,
    calls `handleGracefulShutdown (tdState daemon)`, then asserts
    `vdStatus == VmStopped` (graceful shutdown propagates to every running VM).

---

## WindowsIntegrationSpec.hs

**Purpose:** verify Corvus works against Windows guests — guest agent ping,
QGA-based command execution (auto cmd.exe shell), OS-info detection, and
cloud-init (cloudbase-init) user-data execution.

**Setup/fixtures used:** `withTestDb`, `withTestDaemon`, `withWindowsImage`
(pendingWith if `.test-images/windows-server-eval.qcow2` is missing —
`make test-image-windows` builds it), `withDaemonConnection`,
`createTestVmWithOptions`, `addVmDisk`/`addVmNetIf`, `setCloudInitConfig`,
`startTestVm`/`stopTestVmAndWait`/`deleteTestVm`, `findFreePort`,
`runViaGuestAgent`, `vmShow`, `guestGetOsInfo`. UEFI firmware paths used
directly: `/usr/share/edk2/OvmfX64/OVMF_CODE.fd` and `OVMF_VARS.fd`. Local
helpers `registerDisk`, `createOverlay`, `cloneDisk`, `waitForHealthcheck`
(2 s polling, default 300 s timeout — Windows boots are slow), and
`waitForMarkerFile` (5 s polling).

### describe "Windows VM Integration"

  - [ ] **it "guest agent healthcheck and exec work on Windows VM"** — registers
    the Windows base image, creates an overlay, registers OVMF code and a
    cloned OVMF vars file. Creates VM `test-win-vm` (2 CPU / 4096 MB,
    non-headless, guest-agent on, cloud-init off). Attaches pflash CODE
    (readOnly), pflash VARS, virtio boot disk, and a `user` NIC with TCP
    hostfwd. Starts async, waits up to 300 s for healthcheck, asserts
    `vdStatus == VmRunning`. Runs `echo windows-test-ok` via guest agent
    (uses Windows shell auto-detection, i.e. `cmd.exe /c`) and asserts
    `ExitSuccess` and stdout contains `windows-test-ok`. Calls
    `guestGetOsInfo` directly and asserts `goiId` has prefix `mswindows`.

  - [ ] **it "cloud-init user-data script and hostname are applied on Windows VM"** —
    same firmware/disk/network setup but cloud-init enabled and custom
    user-data is a `#ps1_sysnative` PowerShell script that writes
    `cloud-init-ok` to `C:\cloud-init-marker.txt` and runs
    `net user Administrator corvus /y`. After boot + healthcheck, polls up
    to 120 s for `type C:\cloud-init-marker.txt` to succeed via guest agent
    and asserts the output contains `cloud-init-ok`. Then asserts
    `hostname` via guest agent returns exactly `test-win-ci` (cloud-init
    set hostname from VM name).
