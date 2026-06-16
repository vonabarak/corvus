# Image Builds (`crv build`)

`crv build` runs a YAML **pipeline**: an ordered list of steps that
either bake a reusable OS disk image (`build:`) or apply a declarative
config (`apply:`). The two kinds can be freely mixed in a single
file — typical use is *bake* → *register-as-template* → *bake again on
top of that template*.

A `build:` step:

1. Instantiates a named template into an ephemeral "bake" VM.
2. Optionally attaches an empty target disk (for `from-scratch`).
3. Starts the bake VM, waits for the guest agent.
4. Runs each provisioner inside the running VM (via `qemu-guest-agent`).
5. Stops the VM gracefully.
6. Publishes the artifact: `qemu-img convert`-clones the bake VM's
   chosen drive into a fresh non-ephemeral `DiskImage` (named by the
   build's `name:` field, optionally compacted). The clone is flat by
   construction — `qemu-img convert` does NOT carry over the source's
   internal qcow2 snapshot table, so the published image is
   guaranteed to have zero snapshots.
7. Tears down the bake VM and any other ephemeral resources, unless
   `--build-cache` left cache rows referencing the bake VM (in which
   case the bake VM survives — see [Build-step cache](#build-step-cache)).

An `apply:` step embeds the full
[`crv apply` schema](apply-configuration.md) and runs through the same
handler — registering SSH keys, disks, networks, VMs, and templates
into the daemon's database. Apply steps are most useful between two
build steps when the second one needs a template that wraps the first
one's artifact.

The bake VM has no cloud-init ISO and no SSH keys. Provisioners run via
`qemu-guest-agent`'s `guest-exec`, so the operator's identity and keys
never end up baked into the image.

## Hard requirement

> The base **template's bootdisk must already include `qemu-guest-agent`.**
> Without QGA we cannot run any provisioner. Almost every mainstream cloud
> image (Debian, Ubuntu, Fedora, Rocky, AlmaLinux, openSUSE) ships with
> QGA preinstalled or auto-installable on first boot.

## Running

```sh
# Async (default): returns a parent task id immediately; the build
# runs in the background. Track progress with `crv task show <id>` or
# `crv task list --subsystem build`.
crv build path/to/build.yml

# Blocking: client waits until every build in the pipeline completes
# (or fails) and prints per-build artifact disk ids.
crv build path/to/build.yml --wait
```

The client preprocesses the YAML before sending it to the daemon: any
`shell.script: <path>` becomes `shell.inline: <text>` and any
`file.from: <path>` becomes `file.content: <base64>`. Paths are
relative to the YAML file's directory. The daemon never reads the
client's filesystem.

## Schema

```yaml
pipeline:
  - build:
      name: debian-12-nginx                # required, unique within the file
      description: Debian 12 with NGINX    # optional, free text

      template: debian-12-cloud            # required: name of an existing template

      target:
        format: qcow2                      # default: qcow2
        sizeGb: 10                         # only used by from-scratch strategy
        compact: true                      # qemu-img -c rewrite at end (default true)
        path: builds/debian/               # optional, see below
        ifExists: error                    # error (default) | skip | overwrite — see below

      useCache: false                      # default: false; equivalent to --use-cache
      buildCache: false                    # default: false; equivalent to --build-cache
      cacheMode: memory                    # memory (default) | disk — see "Build-step cache" below

      strategy: overlay                    # overlay (default) | from-scratch | installer

      vm:
        cpuCount: 4                        # default: 4
        ramMb: 4096                        # default: 4096

      shellDefaults:
        preamble: "set -eux"
        env:
          SYSROOT: /mnt/sysroot
          DEV: /dev/vdb

      provisioners:                        # ordered, run in the started VM
        - shell: |
            set -euxo pipefail
            apt-get update
            apt-get install -y nginx

        - shell:
            script: ./scripts/harden.sh    # client reads + inlines
            env: { DEBIAN_FRONTEND: noninteractive }
            workdir: /tmp
            timeoutSec: 1800

        - file:
            from: ./files/nginx.conf       # client reads + inlines
            to:   /etc/nginx/nginx.conf
            mode: "0644"

        - wait-for: { file: /var/lib/cloud/instance/boot-finished, timeoutSec: 300 }

        - reboot: { timeoutSec: 300 }

      cleanup: always                      # always (default) | onSuccess | never

  - apply:                                 # full crv apply document, inlined
      templates:
        - name: debian-12-nginx
          cpuCount: 2
          ramMb: 2048
          guestAgent: true
          headless: true
          drives:
            - diskImageName: debian-12-nginx
              interface: virtio
              strategy: overlay
```

A pipeline step has exactly one of `build:` or `apply:`. Steps run in
order; if a step fails the pipeline aborts and any prior successful
steps stay applied (no rollback).

### `target.ifExists`

Controls what the daemon does when a disk with the same name as the
build (i.e. the `build.name` field, which drives the published
artifact's identity) is already registered at the moment the build
is about to start. The check happens **before** the bake VM is
created, so a wrong policy never wastes a bake.

| Value | Behaviour |
|---|---|
| `error` (default) | Fail immediately with `target '<name>' already exists; use ifExists: skip or overwrite to allow`. No bake VM is created. |
| `skip` | Return success without baking. The existing disk is treated as the artifact and its id is reported in the build result. Lets a re-run of a partially-failed pipeline walk past already-completed builds. |
| `overwrite` | Verify the existing disk is not attached to any VM (fails with the attached-VM names if it is), then proceed to bake. The actual deletion happens at publish time so a mid-bake failure preserves the existing artifact. |

```yaml
target:
  name: debian-12-nginx
  ifExists: skip
```

`overwrite` always **refuses** to delete a disk that is currently
attached to any VM; the operator must detach (or delete those VMs)
explicitly. This avoids silently yanking a disk out from under a
running or stopped VM. There is no auto-detach mode.

The same field name `ifExists:` exists at the top level of an `apply:`
document (see [apply-configuration](apply-configuration.md)), where it
acts as the YAML equivalent of `crv apply --skip-existing`. Apply
accepts `error` and `skip` only; `overwrite` is rejected. This means
a pipeline `apply:` step inside `crv build` (which has no
`--skip-existing` flag) can opt into skip-existing via the YAML.

### `target.path`

Optional. Controls the on-disk location of the published artifact.
Same semantics as `path:` on `apply` YAML's `disks:` entries:

| `path:` value | Resulting file location |
|---|---|
| omitted | `<basePath>/<name>.<ext>` |
| `subdir/` (trailing `/`) | `<basePath>/subdir/<name>.<ext>` |
| `subdir/file.qcow2` | `<basePath>/subdir/file.qcow2` |
| `/abs/dir/` | `/abs/dir/<name>.<ext>` (absolute) |
| `/abs/file.qcow2` | `/abs/file.qcow2` (absolute) |

Without `path:`, the artifact is moved out of the bake VM's
ephemeral runtime directory into the disk-base root. With `path:`,
it goes wherever you say, including absolute paths on different
filesystems (the daemon falls back to `copy + delete` if the
in-place rename crosses a filesystem boundary).

The `<ext>` is derived from `target.format` (`qcow2`, `raw`, …).

`crv disk show <name>` reports the absolute path the artifact ended
up at, so callers (`make` recipes, integration scripts) can resolve
the location without knowing the daemon's disk base.

## Strategies

### `installer`

For OSes whose vendor installer ships only as a bootable ISO with its
own unattended-install mechanism (Windows answer file, Alpine
`setup-alpine -e`, FreeBSD `bsdinstall script`). Corvus does not
drive provisioners over QGA — it just starts the bake VM, optionally
sends a few keystrokes via QMP `send-key` to dismiss firmware
prompts, and waits for the guest to power itself off. The bake VM's
first drive is captured as the artifact.

```yaml
strategy: installer
bootKeys:
  - keys: ret              # QEMU qcode (ret, esc, spc, tab, …)
    delaySec: 3
    repeat: 6
    intervalSec: 1
waitForShutdownSec: 3600   # max wall-clock seconds to wait
floppy:
  from: ./windows-autounattend.xml   # client reads + inlines
  filename: autounattend.xml         # optional, default: basename of from
provisioners: []           # not used for installer
```

`floppy.from` (path relative to this YAML's directory) is read by the
client, base64-inlined, and turned into a 1.44 MB FAT12 floppy by
the daemon — so editing the answer file and re-running `crv build`
just works, no out-of-band `mkfs.fat`/`mcopy`. The daemon writes the
image to a `__build_<taskId>_*-floppy.img` file under the disk dir
and tears it down with the rest of the bake VM's ephemerals.

The template for an `installer` build typically has
`guestAgent: false` (the build does not depend on QGA), a fresh
40 GiB blank disk via `strategy: create` as its first drive, and the
vendor install ISO + any driver ISO as `media: cdrom` drives. The
floppy itself is per-build content, not part of the template. See
[yaml/windows-server-2025/windows-server-2025.yml](../yaml/windows-server-2025/windows-server-2025.yml)
for a worked Windows Server 2025 example — a self-contained
three-step pipeline (apply ISOs + bake-template → build → apply
runtime-template).

### `overlay` (default)

The bake VM boots from the disks the template defines (typically a
clone or overlay of the template's source). The build picks the **first
attached drive** as the artifact: after provisioners run, that drive is
the source of the publish-time clone (`qemu-img convert` into a fresh
flat qcow2 named by `build.name`). The original ephemeral drive stays
attached to the bake VM until cleanup.

Use this when you want a self-contained derivative of an existing
image — install some packages on top of `debian-12-cloud`, save the
result as `debian-12-nginx`, then use it as a template/base for VMs.

### `from-scratch`

The template's bootdisk is treated as a **bootstrapper** — it provides
the kernel, QGA, and whatever installer (e.g. `debootstrap`) you need.
A separate empty disk is attached as a second drive (e.g. `/dev/vdb`).
Provisioners populate the empty disk; at the end, the second drive is
the source of the publish-time clone.

Use this for stripped-down golden images or images that don't share
content with any existing template (custom kernels, embedded OSes).

## `shellDefaults`

Build-level defaults applied to every `shell:` provisioner. Hoists
boilerplate (`set -eux`, shared env vars) out of every step:

```yaml
shellDefaults:
  preamble: |             # literal shell, prepended verbatim to every step's body
    set -eux
  env:                    # exported before per-step `env:`; step env wins on a clash
    SYSROOT: /mnt/sysroot
    DEBIAN_FRONTEND: noninteractive
```

Composition order for a `shell:` step's command:

1. `shellDefaults.preamble` — runs first so `set -e` propagates into the env exports.
2. `shellDefaults.env` — exported.
3. The step's own `shell.env` — exported AFTER defaults, so a step can override a shared key by re-declaring it.
4. The step's `shell.workdir` — `cd` if set.
5. The step's `inline:` body.

To opt a single step out of the preamble's effect (e.g. a script that
intentionally tolerates failures), prepend `set +e` at the top of its
`inline:`. There's no per-step opt-out flag.

`shellDefaults` does not apply to non-shell provisioners (`file`,
`wait-for`, `reboot`) or to commands run inside heredoc'd sub-shells
like `chroot ... <<'EOF'` — those are separate shell instances.

## Provisioner kinds

### `shell`

Runs a script inside the VM via QGA `guest-exec` against `/bin/sh -c`
(Linux/BSD) or `cmd.exe /c` (Windows guests).

```yaml
- shell: |              # short form: inline string
    apt-get install -y nginx

- shell:                # full form
    inline: "echo hi"   #   OR script: <path> on the client
    workdir: /tmp
    env:
      DEBIAN_FRONTEND: noninteractive
    timeoutSec: 1800    # default: 600 s (10 min)
```

Stdout and stderr go to the build task log; non-zero exit fails the build.

### `file`

Uploads a host file into the VM. Internally: the file is base64-encoded
on the client, streamed over QGA stdin to a `base64 -d > <to>` shell
invocation, and `chmod`-ed to the requested mode.

```yaml
- file:
    from: ./files/nginx.conf   # client reads + base64-inlines
    to:   /etc/nginx/nginx.conf
    mode: "0644"               # optional, default 0644
```

For files larger than ~16 MiB use a tarball + a `shell` provisioner
that extracts it; the file kind is intended for config drops, not bulk
data.

### `wait-for`

Block until a condition holds inside the VM. Useful between commands
that have asynchronous effects (e.g. cloud-init still running, a
service still binding).

```yaml
- wait-for: { ping: true, timeoutSec: 300 }       # guest-agent ping
- wait-for: { file: /var/lib/cloud/instance/boot-finished, timeoutSec: 600 }
- wait-for: { port: 5432, timeoutSec: 120 }       # any local listener on tcp/5432
```

`port:` uses `ss` (with a `netstat` fallback) inside the guest.

### `reboot`

Issue a reboot from inside the guest, then poll until the guest agent
comes back. Use after kernel updates.

```yaml
- reboot: { timeoutSec: 300 }
```

## Cleanup model

Each ephemeral resource (the bake VM, any temporary disks) registers a
destructor when it's created. After provisioning completes — whether
successfully or with a failure — the cleanup pass runs the destructors
in reverse order.

| `cleanup:`    | on success | on failure |
|---------------|------------|------------|
| `always` (default) | runs cleanup | runs cleanup |
| `onSuccess`   | runs cleanup | **leaves resources for inspection** |
| `never`       | leaves resources | leaves resources |

After a successful build the published artifact is a **clone** of the
bake VM's artifact disk (created via `qemu-img convert`, non-ephemeral,
named by the build's `name:` field). The bake VM's original artifact
disk stays attached and ephemeral, so the standard cleanup pass reaps
it together with the bake VM — unless `--build-cache` left cache rows
referencing the bake VM, in which case the cleanup skips the
`VmDelete` and the bake VM survives for future `--use-cache` resumes.

Stranded ephemeral resources are named with the prefix
`__build_<task-id>_…` so they're greppable via `crv vm list` /
`crv disk list`.

## Build-step cache

`crv build` can cache the output of each successful provisioner step
as a qcow2 internal snapshot, keyed by a hash of the step's content
chained through all prior steps and the build envelope. Two
independent flags control the cache; both default to **off**:

| Flag (CLI / YAML) | Effect |
|---|---|
| `--use-cache` / `useCache: true` | If a cached prefix matches the current build's chain, reuse the bake VM, roll its writable disks back to the cached step, and resume from the first unmatched step. |
| `--build-cache` / `buildCache: true` | After each successful step, take an atomic multi-disk live snapshot of the bake VM's writable disks and write a `BuildCacheEntry` row tying that snapshot to the chain hash. The bake VM survives the cleanup pass while any cache row references it. |

The `--rebuild-from N` flag (1-based step index) caps the matched
prefix length so step `N` and everything after it is always
re-executed, even if those steps' content is unchanged. Only
meaningful with `--use-cache`.

```bash
# Prime the cache: every successful step gets a snapshot.
crv build pipeline.yml --wait --build-cache

# Use the primed cache without extending it. Steps whose content
# hasn't changed re-run from the snapshot in ~ms; the first
# unmatched step rebakes from there.
crv build pipeline.yml --wait --use-cache

# Iterative workflow: use existing cache, write new entries for
# steps that hadn't been cached yet.
crv build pipeline.yml --wait --use-cache --build-cache

# Force step 3 and beyond to re-execute even though their content
# matches a cache prefix.
crv build pipeline.yml --wait --use-cache --rebuild-from 3
```

### What goes into the chain hash

Every step's hash is `sha256(canonical-YAML(envelope || step-i))`
folded left-to-right. The envelope captures the YAML fields that
affect what the bake VM **does** (template, target spec, strategy,
VM cpu/ram, shell defaults, boot keys, floppy); it excludes the
operator-policy fields (`name`, `description`, `node`, `cleanup`,
`waitForShutdownSec`, `useCache`, `buildCache`). Per-step inputs
filter out the auto-injected `CORVUS_BAKEVM_ID` /
`CORVUS_BUILD_TASK_ID` / `CORVUS_BUILD_TASK_ID` envs (they change
every invocation and would defeat caching), and the runtime-only
`shell.script` / `file.from` / `floppy.from` fields (always
`Nothing` post-client-inlining).

Editing a provisioner step invalidates that step's chain hash **and
every subsequent step's** — a single character change in step 4 of a
10-step build invalidates steps 4..10 in the cache. Reordering steps
is also a content change (the hash includes the step's position).

### Strategy interactions

- **`overlay`** caches just the artifact disk (one snapshot per step).
- **`from-scratch`** caches both the bake VM's system disk and the
  artifact disk per step, atomically via a QMP `transaction`.
  Provisioner steps can install tools into the bake VM that later
  steps depend on, so the cached state must include the system
  disk's progression too.
- **`installer`** skips the cache layer entirely — no provisioners,
  nothing to snapshot per step.

The overlay and from-scratch strategies already require
`guestAgent: true` on their bake template for provisioner exec; the
cache inherits that precondition since it uses QGA `fsfreeze` to
ensure each cache snapshot is filesystem-consistent.

### `cacheMode` — what each per-step snapshot captures

| Mode | Per-step snapshot | Cache resume |
|---|---|---|
| `memory` (default) | Vmstate (RAM + device + CPU state) **plus** the qcow2 active state, atomically via QMP `snapshot-save`. Carrier disk is the artifact (overlay) or the bake VM's system disk (from-scratch); siblings get block snapshots under the same tag. | Bake VM launched paused (`qemu -S`), QMP `snapshot-load` restores RAM + every disk, `cont` resumes CPUs, QGA `guest-set-time` resyncs the wall clock. The bake VM is in the exact pre-snapshot state — mounts, modules, running daemons, tmpfs contents (e.g. `/tmp`) all intact. |
| `disk` | Just the qcow2 active state per writable disk via `blockdev-snapshot-internal-sync`, bracketed by QGA `fsfreeze` for filesystem consistency. | Bake VM stopped, every disk rolled back with offline `qemu-img snapshot -a`, then `VmStart` reboots. Anything outside the disk (mounts, modules, running daemons, tmpfs) is lost — the provisioner step that runs next sees a fresh boot. |

**When to pick which:**

- **`memory`** is the right default for any multi-step bake whose
  later steps assume the kernel/runtime state set up by earlier
  steps — bind mounts of the sysroot, `losetup`/`dmsetup` handles,
  loaded modules, daemons started but not enabled, tmpfs scratch
  dirs. Without it the bake's implicit contract breaks on
  `--use-cache`. The cost is **≈ RAM-size per cached step** of
  extra qcow2 footprint.
- **`disk`** is the opt-out for builds whose provisioner steps
  establish their own preconditions on every invocation and don't
  rely on any cross-step in-memory state. Smaller cache footprint,
  cheaper to keep around.

Switching modes invalidates the cache: the on-disk artifacts are
not interchangeable, so `cacheMode` is in the envelope hash. A
priming run in `memory` mode and a reuse run in `disk` mode will
miss the cache entirely.

**QEMU 6.0+ required** for `memory` mode — the agent
capability-probes via `query-commands` and refuses with a clear
error on older builds. All modern QEMU packages ship 6.0 or
later (Gentoo `app-emulation/qemu`, Debian stable, the upstream
QEMU containers).

**Standalone `crv disk snapshot rollback`** on a vmstate-aware
snapshot drives the same restore lifecycle. The VM-scoped
`crv vm snapshot rollback` additionally handles the
stopped-VM case (paused-start + snapshot-load + cont).
See [doc/snapshots.md](snapshots.md).

### Lifecycle of a cached bake VM

A bake VM with at least one `BuildCacheEntry` row survives every
`cleanup:` mode — `cleanupBakeVm` consults the cache table and
skips the `VmDelete`. After a successful build the bake VM is
**always powered off** (the publish path issues a graceful `VmStop`
before cloning the artifact); a retained bake VM lives in
`stopped` state until either:

- A subsequent `--use-cache` build starts it again, rolls the
  writable disks back to the matched step, and resumes from there.
- An operator runs `crv vm delete <bake-vm-name>`, which cascades
  the cache rows (`deleteVm` cleans them up explicitly).
- A `crv disk snapshot delete` removes one of the cache snapshots,
  which cascades the matching cache row (`deleteDiskAndSnapshots`).

If `--use-cache` is set but reuse fails for any reason — the cached
VM was deleted out-of-band, a cache snapshot was deleted, the
rollback couldn't take the qcow2 lock — the daemon purges the
stale cache rows pointing at that VM and falls back to a fresh
bake. The operator sees a `cache: reuse failed (...); rebuilding
from scratch` log line and the build proceeds normally.

### Cache snapshots vs. operator snapshots

Cache snapshots are stored alongside the disk's regular qcow2
internal snapshots, named `cache-<first-16-chars-of-chain-hash>`.
They appear in `crv disk show <bake-artifact> --field snapshots`
with `live=True, quiesced=True` columns set. The published
artifact is **always** a flat standalone qcow2 with zero
snapshots — `qemu-img convert` (used by the publish path) copies
only the source's active state and does not preserve internal
snapshots. Operators never see cache snapshots leak into a shipped
artifact.

## Predefined provisioner env

Every `shell:` provisioner runs with a set of `CORVUS_*` environment
variables exported by the daemon — useful for tagging artifacts,
manual provenance, conditional logic on the build's strategy, or
making host-side scripts reachable via vsock from inside the bake VM.

| Variable | Source |
|---|---|
| `CORVUS_VERSION` | the daemon's own version (e.g. `0.9.0.0`) |
| `CORVUS_BUILD_NAME` | the current `build.name` |
| `CORVUS_BUILD_TARGET` | the published artifact name (= `build.name`) |
| `CORVUS_BUILD_TEMPLATE` | the current `build.template` |
| `CORVUS_BUILD_STRATEGY` | `overlay` / `from-scratch` / `installer` |
| `CORVUS_BUILD_TASK_ID` | the parent task id (handy for log correlation) |
| `CORVUS_BAKEVM_ID` | the bake VM's id |
| `CORVUS_BAKEVM_NAME` | the bake VM's name (`__build_<task>_<sanitized>-vm`) |
| `CORVUS_BAKEVM_VSOCK_CID` | bake VM's vsock CID, when allocated |
| `CORVUS_BAKEVM` | full `crv vm show -o json` output for the bake VM, single-line JSON |

The composition order in the assembled shell command is:

1. `shellDefaults.preamble`
2. `CORVUS_*` exports — predefined; injected before user env so step env can override
3. `shellDefaults.env` exports
4. The step's own `shell.env` exports
5. The step's `shell.workdir` (`cd …`)
6. The step's `inline:` body

Later `export`s win in shell, so to override a `CORVUS_*` variable
just re-declare it in `shellDefaults.env` or the step's `env:`.

### Manual provenance file

Earlier versions of Corvus wrote `/etc/corvus-build-info` automatically
as the final provisioner step. That implicit step has been removed —
operators who want it (or a different layout) can add a one-liner step
that uses the predefined env:

```yaml
- shell:
    inline: |
      cat > /etc/corvus-build-info <<EOF
      build_name:      $CORVUS_BUILD_NAME
      build_date:      $(date -Iseconds)
      source_template: $CORVUS_BUILD_TEMPLATE
      corvus_version:  $CORVUS_VERSION
      EOF
```

## Example

Several builds are shipped under [yaml/](../yaml/), grouped by topic
(each subdirectory has the build YAML, its prerequisite apply YAML
when applicable, and any companion files like kernel configs or
answer files):

Overlay builds (use templates from
[yaml/multi-os/multi-os.yml](../yaml/multi-os/multi-os.yml); apply
once before building):

- [yaml/debian-nginx/debian-nginx.yml](../yaml/debian-nginx/debian-nginx.yml) —
  Debian 12 with nginx preinstalled (`debian12` template).
- [yaml/ubuntu-nginx/ubuntu-nginx.yml](../yaml/ubuntu-nginx/ubuntu-nginx.yml) —
  Ubuntu 24.04 LTS with nginx preinstalled (`ubuntu24` template).
- [yaml/gentoo-corvus/gentoo-corvus.yml](../yaml/gentoo-corvus/gentoo-corvus.yml) —
  Gentoo image preloaded with the full Corvus build/test toolchain
  (`gentoo20260412` template). The bake takes ~15 minutes.

From-scratch builds:

- [yaml/gentoo-headless/gentoo-headless.yml](../yaml/gentoo-headless/gentoo-headless.yml) —
  minimal headless Gentoo on an empty target disk, custom kernel
  (BIOS-boot GPT), built by emerging into a sysroot. Bundles the
  kernel `.config` next to the YAML.
- [yaml/alpine-test/alpine-test.yml](../yaml/alpine-test/alpine-test.yml) —
  the integration-test Alpine image (BIOS+UEFI, sshd, qemu-ga,
  vsock-sshd), bootstrapped with `apk-tools-static` inside a Debian
  bake VM.

Installer build:

- [yaml/windows-server-2025/windows-server-2025.yml](../yaml/windows-server-2025/windows-server-2025.yml) —
  Windows Server 2025 with qemu-guest-agent + cloudbase-init,
  installed unattended via the bundled `autounattend.xml` on a
  floppy that the build materialises automatically. Self-contained
  pipeline: the first `apply` step downloads the Windows Server 2025
  evaluation ISO + virtio-win drivers ISO (~9 GiB total) into
  `~/VMs/BaseImages/WindowsServer2025/` on first run, the `build`
  step drives the install, and a final `apply` registers a
  `windows-server-2025` runtime template that overlays the baked
  image for convenient manual testing.

All builds rely on the host having a VDE switch at `/run/vde2/switch.ctl`
(the network type used by every template); the bake VMs need outbound
internet, so the host must NAT/MASQUERADE the VDE subnet and have
`net.ipv4.ip_forward=1`.

## Limitations

- Overlay and from-scratch templates must have `guestAgent: true` and a base
  image with qemu-guest-agent installed. The `installer` strategy does
  not require QGA.
- The bake VM has no networking semantics declared by the build YAML;
  whatever the template configures applies. Networking can be helpful
  inside provisioners (e.g. `apt-get update`); make sure the template
  attaches a NIC if needed.
- File uploads larger than ~16 MiB should use a tarball + `shell`
  extract.
- Pipeline steps run sequentially; a failing step aborts the pipeline.
  Prior successful steps are not rolled back.
- No `via: ssh` for `shell` (QGA only).
- No `ansible:` provisioner kind (use `shell` for now).
