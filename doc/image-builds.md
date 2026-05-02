# Image Builds (`crv build`)

`crv build` bakes reusable OS disk images from a procedural YAML
pipeline. It is a separate feature from `crv apply`: apply describes
declarative state and converges to it, build runs an ordered set of
steps and produces a single artifact (a registered Corvus disk).

A build:

1. Instantiates a named template into an ephemeral "bake" VM.
2. Optionally attaches an empty target disk (for `from-scratch`).
3. Starts the bake VM, waits for the guest agent.
4. Runs each provisioner inside the running VM (via `qemu-guest-agent`).
5. Writes `/etc/corvus-build-info` so the artifact is self-identifying.
6. Stops the VM gracefully.
7. Captures the artifact: detaches the chosen drive, renames it, optionally
   flattens (overlay flavor) and compacts.
8. Tears down the bake VM and any other ephemeral resources.

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
builds:
  - name: debian-12-nginx                # required, unique within the file
    description: Debian 12 with NGINX    # optional, free text

    template: debian-12-cloud            # required: name of an existing template

    target:
      name: debian-12-nginx              # registered as a Corvus disk on success
      format: qcow2                      # default: qcow2
      sizeGb: 10                         # only used by from-scratch flavor
      compact: true                      # qemu-img -c rewrite at end (default true)

    flavor: overlay                      # overlay (default) | from-scratch

    vm:
      cpuCount: 4                        # default: 4
      ramMb: 4096                        # default: 4096

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
```

## Flavors

### `overlay` (default)

The bake VM boots from the disks the template defines (typically a
clone or overlay of the template's source). The build picks the **first
attached drive** as the artifact: after provisioners run, that drive is
detached, renamed to `target.name`, **flattened** (so the artifact has
no backing chain), and registered as a Corvus disk.

Use this when you want a self-contained derivative of an existing
image — install some packages on top of `debian-12-cloud`, save the
result as `debian-12-nginx`, then use it as a template/base for VMs.

### `from-scratch`

The template's bootdisk is treated as a **bootstrapper** — it provides
the kernel, QGA, and whatever installer (e.g. `debootstrap`) you need.
A separate empty disk is attached as a second drive (e.g. `/dev/vdb`).
Provisioners populate the empty disk; at the end, the second drive is
detached, optionally compacted, and registered as the artifact.

Use this for stripped-down golden images or images that don't share
content with any existing template (custom kernels, embedded OSes).

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

After a successful build the artifact disk has been **detached** from
the bake VM, so deleting the bake VM (with its still-attached
ephemeral drives) doesn't take the artifact down.

Stranded ephemeral resources are named with the prefix
`__build_<task-id>_…` so they're greppable via `crv vm list` /
`crv disk list`.

## Provenance

Every artifact gets a `/etc/corvus-build-info` file written by the
final implicit provisioner step:

```
build_name: debian-12-nginx
build_date: 2026-04-30T15:21:09Z
source_template: debian-12-cloud
corvus_version: 0.9.0.0
```

## Example

Three overlay builds are shipped, all of which use templates from
[doc/apply-examples/multi-os.yml](apply-examples/multi-os.yml). Apply
that file once on the host (it imports the upstream cloud images,
registers OVMF, and defines per-OS templates with VDE networking and
qemu-guest-agent enabled), then run any of the builds below:

- [doc/build-examples/debian-nginx.yml](build-examples/debian-nginx.yml) —
  Debian 12 with nginx preinstalled (`debian12` template).
- [doc/build-examples/ubuntu-nginx.yml](build-examples/ubuntu-nginx.yml) —
  Ubuntu 24.04 LTS with nginx preinstalled (`ubuntu24` template).
- [doc/build-examples/gentoo-corvus.yml](build-examples/gentoo-corvus.yml) —
  Gentoo image preloaded with the full Corvus build/test toolchain
  (`gentoo20260412` template). The bake takes ~15 minutes.

All three rely on the host having a VDE switch at `/run/vde2/switch.ctl`
(the network type used by every template in `multi-os.yml`); the bake
VMs need outbound internet for cloud-init's package installs, so the
host must NAT/MASQUERADE the VDE subnet and have `net.ipv4.ip_forward=1`.

## Limitations

- Base templates must have `guestAgent: true` and a base image with
  qemu-guest-agent installed.
- The bake VM has no networking semantics declared by the build YAML;
  whatever the template configures applies. Networking can be helpful
  inside provisioners (e.g. `apt-get update`); make sure the template
  attaches a NIC if needed.
- File uploads larger than ~16 MiB should use a tarball + `shell`
  extract.
- Builds are run sequentially in the order they appear in the YAML.
- No `via: ssh` for `shell` (QGA only).
- No `ansible:` provisioner kind (use `shell` for now).
