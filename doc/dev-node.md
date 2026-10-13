# Dev Node

The dev node is a long-lived QEMU VM (`corvus-dev-node`) for manual testing
of the daemon and web UI against your local, uncommitted source tree.
Edit on the host, rebuild binaries when necessary, and restart the relevant
process inside the VM. The shared checkout avoids copying source or
installing a Python wheel after each change.

Unlike the integration-test harness, which creates and tears down test VMs,
the dev node stays around until you explicitly delete it.

## Starting the default dev node

Run commands from the repository root on the host unless stated otherwise.
The outer daemon must be running, with the
`corvus-test-node` runtime template available. Networks referenced by that
template must exist for instantiation (the shipped template references
`corvus`); creation replaces its interfaces before boot. To prepare a missing test-node
image and template, use `make image-ensure IMAGE=node`; see
[Image builds](test-images.md) for prerequisites and image management.
The VM targets do not build or refresh the test-node image themselves.

```sh
make dev-node-vm
```

This runs four steps in order:

1. Build the Haskell binaries, create the VM and its dedicated network, attach shares, and start it.
2. Run `scripts/devnode/setup.sh` on the host, deploying credentials and services over VSOCK SSH.
3. Register eligible host base images with the inner daemon.
4. Open an interactive VSOCK SSH session as user `corvus`.

Each step must succeed before the next begins, including with parallel Make.
The web gateway is started separately as described below.

Each new VM gets a managed network named `corvus-dev-node-<outer-vm-id>`.
Like the integration harness, creation chooses an unused `/24` in
`10.91.0.0/16`, enables DHCP and NAT, and sets DNS servers to `1.1.1.1`
and `8.8.8.8`. Existing networks with overlapping subnets are excluded.
The network starts immediately and has autostart enabled for outer-daemon
restarts. The template's network interface is replaced before the VM boots;
the dev node does not share the integration harness's session network.
Existing VMs keep their current network when reused.

An existing VM is reused: its shares and power state are left alone, though
the binaries are rebuilt and setup and image registration run again. Start
an existing stopped VM before running the full workflow:

```sh
crv vm start corvus-dev-node --wait
make dev-node-vm
```

### Choosing PostgreSQL

SQLite remains the default. To use the PostgreSQL 18 instance included in
the guest image, select it on fresh setup:

```sh
make dev-node-vm DEV_NODE_DATABASE=postgresql
```

For standalone setup, use `make dev-node-vm-setup DEV_NODE_DATABASE=postgresql`.
The selector accepts only `sqlite` and `postgresql`; PostgreSQL connects as
user `corvus` to database `corvus` over the local Unix socket using
`postgresql:///corvus`. Setup verifies authenticated access before deployment.
If PostgreSQL is stopped, inspect or start `postgresql-18.service` in the guest.

Repeat `DEV_NODE_DATABASE=postgresql` whenever rerunning setup or the full
workflow for a PostgreSQL node. Existing configurations must match the
requested backend. A mismatch fails without changing the configuration or
migrating data; recreate a disposable VM or configure it manually to switch.
Creation-only, registration, and SSH targets do not select a database.

## Shared files and services

For a new VM, creation attaches these read-only virtiofs shares:

| Host path | Guest mount | Purpose |
|-----------|-------------|---------|
| `$(stack path --local-install-root)/bin` | `/opt/corvus/bin` | Locally built Haskell binaries, including `corvus`, `corvus-netd`, and `crv`. |
| Repository root | `/mnt/corvus` | Live source, scripts, and frontend assets. |
| `~/VMs/BaseImages` | `/run/baseimages/lower` | Host image files used as the lower layer of the guest's BaseImages overlay. |

The image mounts a writable overlay at `/home/corvus/VMs/BaseImages`.
Its lower layer is the read-only host share; writes go to a guest tmpfs
upper layer under `/run/baseimages`. These writes do not change host files
and disappear when the VM reboots. Keep images that need to survive reboot
outside that temporary upper layer or publish them on the host.

The image includes Corvus system service units but does not enable them.
Host setup creates a private CA and client credentials under
`${TMPDIR:-/tmp}/corvus-devnode-<host-uid>/<outer-vm-id>/`. The VM ID keeps
state separate when names are reused and preserves it across renames.
The CA private key stays in the host `admin/` directory. Only the public CA,
client credentials, and each service's own credentials are deployed to the VM.
Neither the outer daemon's credentials nor the host's normal admin store change.

Setup installs user services for the daemon and nodeagent, a system service
for netd, enables lingering for `corvus` so user services survive SSH logout,
and registers the inner node. SQLite uses
`~/.local/share/corvus/corvus.db`; PostgreSQL uses `postgresql:///corvus`.
The daemon certificate covers both the managed IPv4 address and `127.0.0.1`,
so host network access and guest-local TCP access both work.
Guest client credentials live under `~/.config/corvus/`; the guest has no
admin CA key. The image supplies Python dependencies, and deployment uses
`python3` with `PYTHONPATH=/mnt/corvus/python` for node registration.

Repeated setup preserves the CA, daemon identity, and client credentials.
It checks that the backend, managed address, and guest CA still match,
verifies all three services, and checks a host TLS connection. Failed initial
setup retains pending host state for a retry. An already configured guest
without matching host state requires manual handling or recreation.
Temporary-directory cleanup can remove the host credentials while the VM
still exists; keep that state for the VM's lifetime or recreate the disposable
node when it is lost.

## Using the CLI from the host

After setup, use the shared helpers with a VM name or numeric outer VM ID:

```sh
scripts/crv-it corvus-dev-node status
scripts/crv-it corvus-dev-node vm list
scripts/ssh-it corvus-dev-node
```

`crv-it` connects over TCP with the host client credentials and runs
`$(stack path --local-install-root)/bin/crv` from this checkout. Run
`make build` after CLI changes; the helper does not build automatically.
Outer VM lookup uses `CORVUS_CRV` or `crv` from `PATH`. `ssh-it` uses network
SSH with the key recorded during setup; `DEV_NODE_SSH_KEY` can override it.
Both helpers also support integration-test nodes while their credentials
and VMs remain available; see [Integration tests](../integration_tests/README.md).
They work from any directory when invoked by an absolute path.

Inside the guest, the CLI remains available:

```sh
/opt/corvus/bin/crv status
/opt/corvus/bin/crv --host 127.0.0.1 status
```

## Individual steps and custom configurations

The steps are also available separately:

| Target | Behavior |
|--------|----------|
| `make dev-node-vm-create` | Build binaries; create a dedicated network and VM, attach shares, and start it. Leave an existing VM alone. |
| `make dev-node-vm-setup` | Run the default setup script in a running VM. |
| `make dev-node-vm-images` | Register host images against an already configured, running inner daemon. |
| `make dev-node-vm-ssh` | Open an interactive guest shell. |

For a non-trivial configuration, start with `dev-node-vm-create` and
`dev-node-vm-ssh` so you can configure the guest before default setup runs.
Use the granular `corvus-admin` commands or quickstart options appropriate
to your test; see [Security](security.md) for certificate and deployment
configuration. For example, inside an unconfigured guest:

```sh
export PATH=/opt/corvus/bin:$PATH
PYTHONPATH=/mnt/corvus/python python3 -m corvus_admin quickstart --skip-web
```

Choose the deployment and database options needed for your test. This manual
quickstart creates its admin store in the guest; default host setup cannot
adopt it.

`scripts/devnode/setup.sh` runs on the host. It is not a manual guest
quickstart wrapper. For custom guest deployments, use `corvus-admin` from the
shared source with `/opt/corvus/bin` on `PATH`. Custom deployments need their
own host client credentials and connection or registration workflow; the
shared helpers require the managed host state created by default setup.

For standalone host setup:

```sh
scripts/devnode/setup.sh --vm corvus-dev-node --database sqlite
```

`DEV_NODE_VM` overrides the VM name for Make targets. `DEV_NODE_SSH_KEY`
overrides the key used by setup and VSOCK SSH; it defaults to
`integration_tests/keys/corvus-test-key`. `SSH_ARGS` applies only to
`dev-node-vm-ssh`. All VM targets and host helpers honor `CORVUS_CRV` for
outer-daemon access. `STACK_BUILD_FLAGS` applies to host builds and the
helpers' Stack install-root lookup.

## Registering base images

`make dev-node-vm` includes registration. To repeat just that step, for
example after publishing a new host image, run:

```sh
make dev-node-vm-images
```

The integration harness selects outer image versions tagged `latest` with
local `.qcow2`, `.img`, or `.raw` files under `~/VMs/BaseImages`. Unregistered
files and retained versions without `latest` are ignored. It registers the
shared guest paths and prints each image name, inner ID, and backing path.
Unchanged backing paths reuse the current inner version; changed paths
publish a new version while retaining older versions and VM references.
Harness directory aliases resolve to the same image and do not create
additional registrations. An empty selection succeeds without building or
downloading images, but still requires a working connection to the guest daemon.

Registration requires a running VM and ready host client credentials from
setup. It uses those credentials through the VSOCK relay on port 9876;
it does not fetch credentials from the guest or delete the host certificate
state on exit. Relay connections close after registration.

The host-side command can also run from any directory:

```sh
/path/to/corvus/scripts/devnode/register-images.sh --vm corvus-dev-node
```

Host creation, setup, registration, cleanup, and shared helpers create the checkout's
`.venv` with `make venv` if its Python executable is missing.

## Creating nested test VMs

The [dev-node template library](../yaml/dev-node/dev-node.yml) provides reusable
VM templates for the bootable images in the current dev-node catalog. It includes
Debian, Ubuntu, AlmaLinux, FreeBSD, Alpine, the standard Alpine integration-test
VM, the Gentoo variants and integration-test node, Windows Server 2025, and both
Windows 11 images. It excludes installer media and does not download or build
images. Applying it creates templates and supporting resources, not VMs.

After `make dev-node-vm`, run on the host:

```sh
# Register the existing integration-test public key with the INNER daemon.
# Run this once; an existing dev-node key can be reused on subsequent applies.
scripts/crv-it corvus-dev-node ssh-key create dev-node \
  "$(cat integration_tests/keys/corvus-test-key.pub)"
scripts/crv-it corvus-dev-node apply yaml/dev-node/dev-node.yml --wait
scripts/crv-it corvus-dev-node network start dev-node-vms

scripts/crv-it corvus-dev-node template instantiate corvus-test-vm test-alpine
scripts/crv-it corvus-dev-node vm start test-alpine --wait
scripts/crv-it corvus-dev-node vm exec test-alpine 'cat /etc/os-release'
```

Use your selected `DEV_NODE_VM` name in place of `corvus-dev-node`. To inject
a different key into cloud-init guests, pass its public key to `ssh-key create`.
The Alpine integration image and `corvus-test-node` retain their baked-in key;
changing the registered key does not change those images.

Template names match image families, except `windows-11-pro` uses
`windows-11-pro-base`. List the available templates with
`scripts/crv-it corvus-dev-node template list`. Image selectors are unversioned,
so each new VM uses the latest version registered inside the dev node. Re-run
`make dev-node-vm-images` after publishing replacement host images.

This is a static catalog: remove template entries for images you have not built
and registered, or prepare those images first. Missing images cause apply to
fail; resources created before a failure remain available for retry. Applying
again skips existing resources. To update a template definition, edit it with
`template edit`, or delete it with `template delete` before reapplying the file.
No setup or Make target applies this library automatically.

The YAML registers the dev node's installed OVMF firmware and creates a nested
managed network named `dev-node-vms` on `10.92.0.0/24`, with DHCP, NAT, and DNS.
Start it explicitly after the first apply; autostart handles later daemon
restarts. This network lives **inside** the dev node and is separate from the
outer `corvus-dev-node-<id>` network and the baked VDE network. Choose another
subnet in the YAML before applying if it overlaps your local networks.

Each VM gets an overlay root disk at the base image's original size. UEFI VMs
also get an independent writable firmware variables clone; firmware code is
shared read-only. Defaults are 2 CPUs and 2 GiB RAM, except the Alpine
integration VM (1 GiB) and Windows (4 CPUs, 8 GiB). Run larger guests one at a
time within the dev node's memory budget.

### Guest access

Cloud-init Linux and FreeBSD templates create user `corvus`, inject the
`dev-node` SSH key, and set the test password to `corvus`. Linux templates use
baked QGA or install and start it, except upstream `gentoo-cloud`: its template
disables guest-agent readiness because the image does not include QGA.
Install and configure QGA manually there, then enable the VM's guest-agent
setting while stopped if you need `vm exec`.

FreeBSD uses
[nuageinit](https://github.com/freebsd/freebsd-src/blob/releng/14.3/libexec/nuageinit/nuageinit),
which does not process cloud-init `packages` or
`runcmd`; its template also disables QGA readiness and keeps a graphical console.
The FreeBSD user belongs to `wheel`; Linux's sudo configuration is not applied.
Its first boot can run `freebsd-update` before starting SSH. With QGA disabled,
`vm start --wait` confirms the QEMU process, not completion of guest setup;
watch the console and allow time for updates and SSH startup.
Install and configure QGA manually in FreeBSD, then enable the VM's
guest-agent setting while stopped if you need `vm exec`.

On Debian's first boot, QGA can be installed and running while Corvus still
reports `starting`. After cloud-init finishes, stop the VM with `--wait` and
start it again; SSH remains available during that initial provisioning.

Gentoo images without cloud-init retain their baked accounts; use `vm exec`
to configure an account if needed. The standard Alpine integration template
uses baked SSH access and QGA without cloud-init.

Inspect a guest's address with `scripts/crv-it corvus-dev-node vm show <vm>`
when QGA is available. For guests without QGA, use their console or inspect
`/var/lib/corvus/netd/leases/corvus-br-<network-id>.leases` inside the dev node;
`network show dev-node-vms` reports the network ID.
The nested subnet is reachable from inside the dev node. To use TCP SSH from
the host, tunnel through the dev node, for example:

```sh
# Replace <nested-ip> with the address reported for the nested VM.
make dev-node-vm-ssh SSH_ARGS='-N -L 127.0.0.1:2222:<nested-ip>:22'
# In another host terminal:
ssh -i integration_tests/keys/corvus-test-key -p 2222 corvus@127.0.0.1
```

Windows Server uses Cloudbase-Init and sets `Administrator`'s password to
`corvus`. Windows 11 does not use cloud-init; its first boot recreates the
`corvus` account with password `corvus`. Windows templates keep graphical
consoles and QGA; Windows 11 also uses Secure Boot firmware and independent
TPM state. Guest-agent readiness can precede completion of Windows first-boot
setup. See [Windows 11](../yaml/windows-11/README.md) for console access and
first-boot details.

Stop and delete nested VMs through the inner daemon:

```sh
scripts/crv-it corvus-dev-node vm stop test-alpine --wait
scripts/crv-it corvus-dev-node vm delete test-alpine
```

Deletion removes the VM's ephemeral overlays and firmware clones. The registered
base images, firmware, templates, and nested network remain for future use.

## Running the web UI

After the default workflow opens SSH, run inside the VM:

```sh
bash /mnt/corvus/scripts/devnode/web.sh
```

This runs the gateway from the shared Python source, binding to
`0.0.0.0:8080`. It uses `frontend/dist/` if that directory contains
`index.html`, otherwise `python/corvus_web/static/` in the shared checkout.
Run `make web-build` on the host to populate both locations. If the selected
assets are missing, the gateway serves the API but shows a frontend-not-built
404 at the SPA entry point.

The script prints a URL using the first IPv4 address on an `enp*` interface.
You can also inspect the VM's network configuration and guest-agent addresses
on the host:

```sh
crv vm show corvus-dev-node
```

Open `http://<vm-ip>:8080/` using the address reachable from the host on the
dev node's dedicated managed network.

### Overrides

The script forwards extra arguments to `corvus-web` and reads these variables:

| Variable | Default | Effect |
|----------|---------|--------|
| `CORVUS_WEB_PORT` | `8080` | HTTP bind port. |
| `CORVUS_WEB_LOG_LEVEL` | `debug` | Gateway log level. |

By default the gateway connects to the user daemon's Unix socket. To use
the daemon's local TCP listener instead:

```sh
bash /mnt/corvus/scripts/devnode/web.sh --daemon-host 127.0.0.1
```

## Iteration loop

| Change | Action |
|--------|--------|
| Python gateway or client source | Stop `web.sh` with Ctrl-C and rerun it. Automatic reload is disabled. |
| Haskell daemon code | Run `make build` on the host, then `systemctl --user restart corvus.service` in the VM. Restart other changed components as needed; netd uses `sudo systemctl restart corvus-netd.service`. |
| Frontend source | Run `make web-build` on the host and refresh the browser. If `web.sh` was started before `frontend/dist/index.html` existed, restart it to select that directory. |

For frontend HMR, `make web-dev` runs Vite on the host. Its `/api` and `/ws`
proxies are fixed to `127.0.0.1:8080`, so first forward that host port to the
VM gateway in another host terminal:

```sh
make dev-node-vm-ssh SSH_ARGS='-N -L 127.0.0.1:8080:127.0.0.1:8080'
```

With `web.sh` running in the VM and host port 8080 available, run
`make web-dev` and open the Vite URL. Install frontend dependencies first if
needed; see [Web interface](web-interface.md).

## Teardown

On the host:

```sh
make dev-node-vm-clean
```

This force-deletes the VM and its overlay disk, including the guest
configuration and data stored there. After successful deletion it removes
its dedicated network and associated host PKI. If VM or network deletion
fails, host state remains so cleanup can be retried, even after the VM is gone.
It honors `CORVUS_CRV`; override `DEV_NODE_VM` for a different VM name.
Deleting a VM directly with `crv` leaves the dedicated network and host state
behind. Run the cleanup target with its original name (or numeric VM ID) to
remove these resources. If the temporary host state has been lost, remove the
`corvus-dev-node-<outer-vm-id>` network manually after confirming that VM is gone.

## Troubleshooting

**Setup reports that the inner `corvus.service` is inactive.**
Open a guest shell with `make dev-node-vm-ssh` and inspect:

```sh
systemctl --user status corvus.service
journalctl --user -u corvus.service -n 50
```

If the configuration is valid but the service is stopped, start it with
`systemctl --user start corvus.service`, then retry `make dev-node-vm-setup`
on the host with the matching `DEV_NODE_DATABASE` selection. Inspect nodeagent
and netd units as well if their checks fail. For a disposable dev node, use
`make dev-node-vm-clean` followed by `make dev-node-vm` to start fresh.

**Host credentials are missing or setup reports a CA/backend/IP mismatch.**
Do not replace the CA on a configured VM. Recreate a disposable node using
the cleanup target, or manage a custom configuration and host client credentials
manually. Changing `DEV_NODE_DATABASE` does not migrate existing data.

**Image registration fails after setup.**
Check that the VM is running, the host client certificate directory exists, and the
inner daemon and `corvus-tcp-relay.service` are available. Missing host image
files or ambiguous local placements also fail registration. Once corrected,
retry `make dev-node-vm-images`; setup does not need to be repeated if the
daemon is already configured and running.

**Browser shows the frontend-not-built 404.**
Run `make web-build` on the host and restart `web.sh` if needed to select the
new assets. Alternatively use the Vite workflow with SSH forwarding above.
See [Web interface](web-interface.md) for the asset pipeline.

**Browser cannot reach `<vm-ip>:8080`.**
Confirm that the VM has an interface on the dev node's dedicated managed network and
check its guest-agent address with `crv vm show corvus-dev-node`. Allow time
for DHCP after boot. Check that `web.sh` is running in the VM; the default
setup does not start a web service.

**Inner daemon does not pick up a host rebuild.**
Restart `corvus.service` in the VM after `make build`. The read-only binary
share reflects host updates, but an already running process continues using
the old executable until restarted.
