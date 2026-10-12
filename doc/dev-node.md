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
The outer daemon must be running, with the `corvus` managed network and
`corvus-test-node` runtime template available. To prepare a missing test-node
image and template, use `make image-ensure IMAGE=node`; see
[Image builds](test-images.md) for prerequisites and image management.
The VM targets do not build or refresh the test-node image themselves.

```sh
make dev-node-vm
```

This runs four steps in order:

1. Build the Haskell binaries, create the VM, attach shares, and start it.
2. Run `scripts/devnode/setup.sh` inside the VM over VSOCK SSH.
3. Register eligible host base images with the inner daemon.
4. Open an interactive VSOCK SSH session as user `corvus`.

Each step must succeed before the next begins, including with parallel Make.
The web gateway is started separately as described below.

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
`postgresql:///corvus`. Setup verifies authenticated access before quickstart.
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
On a fresh dev node, setup runs `corvus-admin quickstart --skip-web` from
the shared Python source. Quickstart creates the guest admin CA and client
credentials, installs and starts user services for the daemon and nodeagent,
and configures netd as a system service when available. Default quickstart
uses a SQLite database at `~/.local/share/corvus/corvus.db`, rather than the
PostgreSQL instance included in the image. The guest admin store defaults to `~/.config/corvus/admin/`; the host admin store is unaffected.

Setup adds `/opt/corvus/bin` to `PATH` through the host Make target. If the
admin-store directory already exists, the script skips quickstart, verifies
that the configured database backend matches the selection, and checks that
the user `corvus.service` is active. It does not repair an incomplete configuration or verify every component's health.

The image supplies the runtime Python dependencies. Scripts use guest
`python3` with `PYTHONPATH=/mnt/corvus/python`, without a guest virtualenv.

## Individual steps and custom configurations

The steps are also available separately:

| Target | Behavior |
|--------|----------|
| `make dev-node-vm-create` | Build binaries; create, attach shares, and start a new VM. Leave an existing VM alone. |
| `make dev-node-vm-setup` | Run the default setup script in a running VM. |
| `make dev-node-vm-images` | Register host images against an already configured, running inner daemon. |
| `make dev-node-vm-ssh` | Open an interactive guest shell. |

For a non-trivial configuration, start with `dev-node-vm-create` and
`dev-node-vm-ssh` so you can configure the guest before default setup runs.
Use the granular `corvus-admin` commands or quickstart options appropriate
to your test; see [Security](security.md) for certificate and deployment
configuration. The setup script does not forward arguments to quickstart.

If the default setup is appropriate after your manual preparation, run it
inside the VM with the mounted binaries explicitly on `PATH`:

```sh
export PATH=/opt/corvus/bin:$PATH
bash /mnt/corvus/scripts/devnode/setup.sh
# Or use the bundled PostgreSQL database on fresh setup:
# bash /mnt/corvus/scripts/devnode/setup.sh --database postgresql
```

Then run `make dev-node-vm-images` on the host if your configuration supports
the standard registration connection described below. Custom certificate
locations or transports may require a different registration workflow.

`DEV_NODE_VM` overrides the VM name. `DEV_NODE_SSH_KEY` overrides the SSH
key for setup, registration, and interactive SSH; it defaults to
`integration_tests/keys/corvus-test-key`. Setup and interactive SSH also
accept `SSH_ARGS`. Creation, setup, registration, and SSH honor
`CORVUS_CRV` for the outer-daemon CLI executable.

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

Registration requires a running VM and the guest's `ca.crt`,
`corvus-client.crt`, and `corvus-client.key` under
`${XDG_CONFIG_HOME:-$HOME/.config}/corvus/`. It fetches these over SSH into
a private temporary directory, then connects with TLS through the VSOCK
relay on port 9876. Credentials and connections are cleaned up on exit.

The host-side command can also be run from any directory:

```sh
/path/to/corvus/scripts/devnode/register-images.sh --vm corvus-dev-node --ssh-key /path/to/corvus/integration_tests/keys/corvus-test-key
```

It honors `CORVUS_CRV` and creates the checkout's `.venv` with `make venv`
if its Python executable is missing.

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
`corvus` managed network.

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
configuration and data stored there. It uses `crv` from `PATH`; override
`DEV_NODE_VM` if you used a different VM name.

## Troubleshooting

**Setup reports that the inner `corvus.service` is inactive.**
Open a guest shell with `make dev-node-vm-ssh` and inspect:

```sh
systemctl --user status corvus.service
journalctl --user -u corvus.service -n 50
```

If the configuration is valid but the service is stopped, start it with
`systemctl --user start corvus.service`, then retry `make dev-node-vm-setup`
on the host with the matching `DEV_NODE_DATABASE` selection. An existing admin-store directory causes setup to skip
quickstart, even if certificates or units are incomplete. Diagnose those
with `corvus-admin` from the shared source rather than deleting the whole
configuration directory. For a disposable dev node, use
`make dev-node-vm-clean` followed by `make dev-node-vm` to start fresh.

**Image registration fails after setup.**
Check that the VM is running, the guest client certificates exist, and the
inner daemon and `corvus-tcp-relay.service` are available. Missing host image
files or ambiguous local placements also fail registration. Once corrected,
retry `make dev-node-vm-images`; setup does not need to be repeated if the
daemon is already configured and running.

**Browser shows the frontend-not-built 404.**
Run `make web-build` on the host and restart `web.sh` if needed to select the
new assets. Alternatively use the Vite workflow with SSH forwarding above.
See [Web interface](web-interface.md) for the asset pipeline.

**Browser cannot reach `<vm-ip>:8080`.**
Confirm that the VM has an interface on the `corvus` managed network and
check its guest-agent address with `crv vm show corvus-dev-node`. Allow time
for DHCP after boot. Check that `web.sh` is running in the VM; the default
setup does not start a web service.

**Inner daemon does not pick up a host rebuild.**
Restart `corvus.service` in the VM after `make build`. The read-only binary
share reflects host updates, but an already running process continues using
the old executable until restarted.
