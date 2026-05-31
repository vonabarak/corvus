# Dev Node

The dev node is a long-lived QEMU VM (`corvus-dev-node`) used for
manual testing of the daemon and the web UI against your local,
uncommitted source tree. Edit on the host, restart the relevant
process inside the VM, see the change — no image rebuild, no
deploy step, no installed wheel.

This is distinct from the integration-test harness, which boots a
fresh test-node VM per class and tears it down afterwards. The dev
node is your single, sticky workspace; you bring it up once and
keep it around until you want a clean slate.

## How it works

`make dev-node-vm` instantiates the `corvus-test-node` template
under the name `corvus-dev-node` and attaches three virtiofs
shares:

| Host path                       | Guest mount             | RO/RW | Purpose                                                                 |
|---------------------------------|-------------------------|-------|-------------------------------------------------------------------------|
| `stack path --local-install-root`/bin | `/opt/corvus/bin`       | RO    | The Haskell binaries you just built (`corvus`, `corvus-netd`, `crv`, …). |
| repo root                       | `/mnt/corvus`           | RO    | The full source tree, including `python/` for live `corvus_web` / `corvus_admin`. |
| `~/VMs/BaseImages`              | `/home/corvus/VMs/BaseImages` | RO | Host's base-image library, for nested-VM testing.                       |

The systemd services for `corvus`, `corvus-netd`, and
`corvus-nodeagent` are **installed but not enabled** in the image.
The image waits for an admin CA to be bootstrapped before the
units come up — that's what
[`scripts/devnode/setup.sh`](../scripts/devnode/setup.sh) does.

The image also ships every runtime Python dep `corvus_web` and
`corvus_admin` need (fastapi, uvicorn, prometheus-client, click,
cryptography, jinja2, pyyaml, pycapnp), so there's no venv inside
the VM — `python3` with `PYTHONPATH=/mnt/corvus/python` imports the
live source directly.

## One-time setup

On the **host**, once per dev-node lifetime:

```sh
make dev-node-vm
make dev-node-vm-ssh
```

The first command boots the VM (idempotent — leaves an existing
`corvus-dev-node` alone). The second drops you into a shell as user
`corvus` over SSH-over-VSOCK.

Inside the VM, once per dev-node lifetime:

```sh
bash /mnt/corvus/scripts/devnode/setup.sh
```

This bootstraps an admin CA at `~/.config/corvus/admin/` (inside
the VM — the host's admin store is left alone) by running
`corvus-admin quickstart --skip-web`, which mints the daemon /
nodeagent / netd certs and starts the corresponding user-systemd
services. `--skip-web` is intentional — we run `corvus-web` from
the live source, not the wheel.

The script is idempotent: re-running it on an already-set-up VM
just verifies the inner `corvus.service` is active.

## Running the web UI

In the SSH session inside the VM:

```sh
bash /mnt/corvus/scripts/devnode/web.sh
```

This `exec`s `corvus-web` from the live source via
`PYTHONPATH=/mnt/corvus/python`, binding to `0.0.0.0:8080` on every
interface — including the VM's address on the `corvus` managed
network, which is routed from the host. The script picks the best
available SPA dir: `frontend/dist/` if you ran `make web-build` on
the host, otherwise the wheel-bundled `python/corvus_web/static/`
(which 404s with a helpful hint when empty).

Find the VM's address on the host (look for the managed-network
interface — `crv vm show` reports both the static config and the
guest-agent observed IP):

```sh
crv vm show corvus-dev-node
```

Then open `http://<vm-ip>:8080/` in your host browser.

### Overrides

The script forwards extra args to `corvus-web` and reads two env
vars:

| Env var               | Default | Effect                                          |
|-----------------------|---------|-------------------------------------------------|
| `CORVUS_WEB_PORT`     | `8080`  | `--bind-port`.                                  |
| `CORVUS_WEB_LOG_LEVEL`| `debug` | `--log-level`.                                  |

For example, to point at the daemon over its TCP relay instead of
the user-systemd Unix socket:

```sh
bash /mnt/corvus/scripts/devnode/web.sh --daemon-host 127.0.0.1
```

## Iteration loop

| You changed                    | Do this                                                                |
|--------------------------------|------------------------------------------------------------------------|
| Python under `python/corvus_web/` | Ctrl-C `web.sh` and re-run it. (`uvicorn --reload` is disabled — see [Makefile:434-438](../Makefile#L434-L438).) |
| Haskell daemon code            | `make build` on the host, then `systemctl --user restart corvus.service` in the VM. The `/opt/corvus/bin` mount means no copy is needed. |
| Frontend (`frontend/`)         | `make web-build` on the host (writes to `frontend/dist/`); refresh the browser. For live HMR, run `make web-dev` on the host instead, pointed at the VM's `:8080`. |

## Teardown

```sh
make dev-node-vm-clean
```

Resets and deletes the VM plus its overlay disk.

## Troubleshooting

**`setup.sh` says the inner `corvus.service` isn't active.**  
Check `systemctl --user status corvus.service` and
`journalctl --user -u corvus -n 50`. Most often this is a cert
issue — the admin store at `~/.config/corvus/admin/` is missing or
half-written. Wipe it and re-run `setup.sh`:

```sh
rm -rf ~/.config/corvus
bash /mnt/corvus/scripts/devnode/setup.sh
```

**Browser shows the SPA's "frontend not built" 404.**  
You haven't run `make web-build` on the host yet. Either run it
(see [web-interface.md](web-interface.md) for the full asset
pipeline) or just use `make web-dev` on the host while the VM
serves the API.

**Browser can't reach `<vm-ip>:8080`.**  
Confirm the VM has an interface on the `corvus` managed network
(`crv vm show corvus-dev-node`) and that the host can ping that
address. If the IP only appears under the guest-agent section,
wait a few seconds after `make dev-node-vm` for the DHCP lease to
land. Make sure you ran `bash /mnt/corvus/scripts/devnode/web.sh`
inside the VM — `corvus-web` isn't a service.

**Inner daemon doesn't pick up a host rebuild.**  
You forgot the `systemctl --user restart corvus.service` step. The
`/opt/corvus/bin` mount is read-only from the guest but the host's
stack output changes underneath it — restart re-execs the
already-replaced binary.
