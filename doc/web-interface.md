# Web Interface

Corvus ships with a browser UI served by the `corvus-web` HTTP gateway. The
web interface is highly experimental and lacks much of the functionality
available in the `crv` CLI. It provides a dashboard, VM/disk/network management,
a live task feed, a YAML editor for `crv apply`, and browser consoles. Use `crv`
for operations the web interface does not expose.

`corvus-web` is a separate process from the daemon. It uses the Python `corvus_client` library to speak Cap'n Proto to the daemon and serves a React SPA over HTTP, plus a small REST + WebSocket bridge for the SPA to talk to.

## Daemon connection lifecycle

The HTTP gateway listens even if the daemon is unavailable at startup. It
establishes a daemon connection in the background, verifies it with a ping,
and replaces it after a transport disconnect. Each connection attempt has a
five-second timeout. Failed attempts retry after 1, 2, 4, 8, 16, then 30 seconds;
the delay resets after a successful connection. Unix sockets and TCP/mTLS use
the same lifecycle and retain the configured transport and certificate checks.

Daemon-dependent REST requests return HTTP 502 during an outage. Failed RPCs
are never replayed automatically: the daemon may already have accepted a
mutating request before its response was lost. Later requests use the new
connection. Static UI resources remain available.

Daemon-backed serial, guest-agent, VM statistics, and task-progress WebSockets
close with code 1013 when their daemon session is unavailable or disconnects,
even when no events are being sent. Clients must establish a new subscription
after recovery. Existing SPICE TCP bridges have their own transport lifetime.
The metrics endpoint serves a short cache grace period as described in
[Observability](observability.md#daemon-outages-and-scrape-availability).

## Architecture

```
browser ──HTTP/WS──► corvus-web (Python, FastAPI)
                         │ pycapnp
                         ▼
                      corvus (Haskell, Cap'n Proto RPC)
```

The FastAPI lifespan supervises one daemon session at a time and replaces its `AsyncClient` after a disconnect. WebSockets in `corvus-web` bridge the daemon's existing streaming sinks (task progress, guest-agent reachability, serial-console bytes) to the browser.

## Running

After `make install`, run:

```sh
corvus-web
```

Defaults:

- HTTP bind: `127.0.0.1:8080` (loopback only). The corvus family of RPC ports (9876 daemon, 9877 netd, 9878 nodeagent) all carry Cap'n Proto traffic; the web UI sits on the conventional 8080 instead.
- Daemon transport: `$XDG_RUNTIME_DIR/corvus/corvus.sock` (use `crv --unix` for the same transport).
- Frontend assets: bundled inside the wheel under `corvus_web/static/`.

Open `http://127.0.0.1:8080` in a browser.

### Useful flags

| Flag | Default | Notes |
|---|---|---|
| `--bind-host` | `127.0.0.1` | Keep localhost-only — there is no authentication yet. |
| `--bind-port` | `8080` | Conventional HTTP UI port. The daemon/netd/nodeagent family lives on 9876–9878. |
| `--daemon-socket` | `$XDG_RUNTIME_DIR/corvus/corvus.sock` | Unix socket to the daemon. |
| `--daemon-host`, `--daemon-port` | — | Connect to the daemon over TCP instead. Mutually exclusive with `--daemon-socket`. |
| `--daemon-tls` / `--no-daemon-tls` | auto | TLS auto-on for TCP, off for Unix. `--no-daemon-tls` mirrors `crv --no-tls`. |
| `--daemon-cert-dir` | corvus-admin search path | Client cert/key/CA bundle for TLS. |
| `--frontend-dir` | bundled `static/` | Override to point at `frontend/dist/` for dev. |
| `--log-level` | `info` | One of `critical`, `error`, `warning`, `info`, `debug`, `trace`. |

## Current scope

What works today:

- Dashboard with live daemon status, uptime, connection count.
- Resource lists and details for VMs, disks, networks, SSH keys, templates, and cloud-init configs, with selected management and VM lifecycle operations.
- `crv apply` from the browser via a Monaco-hosted YAML editor.
- Task history + live task progress over WebSocket.
- In-browser serial console for headless VMs (xterm.js over WebSocket); ring-buffer replay on reconnect.
- In-browser graphical console for non-headless VMs ([spice-html5](https://gitlab.freedesktop.org/spice/spice-html5) over WebSocket). See "Graphical console" below for the limitations.

Limitations:

- **No authentication.** The gateway is localhost-only by design; tunnel via SSH for remote use. Do not expose it directly to untrusted clients.
- **No build-pipeline UI** (`Daemon.build` / `crv build`).
- **No multi-user RBAC.**

## Graphical console (SPICE)

Click **Graphical console** on the detail page of a non-headless running VM. The gateway:

1. Calls the daemon's `viewGrant` (the same RPC `crv vm view` uses), which generates a fresh 120-second password and pushes it into QEMU via QMP `set_password`.
2. Stashes the SPICE host:port server-side under a VM-scoped opaque session id, valid for one hour and returns the password + session id to the browser.
3. The browser opens a WebSocket to `/api/vms/{id}/spice/ws?session=<token>`. The gateway proxies binary frames to QEMU's SPICE TCP port; `spice-html5` runs the SPICE handshake against the previously-set ticket.

Operational constraints:

- **The gateway must reach the VM node's SPICE host and port.** It need not share a host with the daemon. Configure the daemon's `--spice-bind` setting and network routing so the address returned by `viewGrant` is reachable; see [VM display access](vm-management.md#spice-graphical-vms).
- **The password never appears in a URL.** The session token in the WS query string is an opaque value reused for the SPICE channels of that session; the actual SPICE password is delivered in a JSON response body.
- **Sessions die with the gateway process.** Outstanding session tokens are kept in process memory; restarting `corvus-web` invalidates them. The browser just calls `POST /api/vms/{id}/spice` again on the next user interaction.

Known limitations vs. `crv vm view`:

- SPICE playback requires WebM/Opus support in the browser. Click **Enable sound** in the console while the VM is playing audio to allow playback if the browser blocks autoplay.
- No USB redirect.
- No 3D acceleration.
- Locale-specific keyboard keys may misbehave (non-US layouts).
- No clipboard or file transfer (could be enabled later — spice-html5 has partial support behind feature flags).
- No SPICE folder sharing. Use a native viewer opened through `crv vm view` or the desktop application; see [SPICE folder sharing](shared-directories.md#spice-folder-sharing).

For serious work on a graphical guest, use `crv vm view`. The in-browser console is for quick fixes, install screens, and BIOS-level interaction.

To capture SPICE audio state and media events without opening browser DevTools,
install Firefox, geckodriver, and Python's `selenium` package. With `corvus-web`
running and the guest producing sound, run:

```sh
python scripts/debug_spice_audio.py <vm-id> --firefox-binary /opt/firefox/firefox
```

The probe opens a headless Firefox session, clicks **Enable sound**, and prints
playback state and events once per second. Use `--url` if the gateway is not at
`http://127.0.0.1:8080`.

## Development

Two-terminal workflow:

```sh
# Terminal 1: gateway against the running daemon
corvus-web --frontend-dir frontend/dist

# Terminal 2: vite dev server with HMR (proxies /api and /ws to :8080)
make web-dev
```

Open the vite dev URL (`http://127.0.0.1:5173`); the SPA hot-reloads on every save.

Frontend lint/format/typecheck:

```sh
make web-lint     # eslint + prettier --check + tsc --noEmit
make web-format   # prettier --write
```

`make check` and `make format` invoke the web variants automatically when `frontend/node_modules/` is present.

## Packaging

`make web-build` installs frontend dependencies and runs `npm run build` in `frontend/` and copies the output into `python/corvus_web/static/`. The Python wheel ships those static assets via `package_data` in [`pyproject.toml`](../pyproject.toml). `make release` requires npm and builds the assets before packaging. `make install` builds them when npm is on PATH; without it the daemon and CLI still install, but the browser UI is unavailable unless assets were already built. Released wheels include the assets.
