# Web Interface

Corvus ships with a browser UI served by the `corvus-web` HTTP gateway. It mirrors what `crv` exposes today: a dashboard, VM/disk/network management, a live task feed, a YAML editor for `crv apply`, and an in-browser serial console for headless VMs.

`corvus-web` is a separate process from the daemon. It uses the Python `corvus_client` library to speak Cap'n Proto to the daemon and serves a React SPA over HTTP, plus a small REST + WebSocket bridge for the SPA to talk to.

## Architecture

```
browser ──HTTP/WS──► corvus-web (Python, FastAPI)
                         │ pycapnp
                         ▼
                      corvusd (Haskell, Cap'n Proto RPC)
```

One persistent `AsyncClient` is opened in the FastAPI lifespan and reused across requests. WebSockets in `corvus-web` bridge the daemon's existing streaming sinks (task progress, guest-agent reachability, serial-console bytes) to the browser.

## Running

After `make install`, run:

```sh
corvus-web
```

Defaults:

- HTTP bind: `127.0.0.1:8080` (loopback only). The corvus family of RPC ports (9876 daemon, 9877 netd, 9878 nodeagent) all carry Cap'n Proto traffic; the web UI sits on the conventional 8080 instead.
- Daemon transport: `$XDG_RUNTIME_DIR/corvus/corvus.sock` (same default as `crv`).
- Frontend assets: bundled inside the wheel under `corvus_web/static/`.

Open `http://127.0.0.1:8080` in a browser.

### Useful flags

| Flag | Default | Notes |
|---|---|---|
| `--bind-host` | `127.0.0.1` | Stay localhost-only in v1 — there is no authentication yet. |
| `--bind-port` | `8080` | Conventional HTTP UI port. The daemon/netd/nodeagent family lives on 9876–9878. |
| `--daemon-socket` | `$XDG_RUNTIME_DIR/corvus/corvus.sock` | Unix socket to the daemon. |
| `--daemon-host`, `--daemon-port` | — | Connect to the daemon over TCP instead. Mutually exclusive with `--daemon-socket`. |
| `--daemon-tls` / `--no-daemon-tls` | auto | TLS auto-on for TCP, off for Unix. `--no-daemon-tls` mirrors `crv --no-tls`. |
| `--daemon-cert-dir` | corvus-admin search path | Client cert/key/CA bundle for TLS. |
| `--frontend-dir` | bundled `static/` | Override to point at `frontend/dist/` for dev. |
| `--log-level` | `info` | One of `critical`, `error`, `warning`, `info`, `debug`, `trace`. |

## v1 scope

What works today:

- Dashboard with live daemon status, uptime, connection count.
- Full list / detail / lifecycle for VMs, disks, networks, SSH keys, templates, cloud-init configs (mirroring `crv … list`/`show`).
- `crv apply` from the browser via a Monaco-hosted YAML editor.
- Task history + live task progress over WebSocket.
- In-browser serial console for headless VMs (xterm.js over WebSocket); ring-buffer replay on reconnect.
- In-browser graphical console for non-headless VMs ([spice-html5](https://gitlab.freedesktop.org/spice/spice-html5) over WebSocket). See "Graphical console" below for the limitations.

What v1 does **not** include:

- **No authentication.** The gateway is localhost-only by design; tunnel via SSH for remote use. Auth lands in v2.
- **No build-pipeline UI** (`Daemon.build` / `crv build`).
- **No multi-user RBAC.**

## Graphical console (SPICE)

Click **Graphical console** on the detail page of a non-headless running VM. The gateway:

1. Calls the daemon's `viewGrant` (the same RPC `crv vm view` uses), which generates a fresh 120-second password and pushes it into QEMU via QMP `set_spice_password`.
2. Stashes the SPICE host:port server-side under a single-use opaque session id and returns the password + session id to the browser.
3. The browser opens a WebSocket to `/api/vms/{id}/spice/ws?session=<token>`. The gateway proxies binary frames to QEMU's SPICE TCP port; `spice-html5` runs the SPICE handshake against the previously-set ticket.

Operational constraints:

- **The gateway must run on the same host as the daemon.** QEMU binds the SPICE port to the daemon's configured bind address (defaults to `127.0.0.1`), so the WebSocket-to-TCP proxy needs local network reach to it. The same constraint already applies to `crv vm view`.
- **The password never appears in a URL.** The session token in the WS query string is a one-shot opaque value; the actual SPICE password is delivered in a JSON response body.
- **Sessions die with the gateway process.** Outstanding session tokens are kept in process memory; restarting `corvus-web` invalidates them. The browser just calls `POST /api/vms/{id}/spice` again on the next user interaction.

Known limitations vs. `crv vm view`:

- No audio.
- No USB redirect.
- No 3D acceleration.
- Locale-specific keyboard keys may misbehave (non-US layouts).
- No clipboard or file transfer (could be enabled later — spice-html5 has partial support behind feature flags).

For serious work on a graphical guest, use `crv vm view`. The in-browser console is for quick fixes, install screens, and BIOS-level interaction.

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

`make lint` and `make format` invoke the web variants automatically when `frontend/node_modules/` is present.

## Packaging

`make web-build` runs `npm ci && npm run build` in `frontend/` and copies the output into `python/corvus_web/static/`. The Python wheel ships those static assets via `package_data` in [`pyproject.toml`](../pyproject.toml). `make install` and `make release` both invoke `web-build` automatically when `npm` is on PATH; operators without Node still get a working daemon + CLI, and `corvus-web` returns a friendly 404 message until the UI is built.
