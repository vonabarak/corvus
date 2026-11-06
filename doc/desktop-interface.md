# Desktop interface

`corvus-desktop` is a native Qt application using the Python Cap'n Proto client.
It connects directly to the daemon; it does not require `corvus-web`. Set up the
daemon and agents with the [installation guide](README.md) first.

The desktop client is highly experimental and is not yet fully featured. It
lacks some functionality available in the CLI; use `crv` for operations that
the desktop interface does not expose.

## Install and launch

From a source checkout, install the optional desktop dependencies in a virtual
environment:

```sh
python3 -m venv .venv
.venv/bin/pip install -e '.[desktop]'
.venv/bin/corvus-desktop
```

For a binary release, install the wheel with its extra in a virtual environment,
substituting the actual wheel filename:

```sh
python3 -m venv ~/.local/share/corvus-desktop
~/.local/share/corvus-desktop/bin/pip install './python/corvus-<version>-py3-none-any.whl[desktop]'
~/.local/share/corvus-desktop/bin/corvus-desktop
```

The `desktop` extra installs PySide6 and pyte. Installing the base package alone
provides the entry point but not those GUI dependencies. Developers can use
`make venv`, install `'.[desktop]'` with `.venv/bin/pip`, and then run
`make desktop-run` from the repository root.

## Connect to a daemon

By default the application uses `$XDG_RUNTIME_DIR/corvus/corvus.sock`, falling
back to `/tmp/corvus/corvus.sock` when `XDG_RUNTIME_DIR` is unset. This differs
from `crv`, which defaults to TCP. A custom Unix socket uses filesystem
permissions and no TLS:

```sh
corvus-desktop --daemon-socket /run/user/1000/corvus/corvus.sock
```

For another host, TCP defaults to port 9876 with mutual TLS:

```sh
corvus-desktop --daemon-host daemon.example.com --daemon-port 9876
corvus-desktop --daemon-host daemon.example.com --daemon-cert-dir ~/.config/corvus
```

The certificate directory must contain `ca.crt`, `corvus-client.crt`, and
`corvus-client.key`. Provision them using [corvus-admin](security.md).
`--daemon-host` and `--daemon-socket` are mutually exclusive. Use
`--no-daemon-tls` only against an intentionally configured development daemon
with TLS disabled. `--log-level debug` prints connection diagnostics to the
launching terminal; `--help` lists all flags.

## Common workflows

- Open **VMs**, select a VM, and use its lifecycle buttons to start, stop,
  pause, save, or reset it. The available operations depend on its current
  [lifecycle state](vm-management.md#state-machine).
- Use the VM detail tabs for drives, network interfaces, SSH keys, shared
  directories, audio, cloud-init, live resource statistics, serial console,
  and the HMP monitor. Guest execution requires a running guest agent.
- Use **Disks**, **Networks**, **SSH keys**, **Templates**, and **Nodes** for
  their resource lists and details. Disk details include snapshot controls.
- Load declarative YAML in **Apply**, or load an image pipeline in **Build**.
  Both use the same daemon operations as the CLI; see
  [apply](apply-configuration.md) and [image builds](image-builds.md).
- Inspect **Tasks** for progress and errors. A returned task ID identifies
  accepted work; inspect its final result before assuming success.

## Consoles and host paths

Headless VMs use the embedded serial console. The guest must configure a serial
console/getty to display a login prompt. The HMP monitor is a QEMU control
console, not a guest shell.

**View (SPICE)** launches `remote-viewer` (virt-viewer) installed on the desktop
machine. If it is unavailable, the application displays connection details for
another SPICE client. The viewer needs direct access to the VM node's advertised SPICE
host and port in addition to the application's RPC connection. See
[SPICE access](vm-management.md#spice-graphical-vms) and
[viewer-local folder sharing](shared-directories.md#spice-folder-sharing).

Paths for virtiofs shares and disk registration refer to the selected VM node.
Files loaded into the YAML editor come from the desktop machine. Read each
feature's path rules when using a remote daemon.

The desktop exposes many management operations but is not a complete CLI
replacement; use the command references for operations without a GUI workflow,
such as disk-version cleanup. See [Troubleshooting](troubleshooting.md) for
connection, startup, and display failures.
