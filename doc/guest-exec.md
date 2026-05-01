# Guest Execution

Execute commands inside running VMs via the QEMU Guest Agent (QGA). The guest agent runs inside the VM and communicates with the daemon over a virtio-serial channel.

## Requirements

- The VM must have `guestAgent: true`.
- The VM must be in `running` state (not `starting`, `paused`, etc.).
- `qemu-guest-agent` must be installed and running inside the guest.

## Commands

```bash
crv vm exec <vm> <command>
```

`<vm>` accepts a name or numeric ID. `<command>` is a **single argument** — quote it whenever the command contains spaces, flags, or redirections, otherwise the shell will split it into multiple tokens that the parser rejects (or, for a leading `-`, treats as a flag to `crv` itself). The daemon executes the string via `/bin/sh -c` inside the guest.

## Examples

```bash
# Single-token commands (no quoting needed)
crv vm exec my-vm whoami
crv vm exec my-vm hostname

# Multi-token commands MUST be quoted as one argument
crv vm exec my-vm 'cat /etc/os-release'
crv vm exec my-vm 'uname -a'
crv vm exec my-vm 'df -h'
crv vm exec my-vm 'free -m'

# Service management
crv vm exec my-vm 'systemctl status nginx'
crv vm exec my-vm 'systemctl restart nginx'

# File operations
crv vm exec my-vm 'ls -la /var/log/'
crv vm exec my-vm 'tail -20 /var/log/syslog'

# Network diagnostics
crv vm exec my-vm 'ip addr'
crv vm exec my-vm 'ss -tlnp'

# Pipes and redirections (interpreted by /bin/sh inside the guest)
crv vm exec my-vm 'ps -ef | grep nginx'
crv vm exec my-vm 'echo hello > /tmp/marker'

# Windows VMs (via cmd.exe)
crv vm exec win-vm 'cmd.exe /c hostname'
crv vm exec win-vm 'powershell -Command "Get-Service"'
```

## Output

In text mode, the guest's stdout is written to `crv`'s stdout and the guest's stderr to `crv`'s stderr. `crv` exits with the guest command's exit code so it can be chained with `&&` / `||` like a local command:

```bash
crv vm exec my-vm whoami && echo "ok"
```

In structured output mode (`-o json`), exit code, stdout, and stderr are all returned in one object:

```bash
crv -o json vm exec my-vm hostname
# {"exitcode":0,"stdout":"my-vm\n","stderr":""}
```

## Windows Quoting

Windows guest agent commands are executed via `cmd.exe /c`. For complex PowerShell commands, use Base64-encoded commands to avoid quoting issues:

```bash
# Encode a PowerShell script to Base64 UTF-16LE
ENCODED=$(echo -n 'Get-Service | Where-Object {$_.Status -eq "Running"}' | iconv -t UTF-16LE | base64 -w0)
crv vm exec win-vm "powershell -EncodedCommand $ENCODED"
```

## Health Checks

When `guestAgent: true`, the daemon periodically pings the guest agent to verify it's responsive. This serves two purposes:

1. **State transition**: VMs with guest agent go through a `starting` state and transition to `running` only after the first successful ping.
2. **Network discovery**: the daemon queries `guest-network-get-interfaces` to discover guest IP addresses, visible in `crv vm show`.

The health check interval is configurable via daemon settings (default: 5 seconds).

## Troubleshooting

**"Guest agent not enabled"**: the VM was created without `--guest-agent`. Use `crv vm edit <vm> --guest-agent true` (VM must be stopped).

**"Guest agent communication error"**: the agent is not running inside the guest. Install and start it:
- Linux: `apt install qemu-guest-agent && systemctl start qemu-guest-agent`
- Alpine: `apk add qemu-guest-agent && rc-service qemu-guest-agent start`
- Windows: install the QEMU guest agent MSI from the virtio drivers package.

**Command hangs**: the guest agent executes commands synchronously. Long-running commands block the agent channel. For background tasks, use `nohup` or `&` inside the command.
