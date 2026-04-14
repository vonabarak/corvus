# Guest Execution

Execute commands inside running VMs via the QEMU Guest Agent (QGA). The guest agent runs inside the VM and communicates with the daemon over a virtio-serial channel.

## Requirements

- The VM must have `guestAgent: true`.
- The VM must be in `running` state (not `starting`, `paused`, etc.).
- `qemu-guest-agent` must be installed and running inside the guest.

## Commands

```bash
crv guest-exec <vm> <command>
```

`<vm>` accepts a name or numeric ID.

## Examples

```bash
# Basic commands
crv guest-exec my-vm whoami
crv guest-exec my-vm hostname
crv guest-exec my-vm cat /etc/os-release

# System information
crv guest-exec my-vm uname -a
crv guest-exec my-vm df -h
crv guest-exec my-vm free -m

# Service management
crv guest-exec my-vm systemctl status nginx
crv guest-exec my-vm systemctl restart nginx

# File operations
crv guest-exec my-vm ls -la /var/log/
crv guest-exec my-vm tail -20 /var/log/syslog

# Network diagnostics
crv guest-exec my-vm ip addr
crv guest-exec my-vm ss -tlnp

# Windows VMs (via cmd.exe)
crv guest-exec win-vm 'cmd.exe /c hostname'
crv guest-exec win-vm 'powershell -Command "Get-Service"'
```

## Output

The command returns exit code, stdout, and stderr:

```
Exit code: 0
whoami output here
```

In structured output mode (`-o json`), all three are included:

```bash
crv guest-exec my-vm hostname -o json
```

## Windows Quoting

Windows guest agent commands are executed via `cmd.exe /c`. For complex PowerShell commands, use Base64-encoded commands to avoid quoting issues:

```bash
# Encode a PowerShell script to Base64 UTF-16LE
ENCODED=$(echo -n 'Get-Service | Where-Object {$_.Status -eq "Running"}' | iconv -t UTF-16LE | base64 -w0)
crv guest-exec win-vm "powershell -EncodedCommand $ENCODED"
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
