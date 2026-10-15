# SSH Keys

SSH keys are managed as named entities in the database and can be attached to one or more VMs. When attached to a cloud-init-enabled VM, keys are injected into the guest's `authorized_keys` via the cloud-init ISO.

## Commands

```bash
crv ssh-key create <name> "<public_key>"   # Create a new SSH key
crv ssh-key list                           # List all SSH keys
crv ssh-key delete <key>                   # Delete an SSH key
crv ssh-key attach <vm> <key>              # Attach key to a VM
crv ssh-key detach <vm> <key>              # Detach key from a VM
crv ssh-key list-vm <vm>                   # List keys attached to a VM
crv ssh-key inject <vm> <username> [--reset] # Inject assigned keys through QGA
```

`<key>` and `<vm>` accept names or numeric IDs.

## Creating Keys

```bash
crv ssh-key create admin "ssh-ed25519 AAAA... admin@host"
crv ssh-key create deploy "ssh-rsa AAAA... deploy@ci"
```

The public key string is stored as-is. Corvus does not generate key pairs — provide the public half of an existing key.

## Attaching to VMs

```bash
crv ssh-key attach my-vm admin
crv ssh-key attach my-vm deploy
```

Keys can be assigned to any VM, including VMs without cloud-init.

When a key is attached:
1. The key is recorded in the database.
2. If cloud-init is enabled, its ISO is regenerated to include the new key.
3. Cloud-init provisions the keys when it runs in the guest. Assignment alone does not inject keys through QGA.

Multiple keys can be attached to the same VM. The same key can be attached to multiple VMs.

## Detaching from VMs

```bash
crv ssh-key detach my-vm admin
```

If cloud-init is enabled, its ISO is regenerated without the detached key. Detaching does not revoke keys already installed in the guest. The key itself remains in the database — only the VM association is removed.

## Injecting into a Running Guest

```bash
crv ssh-key inject my-vm root
crv ssh-key inject my-vm root --reset
```

Injection uses QGA's `guest-ssh-add-authorized-keys` command to install all keys
currently assigned to the VM. The username is required and must already exist.
The VM must be running with the guest agent enabled, and its QGA must support
this command (available since QEMU 5.2 on supported Unix guests). Cloud-init is
optional; its `injectSshKeys` setting does not affect this operation.

By default, existing authorized keys are preserved and assigned keys are appended.
QGA avoids adding identical key strings twice. `--reset` replaces all authorized
keys for the account, including keys added outside Corvus. A VM with no assigned
keys is rejected in both modes. Corvus does not create guest users or fall back to
shell commands. Attach, detach, start, and template instantiation do not implicitly
perform QGA injection. Each explicit injection records a VM task named
`inject-ssh-keys`; guest-agent errors are reported to the caller.

Python clients expose the same operation in both sync and async APIs:

```python
client.ssh_keys.inject("my-vm", "root")
client.ssh_keys.inject("my-vm", "root", reset=True)
await async_client.ssh_keys.inject("my-vm", "root")
```

VM references accept IDs or names; use `by_name=True` for a numeric VM name.

## Deleting Keys

```bash
crv ssh-key delete admin
```

A key cannot be deleted while it is attached to any VM. Detach it from all VMs first.

## Interaction with Cloud-Init

SSH keys are injected into the cloud-init user-data during ISO generation:

- **Default config** (no custom `cloudInitConfig`): keys are added to the default user's (`corvus`) `ssh_authorized_keys`.
- **Custom config with `injectSshKeys: true`** (default): keys are merged into the first user's `ssh_authorized_keys` in the custom YAML.
- **Custom config with `injectSshKeys: false`**: keys are not injected. Use this when your user-data is a raw script (e.g., PowerShell for Windows).
- **Raw script user-data** (starts with `#`): key injection is skipped automatically since YAML merging is not possible.

See [cloud-init.md](cloud-init.md) for full details on SSH key injection.

## In Apply and Template Configuration

SSH keys can be defined and attached declaratively:

```yaml
sshKeys:
  - name: admin
    publicKey: "ssh-ed25519 AAAA... admin@host"

vms:
  - name: web-server
    cpuCount: 2
    ram: 2G
    cloudInit: true   # Optional; false allows explicit QGA injection
    sshKeys:
      - admin
```

In templates, keys are referenced by name and copied to each instantiated VM:

```yaml
templates:
  - name: webserver
    cloudInit: true
    sshKeys:
      - name: admin
```
