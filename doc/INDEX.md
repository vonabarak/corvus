# Corvus Documentation

## Getting Started

- [README](README.md) -- Project overview, installation, quick start

## Command References

- [VM Management](vm-management.md) -- Create, start, stop, edit, monitor VMs; serial console
- [Disk Management](disk-management.md) -- Create, register, import, overlay, clone, rebase, attach/detach
- [Snapshots](snapshots.md) -- Create, rollback, merge, delete qcow2 snapshots
- [Networking](networking.md) -- Virtual networks, network interfaces, port forwarding
- [SSH Keys](ssh-keys.md) -- Create, delete, attach/detach to VMs
- [Cloud-Init](cloud-init.md) -- ISO generation, custom user-data, SSH key injection, Windows support
- [Shared Directories](shared-directories.md) -- virtiofs host directory sharing
- [Guest Execution](guest-exec.md) -- Execute commands inside VMs via guest agent
- [Task History](task-history.md) -- Track async operations, query history

## Declarative Configuration

- [Templates](templates.md) -- Define reusable VM blueprints in YAML
- [Apply Configuration](apply-configuration.md) -- Declare entire environments in a single YAML file

## Examples

- [doc/apply-examples/example-apply.yml](apply-examples/example-apply.yml) -- Full environment with UEFI, networking, templates
- [doc/apply-examples/multi-os.yml](apply-examples/multi-os.yml) -- Multi-OS template library
- [doc/apply-examples/test-images.yml](apply-examples/test-images.yml) -- Local test images (Alpine + Windows)
- [doc/template-example.yml](template-example.yml) -- Standalone template file
