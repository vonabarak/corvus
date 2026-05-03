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
- [Image Builds](image-builds.md) -- Bake reusable disk images via a procedural pipeline

## Examples

YAML configurations live under [yaml/](../yaml/), with one subdirectory per
self-contained scenario (each containing the apply YAML, build YAML, and
any companion files like kernel configs or autounattend answer files):

- [yaml/example-apply/](../yaml/example-apply/) -- Full environment with UEFI, networking, templates
- [yaml/multi-os/](../yaml/multi-os/) -- Multi-OS template library (shared prereq for several builds)
- [yaml/test-images/](../yaml/test-images/) -- Local test image VMs (Alpine + Windows)
- [yaml/alpine-test/](../yaml/alpine-test/) -- Build the minimal Alpine integration-test image
- [yaml/windows-server-2025/](../yaml/windows-server-2025/) -- Build a Windows Server 2025 image
- [yaml/gentoo-headless/](../yaml/gentoo-headless/) -- Build a minimal headless Gentoo image
- [yaml/gentoo-corvus/](../yaml/gentoo-corvus/) -- Build a Gentoo image with the Corvus toolchain
- [yaml/debian-nginx/](../yaml/debian-nginx/) -- Bake nginx onto a Debian 12 base
- [yaml/ubuntu-nginx/](../yaml/ubuntu-nginx/) -- Bake nginx onto an Ubuntu 24.04 base
- [doc/template-example.yml](template-example.yml) -- Standalone template file
