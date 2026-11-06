# Corvus documentation

These catalogs cover all first-party Markdown guides and recipe READMEs,
including guides outside `doc/`. Agent policy and upstream vendored documentation
are excluded. Developer source-code links require a source checkout.

The [manual](README.md#documentation) also contains the full catalog as a table.

## Getting started

- [Project overview](../README.md) — Project purpose, components, and entry points
- [Installation and quick start](README.md) — Runtime prerequisites, installation, single-host setup, and first VM
- [Desktop interface](desktop-interface.md) — Install, connect, manage resources, and open native viewers
- [Web interface](web-interface.md) — Gateway setup, browser consoles, recovery, and frontend development
- [Troubleshooting](troubleshooting.md) — Connection, certificate, node, VM, network, and console diagnostics

## VMs and resources

- [VM management](vm-management.md) — Create, edit, start, stop, save, monitor, and view VMs
- [Disk management](disk-management.md) — Image versions and tags, placement, import/upload, attach, and cleanup
- [Snapshots](snapshots.md) — Disk and full-machine checkpoints, rollback, and deletion
- [Networking](networking.md) — Managed networks, interfaces, DNS, VDE, and port forwarding
- [Multi-node deployments](multi-node.md) — Node registration, placement, agent connections, and maintenance
- [VM migration](vm-migration.md) — Save-and-restore VM migration and agent-to-agent disk transfers
- [Security](security.md) — Mutual TLS, certificate provisioning, renewal, and trust boundaries
- [SSH keys](ssh-keys.md) — Assign keys for cloud-init and inject them explicitly through QGA
- [Cloud-init](cloud-init.md) — NoCloud configuration, SSH key injection, ISO generation, and Windows
- [Shared directories](shared-directories.md) — Node-local virtiofs shares and viewer-local SPICE folder sharing
- [VM audio](vm-audio.md) — Sound cards, audio backends, playback, and microphone support
- [Guest execution](guest-exec.md) — Execute commands through the QEMU guest agent
- [Guest file transfers](guest-files.md) — Upload and download files through the QEMU guest agent
- [Task history](task-history.md) — Inspect operations, wait for completion, and request cancellation
- [List output columns](list-output.md) — Select text columns and use column completion
- [Observability](observability.md) — Resource samples, Prometheus metrics, and Grafana queries

## Declarative configuration

- [Templates](templates.md) — Reusable VM blueprints and instantiation
- [Apply configuration](apply-configuration.md) — Declare environments, image policies, and resource dependencies
- [Image builds](image-builds.md) — Procedural image pipelines, cache, provisioners, and editor schema

## Development and internals

- [Architecture](architecture.md) — Component ownership, source layout, entities, and runtime patterns
- [Development guide](development.md) — Build dependencies, tests, implementation conventions, and doc maintenance
- [Database migrations](database-migrations.md) — Versioned SQLite/PostgreSQL upgrades, fixtures, and migration tests
- [VM lifecycle and fencing](vm-lifecycle.md) — State transitions, lifecycle revisions, and stale-start protection
- [RPC protocol](rpc-protocol.md) — Capabilities, shared records, errors, streams, and schema generation
- [Python client](../python/README.md) — Typed synchronous/asynchronous APIs, subscriptions, and examples
- [Integration test harness](../integration_tests/README.md) — Nested-VM prerequisites, test authoring, scheduling, and debugging
- [Developer node](dev-node.md) — Long-lived nested development environment and iteration workflow
- [Managed image recipes](test-images.md) — Build, check, clean, and rebuild image fixtures; import ownership
- [QEMU command example](../qemu-command-example.md) — Annotated nodeagent-generated command and device topology
- [SPICE client maintenance](../frontend/src/lib/spice/README.md) — Vendored browser SPICE source and local extension rules

## Recipe guides

- [AlmaLinux desktop](../yaml/almalinux-10-desktop/README.md) — Unattended AlmaLinux 10 GNOME installation with Kickstart
- [Monitoring appliance](../yaml/corvus-monitor/README.md) — Prometheus/Grafana image, configuration, and deployment
- [FreeBSD guest](../yaml/freebsd/README.md) — Headless FreeBSD image with serial console, QGA, and NoCloud
- [Gentoo guests](../yaml/gentoo/README.md) — Cloud imports, headless guests, and development images
- [Windows 11 base](../yaml/windows-11/README.md) — Generalized Windows 11 Pro base, template, and per-VM TPM
- [Windows 11 desktop](../yaml/windows-11-desktop/README.md) — Desktop applications on the managed Windows 11 base

## More examples

The [YAML tree](../yaml/) also includes scenarios without a separate guide:

- [Complete environment](../yaml/example-apply/) — UEFI, networks, and templates.
- [Multi-OS templates](../yaml/multi-os/) — shared prerequisites for image builds.
- [Windows Server 2025](../yaml/windows-server-2025/) — unattended server image.
- [Debian nginx](../yaml/debian-nginx/) and [Ubuntu nginx](../yaml/ubuntu-nginx/) — application images.
- [Standalone template](../yaml/template-example/template-example.yml) — a reusable VM definition.
