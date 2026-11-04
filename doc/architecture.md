# Architecture

Corvus is a QEMU/KVM VM management system. This guide maps component ownership
and source locations for contributors. See the [development guide](development.md)
for build, test, and implementation conventions.

## Entry Points

Haskell executables from `package.yaml`:

- **`corvus`** - daemon and Cap'n Proto RPC bootstrap. Owns the persistent model,
  scheduling, orchestration, task tracking, node supervision, and client-facing
  Unix/TCP listeners.
- **`crv`** - CLI client. Uses `Corvus.Client.Capnp.*` to speak Cap'n Proto RPC
  to the daemon.
- **`corvus-nodeagent`** - per-host agent for local-machine state: QEMU and
  virtiofsd supervision, disk/image operations, console buffers, QGA polling,
  node stats, and VM status pushes.
- **`corvus-netd`** - per-host privileged network agent for bridges, TAPs,
  nftables, dnsmasq, host DNS, and cleanup/reconciliation of kernel state.

Python scripts from `pyproject.toml`:

- **`corvus-admin`** - CA/certificate management, systemd unit deployment,
  quickstart, renewals, and daemon node registration.
- **`corvus-web`** - FastAPI HTTP/WebSocket gateway. Uses `corvus_client` to talk
  to the daemon and serves the React SPA bundle when built.
- **`corvus-desktop`** - PySide desktop GUI using the Python client.

The React/Vite frontend lives under `frontend/` and is packaged into
`python/corvus_web/static/` by `make web-build`.

## Communication

Cap'n Proto RPC over Unix socket or TCP is the system protocol. The wire schema
is the source of truth and lives under [`schema/`](../schema/) (regenerate Haskell
bindings with `make capnp`). The daemon's bootstrap cap is `Daemon`
(`schema/corvus.capnp`); subsystem managers and resource caps hang off that
root. Streaming flows (serial console, HMP monitor, build events, guest-agent
status, task progress, VM status, node stats) use sink caps in
`schema/streams.capnp`.

TCP links use mTLS by default:

- CLI / Python clients / web gateway -> daemon on port 9876
- daemon -> `corvus-nodeagent` on port 9878
- daemon -> `corvus-netd` on port 9877
- nodeagent -> nodeagent for disk migration streams

Unix-socket client connections skip TLS and rely on filesystem permissions. See
[`doc/rpc-protocol.md`](rpc-protocol.md) for the capability tree and
[`doc/security.md`](security.md) for certificate roles and CN checks.

`src/Corvus/Protocol.hs` and `src/Corvus/Protocol/*.hs` retain the internal DTOs
(VmInfo, DiskImageInfo, NamedRef, etc.) that handlers return and that the Wire
layer converts into `Capnp.Gen.*` structs.

## Module Layout

High-level Haskell areas under `src/Corvus/`:

```
Protocol.hs, Protocol/   # Internal handler-return DTOs
Types.hs, Server.hs      # Daemon state, config, startup/shutdown helpers
Model.hs, Model/         # Persistent schema, enums, state machine
Database/                # Backend setup and versioned Migrations/ modules
Action.hs, Handlers.hs   # Action wrapper and read-only helpers
Handlers/                # VM, disk, network, node, build, apply, template, task,
                         # cloud-init, SSH key, shared-dir, guest-exec logic
Rpc/                     # Cap'n Proto cap implementations and server bootstrap
Wire/                    # Haskell <-> Cap'n Proto converters
Client/                  # CLI types, parser, commands, output, editor helpers
Client/Capnp/            # Client-side Cap'n Proto connection and RPC wrappers
Node/                    # corvus-nodeagent runtime, QMP/QGA, image/disk, sockets,
                         # SPICE/VSOCK allocation, cleanup, node stats
NodeAgentClient/         # Daemon-side client shim for corvus-nodeagent
Netd/                    # corvus-netd runtime, kernel networking, dnsmasq,
                         # nftables, cleanup, net-agent caps
NetAgentClient/          # Daemon-side client shim for corvus-netd
Build/                   # Image-build artifact identity hashing
Schema/                  # YAML schema parsers for apply/template/build/cloud-init
Qemu/                    # Shared QEMU config facade retained for compatibility
Utils/                   # YAML, network/subnet helpers
```

Other important trees:

```
app/daemon/              # corvus executable
app/client/              # crv executable
app/nodeagent/           # corvus-nodeagent executable
app/netd/                # corvus-netd executable
tools/                   # Separate Haskell development checks and their tests
schema/                  # Cap'n Proto schemas; run make capnp after edits
src-generated/           # Generated Haskell Cap'n Proto modules
python/corvus_client/    # Sync and async pycapnp client library
python/corvus_admin/     # PKI, deploy, quickstart, systemd templates
python/corvus_web/       # FastAPI REST/WebSocket bridge and packaged SPA assets
python/corvus_desktop/   # PySide desktop app
python/tests/            # Python client/admin/web/desktop tests
frontend/                # React/Vite web UI source
integration_tests/       # Pytest nested-VM harness
```

## Operational Patterns

- **Daemon as orchestrator**: owns the database, task table, scheduler,
  node-routing decisions, Cap'n Proto bootstrap, client listeners, and
  reconnect supervisors for each registered node.
- **Nodeagent as per-host runtime**: owns QEMU/virtiofsd process state, local disk
  and cloud-init files, serial/HMP/QGA streams, SPICE/VSOCK allocation, node
  stats, and cleanup of Corvus-owned processes on restart/shutdown.
- **Netd as privileged network reconciler**: owns bridges, TAP allocation,
  nftables, dnsmasq, host DNS, IP monitoring, and cleanup of Corvus-owned kernel
  networking state on restart/shutdown.
- **Web and desktop clients**: use the Python `corvus_client` path to speak to
  the daemon; `corvus-web` exposes REST/WebSocket surfaces for the SPA.
- **Admin workflow**: `corvus-admin` owns CA state on the admin workstation,
  deploys certs/systemd units to local or SSH targets, and registers nodes.
- **Concurrency**: STM (`TVar`) for shared daemon state; `async` and supervisors
  for background listeners, reconnect loops, process monitors, and streams.
- **Database**: Persistent + Esqueleto ORM with SQLite or PostgreSQL;
  `runSqlPool` pattern. Fresh databases use the current model; existing
  databases use versioned modules under `src/Corvus/Database/Migrations/`.
  See [`doc/database-migrations.md`](database-migrations.md) for authoring,
  retiring, and testing migrations.
- **Logging**: `MonadLogger` (`LoggingT`) with component CLI log-level flags.
- **Task tracking**: mutating daemon operations are recorded in the `task` table
  through `Action`/task wrappers; read-only list/show/get/ping style operations
  are skipped. Startup and shutdown are also task-recorded.
- **VM state machine**: enforced in `Model.VmState.validateTransition` (pure,
  directly unit-testable). VMs with guest agent support can pass through
  `starting` until healthcheck succeeds; reset returns to `stopped`.

## Database Entities

The Persistent entities are defined in `Model.hs`. Major user-visible groups are
VMs, disks and placements, drives, networks, network interfaces, nodes, shared
directories, snapshots, SSH keys, cloud-init configs, templates, build/task
records, and their join tables.

## Key Enums

Enums are text-serializable through `EnumText`. Important examples include
`VmStatus`, `DriveInterface`, `DriveFormat`, `DriveMedia`, `CacheType`,
`NetInterfaceType`, `SharedDirCache`, `TemplateCloneStrategy`, `TaskSubsystem`,
`TaskResult`, node admin/health states, build event/status types, and network
IPAM enums. Check `Model.hs`, `Corvus.Protocol.*`, and `Corvus.Wire.Enums`
before adding or renaming enum values.

## Node-agent request isolation

The node-agent session dispatcher only handles bounded in-memory operations
inline: ping, status lookup, subscription registration, console-buffer flush,
and transfer-token lookup. Filesystem, subprocess, QMP/QGA, network and streaming
operations use `handleParsedAsync`. A stalled request must not delay another
VM's lifecycle requests or the session heartbeat. Async dispatch does not cancel
a mutation when its caller disconnects; runtime ownership and lifecycle fences
still govern its completion.

VM operation locks belong to the process-wide ledger, so reconnecting sessions
share the same locks. Hard stop fences starts and signals QEMU before waiting
for that lock. Reset retains the ledger entry until QEMU exit and helper cleanup
are confirmed. Failed termination retains runtime ownership and network resources
and records an error; stale completions cannot overwrite a newer generation.
Interrupted stops discovered on reconnect require explicit recovery rather than
an automatic start.

Live disk detach accepts asynchronous guest unplug. Delayed backend cleanup is
connection-supervised, limited to 60 seconds, and checks runtime identity while
holding the VM operation lock before sending further QMP commands.

Console attachment returns its input capability before awaiting output callbacks.
A connection-supervised worker sends replay followed by live output, with a
30-second callback deadline. Subscribers wait on their own buffer positions;
output and shutdown wake every subscriber. Input writes are bounded too.
Disk readers stream asynchronously and accept one consumer; cancel remains
available while a callback is stalled. Transfers bound each callback/write to
60 seconds, supervise handles on connection closure, and remove abandoned upload
files. Download progress observers have a five-second deadline and are dropped
on failure without failing the download.

The daemon pings each node session every five seconds with a ten-second deadline,
including during reconciliation and autostart, alongside status freshness checks.
QMP handshakes have a ten-second deadline and ordinary commands a 30-second
budget; snapshot operations retain longer command budgets. Hard stop has a
45-second node budget and a 60-second daemon budget. Process waits share a single
waiter per handle and bound the caller's wait without joining a stuck waiter.
Node request logs record operation, request thread, outcome and elapsed time;
request parameters are omitted because they can contain secrets.
