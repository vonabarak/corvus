# Corvus Agent Guide

Corvus is a QEMU/KVM VM management system. The core daemon, CLI, node
agent, and network agent are Haskell; the Python tree provides the
Cap'n Proto client library, admin/deploy tooling, web gateway, desktop
GUI, and integration-test harness; the browser UI is a React/Vite SPA.

This file is the repository-level guidance for coding agents. Keep it focused on
durable repo facts, commands, conventions, and review expectations. Prefer
tool-neutral wording so Codex and other coding agents can use the same source of
truth.

## Architecture

### Entry Points

Haskell executables from `package.yaml`:

- **`corvus`** - daemon and Cap'n Proto RPC bootstrap. Owns the PostgreSQL model,
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

### Communication

Cap'n Proto RPC over Unix socket or TCP is the system protocol. The wire schema
is the source of truth and lives under [`schema/`](schema/) (regenerate Haskell
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
[`doc/rpc-protocol.md`](doc/rpc-protocol.md) for the capability tree and
[`doc/security.md`](doc/security.md) for certificate roles and CN checks.

`src/Corvus/Protocol.hs` and `src/Corvus/Protocol/*.hs` retain the internal DTOs
(VmInfo, DiskImageInfo, NamedRef, etc.) that handlers return and that the Wire
layer converts into `Capnp.Gen.*` structs.

### Module Layout

High-level Haskell areas under `src/Corvus/`:

```
Protocol.hs, Protocol/   # Internal handler-return DTOs
Types.hs, Server.hs      # Daemon state, config, startup/shutdown helpers
Model.hs, Model/         # Persistent schema, enums, migrations, state machine
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
Build/                   # Image-build cache hashing and store helpers
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

### Operational Patterns

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
- **Database**: Persistent + Esqueleto ORM with PostgreSQL; `runSqlPool` pattern;
  daemon runs migrations on startup.
- **Logging**: `MonadLogger` (`LoggingT`) with component CLI log-level flags.
- **Task tracking**: mutating daemon operations are recorded in the `task` table
  through `Action`/task wrappers; read-only list/show/get/ping style operations
  are skipped. Startup and shutdown are also task-recorded.
- **VM state machine**: enforced in `Model.VmState.validateTransition` (pure,
  directly unit-testable). VMs with guest agent support can pass through
  `starting` until healthcheck succeeds; reset returns to `stopped`.

### Database Entities

The Persistent entities are defined in `Model.hs`. Major user-visible groups are
VMs, disks and placements, drives, networks, network interfaces, nodes, shared
directories, snapshots, SSH keys, cloud-init configs, templates, build/task
records, and their join tables.

### Key Enums

Enums are text-serializable through `EnumText`. Important examples include
`VmStatus`, `DriveInterface`, `DriveFormat`, `DriveMedia`, `CacheType`,
`NetInterfaceType`, `SharedDirCache`, `TemplateCloneStrategy`, `TaskSubsystem`,
`TaskResult`, node admin/health states, build event/status types, and network
IPAM enums. Check `Model.hs`, `Corvus.Protocol.*`, and `Corvus.Wire.Enums`
before adding or renaming enum values.

### Test Structure

```
test/
|-- Spec.hs              # hspec-discover entry point
|-- Test/
|   |-- Prelude.hs       # Re-exports, test utilities
|   |-- Database.hs      # Test database setup/teardown
|   |-- Settings.hs      # Test configuration
|   `-- DSL/
|       |-- Core.hs      # TestM monad, testCase, runDb
|       |-- Given.hs     # Setup primitives
|       |-- When.hs      # Action/RPC primitives
|       `-- Then.hs      # Assertion primitives
`-- Corvus/
    `-- *Spec.hs         # Hspec unit tests
```

Integration tests live in `integration_tests/` (pytest). Python package tests
live in `python/tests/`. Haskell `test/` is unit-test focused and uses the custom
BDD DSL (`Test.DSL.*`) with `testCase`, `given`, `when_`, `then_`.

## Build System

Stack + Hpack (`package.yaml` -> `corvus.cabal`), LTS-23.28 resolver.

### Make Targets

| Target | Description |
|---|---|
| `make build` | Stack build |
| `make capnp` | Regenerate `src-generated/Capnp/Gen/*.hs` from `schema/*.capnp` |
| `make install` | Install Haskell binaries, shell completions, Python tooling, and web assets when available |
| `make format` | Ruff + Fourmolu formatting; also frontend formatting when `frontend/node_modules/` exists |
| `make lint` | HLint, Fourmolu check, Ruff check/format check, mypy; also frontend checks when `frontend/node_modules/` exists |
| `make unit-tests` | Haskell unit tests; accepts `MATCH=<hspec pattern>` |
| `make python-test` | Python client/admin/web/desktop tests against a temp daemon |
| `make integration-tests` | Pytest nested-VM suite; accepts `MATCH=<pytest -k expr>` and `WORKERS=N` |
| `make integration-tests-clean` | Remove leftover `corvus-it-*` VMs/networks from aborted runs |
| `make test` | Umbrella target: `unit-tests`, `python-test`, then `integration-tests` |
| `make web-build` | Build frontend SPA and copy it into `python/corvus_web/static/` |
| `make web-dev` | Run Vite dev server for frontend work |
| `make web-lint` / `make web-format` | Frontend-specific checks/formatting |
| `make desktop-run` | Run the desktop GUI from the local tree |
| `make set-version VERSION=X.Y.Z.W` | Bump `package.yaml`, `corvus.cabal`, and `pyproject.toml` together |
| `make release` | Stage release tree and tarball with binaries, completions, Python artifacts, docs, YAML, and schema |

Use `make set-version VERSION=X.Y.Z.W` whenever the user says "bump version",
"set version", or "release X.Y.Z.W"; do not hand-edit version files.

### Build Dependencies

No external C libraries are required by the Haskell package. Runtime and test
workflows need PostgreSQL, QEMU/KVM, `qemu-img`, and relevant optional tools
(`virtiofsd`, `dnsmasq`, `remote-viewer`, frontend `npm`, desktop extras, etc.)
depending on the area touched.

### Integration Test Notes

- Integration tests require nested QEMU/KVM access.
- Networking tests drive a `corvus-netd` instance with `CAP_NET_ADMIN`; the netd
  smoke path launches it through `systemd-run` on the test node and tunnels
  Cap'n Proto over SSH so pytest itself stays unprivileged.
- Log level during tests is controlled by `CORVUS_TEST_LOG_LEVEL` (default:
  `info`). Use `CORVUS_TEST_LOG_LEVEL=debug` for verbose output.

### Integration Test Scheduling

The custom xdist scheduler in [`integration_tests/conftest.py`](integration_tests/conftest.py)
(`_LoadScopeShutdownSingleton`) enforces two invariants every new test must
respect:

1. **Class atomicity.** Every test method in a class runs on the same worker. The
   class-scoped `topology` fixture in `IntegrationTestCase` boots one outer
   test-node VM per class and relies on this. Design tests so all state shared
   across methods of a class is held by the class fixture; never assume
   coordination between methods on different workers.

2. **On-demand scope dispatch.** A test class is assigned to a worker only when
   that worker is down to one pending test (the running one). A slow class does
   not accumulate a backlog of queued classes behind it on the same worker; idle
   workers pick up queued classes from the global queue first. Design tests so
   they do not depend on a specific cross-class execution order.

When writing a multi-minute test class, mark it `@pytest.mark.slow` so it floats
to the head of the dispatch queue (see `pytest_collection_modifyitems`) and
starts at t=0 on its own worker.

## Documentation

User-facing documentation lives in `doc/`. See [`doc/INDEX.md`](doc/INDEX.md)
for the full list.

| File | Content |
|---|---|
| `doc/README.md` | Project overview, installation, quick start |
| `doc/vm-management.md` | VM lifecycle, state machine, serial/graphical console |
| `doc/disk-management.md` | Disk CRUD, placements, overlays, clones, rebase, attach/detach |
| `doc/snapshots.md` | qcow2 snapshot operations |
| `doc/networking.md` | Virtual networks, network interfaces, `corvus-netd` |
| `doc/multi-node.md` | Nodes, scheduler placement, per-host agents, same-node invariants |
| `doc/vm-migration.md` | Stopped-VM migration and disk copy/move workflows |
| `doc/security.md` | mTLS roles, cert layout, `corvus-admin` deployment |
| `doc/observability.md` | Resource metrics, CLI usage view, Web UI sparklines, Prometheus |
| `doc/image-builds.md` | Procedural disk image build pipelines |
| `doc/web-interface.md` | `corvus-web`, REST/WebSocket bridge, frontend development |
| `doc/ssh-keys.md` | SSH key management |
| `doc/cloud-init.md` | Cloud-init ISO generation, SSH injection, Windows support |
| `doc/shared-directories.md` | virtiofs shared directories |
| `doc/guest-exec.md` | Guest agent command execution |
| `doc/task-history.md` | Async operation tracking |
| `doc/templates.md` | VM template YAML schema and instantiation |
| `doc/apply-configuration.md` | Declarative environment YAML reference |
| `doc/rpc-protocol.md` | Cap'n Proto protocol tour and pycapnp example |
| `doc/dev-node.md` | Manual-testing VM and helper scripts |
| `python/README.md` | Python client usage and coverage |
| `integration_tests/README.md` | Nested-VM integration harness |

## Project Rules

### Handler Implementation

All mutating operations must be implemented as Action types (instances of the
`Action` type class from `Corvus.Action`), not as plain `handle*` functions.
Each Action is a data type carrying its parameters, with `actionExecute`
containing the logic. The `handle*` functions are internal implementation
details called only by their own Action instance; they should not be exported or
called directly from other modules.

When one handler needs to invoke another handler's logic, use the other
handler's Action type via `actionExecute`, `runAction`, or
`runActionAsSubtask`; never call the `handle*` function directly.

Read-only operations (list, show, get) are the exception. They are dispatched
directly without Action wrapping since they do not need task recording.

### After Code Changes

Run `make format` and `make lint` after modifying Haskell or Python source
files. `make format` edits files in place. `make lint` is read-only and covers
static analysis plus formatter check passes, so run `make format` first and fix
all lint warnings before committing.

When `frontend/node_modules/` exists, both targets also cover frontend
formatting/linting. If it does not exist and the change touched frontend code,
run the frontend-specific setup/checks needed for that work.

```
make format
make lint
```

### Before Committing

Run `make test` before every commit unless the user explicitly asks for a
narrower verification. It chains `unit-tests`, `python-test`, and
`integration-tests` in escalating-cost order so cheap failures surface before
slower phases. Make stops on the first non-zero exit, so a green `make test` is
the local equivalent of the full local pre-merge gate.

```
make test
```

### Backwards Compatibility

Corvus is in early beta. Breaking changes to YAML schemas, RPC protocol,
database schema, CLI, Python/TypeScript client shapes, and other public surfaces
are accepted without compatibility shims, deprecation paths, or transitional
warnings. When renaming a field or removing a feature, change every call site
outright; do not keep the old name as an alias and do not add migration code
that detects and rewrites legacy inputs. Make the break clean and update docs
and examples in the same commit.

### Cross-Entity References

Every JSON / REST response field that refers to a different entity must use the
nested `{id, name}` shape, never flat `<role>_id` plus `<role>_name` fields. Use
the shared `NamedRef` type:

- **Cap'n Proto**: `Common.NamedRef` in [`schema/common.capnp`](schema/common.capnp).
- **Haskell**: `Corvus.Protocol.NamedRef` (record with `nrId`, `nrName`).
- **Python**: `corvus_client.types.NamedRef`.
- **TypeScript**: `NamedRef` in [`frontend/src/api/refs.ts`](frontend/src/api/refs.ts).

Optional references become `Maybe NamedRef` / `NamedRef | None` /
`NamedRef | null`. On the wire the absence is encoded as `id == 0`; the
language-level converters translate to the appropriate nullable type at the
boundary so consumers never see the sentinel.

The field name on the container is the role, not the entity's name: a drive's
reference to its image is `disk_image: NamedRef`, not `image` or
`disk_image_ref`. When in doubt, mirror the foreign-key column name in the
database (`drive.disk_image_id` -> field `disk_image`).

Two narrow carve-outs:

- `TaskInfo.parent_id` stays flat (`Maybe Int64`) because tasks do not carry a
  human-readable name field.
- Input-side RPC lookups use `Common.EntityRef` (a separate id-or-name union; see
  `Corvus.Wire.Common.EntityRef`). `NamedRef` is output-only.

Database-only references that never surface in CLI / REST / client output, such
as internal task-tracking columns, are exempt.
