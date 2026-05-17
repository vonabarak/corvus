# Corvus

QEMU/KVM virtual machine management daemon with a CLI client, written in Haskell.

## Architecture

### Executables

- **`corvus`** — daemon that manages VM lifecycle, listens on TCP or Unix socket
- **`crv`** — CLI client that communicates with the daemon via Cap'n Proto RPC

### Communication

Cap'n Proto RPC over TCP or Unix socket. The wire schema is the source of truth and
lives under [`schema/`](schema/) (regenerate Haskell bindings with `make capnp`).
The daemon's bootstrap cap is `Daemon` (`schema/corvus.capnp`); every subsystem
hangs off it as a manager → resource cap hierarchy. Streaming flows (serial
console, HMP monitor, build events, guest-agent status, task progress) use the
sink caps in `schema/streams.capnp`. See [`doc/rpc-protocol.md`](doc/rpc-protocol.md)
for a full tour and a pycapnp client example.

`src/Corvus/Protocol.hs` and `src/Corvus/Protocol/*.hs` retain the **internal** DTOs
(VmInfo, DiskImageInfo, …) that handlers return and that the Wire layer converts
into the Cap'n Proto structs in `Capnp.Gen.*`.

### Module Layout

```
src/Corvus/
├── Protocol.hs          # Internal handler-return DTOs (VmInfo, DiskImageInfo, …)
├── Types.hs             # Server state, config, listen addresses, subscriber TVars
├── Server.hs            # Startup + graceful-shutdown helpers (the Cap'n Proto listener lives in Rpc/Server.hs)
├── Model.hs             # Persistent ORM schema, enums, migrations
├── Handlers.hs          # Read-only task helpers (the legacy big-Request dispatcher is gone post-Cap'n-Proto)
├── Rpc/                 # Cap'n Proto cap implementations (Daemon, Vm, Disk, Network, SshKey, Template, Task, CloudInit, Streams)
├── Wire/                # Haskell ↔ Cap'n Proto struct converters (Common, Vm, Disk, Build, Task, Enums, …)
├── Client/Capnp/        # Client-side Cap'n Proto session (Connection, Rpc wrappers, sink server impls)
├── CloudInit.hs         # Cloud-init NoCloud ISO generation + SSH key injection
├── Handlers/
│   ├── Core.hs          # Ping, status, shutdown
│   ├── Vm.hs            # VM lifecycle + state machine validation
│   ├── Disk.hs          # Disk image CRUD, snapshots, attach/detach, HTTP import, rebase/flatten
│   ├── Template.hs      # YAML template parsing and instantiation
│   ├── Apply.hs         # Declarative environment (crv apply) YAML handler
│   ├── SshKey.hs        # SSH key management (cloud-init gated)
│   ├── CloudInit.hs     # Custom cloud-init config CRUD (set/get/delete)
│   ├── NetIf.hs         # Network interface configuration, MAC generation
│   ├── Network.hs       # Virtual network management (bridge/TAP in namespace)
│   ├── SharedDir.hs     # Virtiofs shared directories
│   ├── GuestExec.hs     # QEMU guest agent command execution
│   └── GuestAgentPoller.hs  # Periodic guest agent health/network polling
├── Qemu/
│   ├── Config.hs        # QEMU binary paths, base directory
│   ├── Runtime.hs       # XDG runtime dir for sockets (HMP, QMP, SPICE, QGA)
│   ├── Process.hs       # QEMU process spawning and PID management
│   ├── Qmp.hs           # QMP protocol client (shutdown, pause)
│   ├── QmpQQ.hs         # Quasi-quoter for QMP JSON commands
│   ├── Command.hs       # QEMU command-line builder
│   ├── Image.hs         # qemu-img wrapper (create, resize, snapshot, clone, download)
│   ├── GuestAgent.hs    # QGA protocol client (exec, ping, network-get-interfaces)
│   ├── Netns.hs         # Haskell FFI to C namespace creator (startNamespace)
│   ├── Netns/
│   │   └── Manager.hs   # Bridge/dnsmasq management via nsenter
│   └── Virtiofsd.hs     # virtiofsd process management
├── Client/
│   ├── Connection.hs    # Socket management, binary protocol
│   ├── Rpc.hs           # High-level RPC call wrappers
│   ├── Parser.hs        # optparse-applicative CLI parsing
│   ├── Commands.hs      # Command execution dispatcher
│   ├── Editor.hs        # $EDITOR launcher for template/cloud-init editing
│   ├── Commands/
│   │   ├── Vm.hs        # VM command handlers and display
│   │   ├── Disk.hs      # Disk command handlers (incl. HTTP import)
│   │   ├── Template.hs  # Template command handlers
│   │   ├── Template/
│   │   │   └── Yaml.hs  # TemplateDetails → YAML serializer, skeleton template
│   │   ├── Apply.hs     # Apply command handler
│   │   ├── Network.hs   # Network command handlers
│   │   ├── NetIf.hs     # Network interface command handlers
│   │   ├── SshKey.hs    # SSH key command handlers
│   │   ├── SharedDir.hs # Shared directory command handlers
│   │   ├── GuestExec.hs # Guest exec command handlers
│   │   ├── CloudInit.hs # Cloud-init config command handlers
│   │   └── Task.hs      # Task history command handlers
│   ├── Output.hs        # Unified table/detail output formatting (printTableHeader, printField)
│   ├── Types.hs         # CLI command types
│   └── Config.hs        # Client configuration
└── Utils/
    ├── Yaml.hs          # YAML parsing with quasi-quoters (yamlQQ, yaml)
    └── Subnet.hs        # Subnet utilities for virtual networks

cbits/
├── netns.h              # C namespace manager header
└── netns.c              # C namespace manager (fork, unshare, loopback, signal wait)
```

### Key Patterns

- **Concurrency**: STM (`TVar`) for shared server state; `async` for background VM process monitoring
- **Database**: Persistent + Esqueleto ORM with PostgreSQL; `runSqlPool` pattern; auto-migration on startup
- **Logging**: `MonadLogger` (LoggingT transformer)
- **VM state machine**: Enforced in `Model.VmState.validateTransition` (pure, directly unit-testable) — stopped → starting (if guest agent) or running → stopping → stopped. VMs with guest agent go through `starting` state until first healthcheck ping succeeds. Reset always returns to stopped.
- **Task tracking**: Every mutating request is recorded in the `task` table via `withTask` wrapper in `Handlers.hs`. Read-only requests (list, show, ping) are skipped. Startup and shutdown are also recorded as tasks.
- **Network namespaces**: A single shared user+network+UTS namespace is created at daemon startup (no root required). The C helper (`cbits/netns.c`) forks from the Haskell process, calls `unshare(2)` in the single-threaded child, brings up loopback, and waits. `unshare(CLONE_NEWUSER)` requires a single-threaded process — the fork **must** happen in C, not via GHC's `forkProcess`, because GHC's threaded RTS keeps multiple OS threads alive after fork. All bridges, dnsmasq instances, and QEMU processes are managed inside this namespace via `nsenter` from Haskell.

### Database Entities

`Vm`, `DiskImage`, `Drive`, `Network`, `NetworkInterface`, `SharedDir`, `Snapshot`, `SshKey`, `VmSshKey`, `TemplateVm`, `TemplateDrive`, `TemplateNetworkInterface`, `TemplateSshKey`, `CloudInit`, `TemplateCloudInit`, `Task` — all defined in `Model.hs`.

### Key Enums (text-serializable via `EnumText` typeclass)

`VmStatus` (stopped/starting/running/stopping/paused/error), `DriveInterface`, `DriveFormat`, `DriveMedia`, `CacheType`, `NetInterfaceType` (user/tap/bridge/macvtap/managed), `SharedDirCache`, `TemplateCloneStrategy` (clone/overlay/direct/create), `TaskSubsystem` (vm/disk/network/ssh-key/template/shared-dir/snapshot/system/apply), `TaskResult` (running/success/error).

### Test Structure

```
test/
├── Spec.hs              # hspec-discover entry point
├── Test/
│   ├── Prelude.hs       # Re-exports, test utilities
│   ├── Database.hs      # Test database setup/teardown
│   ├── Settings.hs      # Test configuration
│   └── DSL/
│       ├── Core.hs      # TestM monad, testCase, runDb
│       ├── Given.hs     # Setup primitives (insert VMs, disks, keys, etc.)
│       ├── When.hs      # Action primitives (RPC calls via daemon)
│       └── Then.hs      # Assertion primitives
└── Corvus/
    └── *Spec.hs         # Unit tests (Hspec, no QEMU / no daemon process)
```

Integration tests live in `integration_tests/` (pytest); `test/` is
Haskell unit tests only. Tests use a custom BDD DSL (`Test.DSL.*`)
with `testCase`, `given`, `when_`, `then_`.

## Build System

Stack + Hpack (`package.yaml` → `corvus.cabal`), LTS-23.28 resolver.

### Make Targets

| Target | Description |
|---|---|
| `make build` | Stack build |
| `make install` | Install binaries to `~/.local/bin/` + systemd user service |
| `make format` | Fourmolu formatting (in-place) |
| `make lint` | HLint static analysis |
| `make unit-tests` | Haskell unit tests (the full Haskell suite — no integration tests left here) |
| `make integration-tests` | Pytest integration suite under `integration_tests/`; accepts `MATCH=<pytest -k expr>` |
| `make all-tests` | Alias for `make unit-tests` |

### Build Dependencies

No external C libraries required. The C namespace manager (`cbits/netns.c`) uses only Linux kernel interfaces (`unshare`, `signalfd`, `capget`/`capset`).

### Integration Test Notes

- Integration tests require QEMU/KVM access.
- Virtual networking tests (`NetworkIntegrationSpec`) run unprivileged using user namespaces — no root or doas required. The kernel must have `CONFIG_USER_NS=y` (check with `unshare --user echo ok`).
- Log level during tests is controlled by `CORVUS_TEST_LOG_LEVEL` env var (default: `info`). Use `CORVUS_TEST_LOG_LEVEL=debug` for verbose output.

## Documentation

User-facing documentation lives in `doc/`. See `doc/INDEX.md` for the full list.

| File | Content |
|---|---|
| `doc/README.md` | Project overview, installation, quick start (symlinked from repo root) |
| `doc/vm-management.md` | VM lifecycle, state machine, serial console |
| `doc/disk-management.md` | Disk CRUD, overlays, clones, rebase, attach/detach |
| `doc/snapshots.md` | qcow2 snapshot operations |
| `doc/networking.md` | Virtual networks, network interfaces, namespace exec |
| `doc/ssh-keys.md` | SSH key management |
| `doc/cloud-init.md` | Cloud-init ISO generation, SSH key injection, Windows support |
| `doc/shared-directories.md` | virtiofs shared directories |
| `doc/guest-exec.md` | Guest agent command execution |
| `doc/task-history.md` | Async operation tracking |
| `doc/templates.md` | VM template YAML schema and instantiation |
| `doc/apply-configuration.md` | Declarative environment YAML reference |

## Project Rules

### Handler implementation

All mutating operations must be implemented as Action types (instances of the `Action` type class from `Corvus.Action`), not as plain `handle*` functions. Each Action is a data type carrying its parameters, with `actionExecute` containing the logic. The `handle*` functions are internal implementation details called only by their own Action instance — they should not be exported or called directly from other modules.

When one handler needs to invoke another handler's logic, it should use the other handler's Action type via `actionExecute`, `runAction`, or `runActionAsSubtask` — never by calling the `handle*` function directly.

Read-only operations (list, show, get) are the exception — they are dispatched directly in `Handlers.hs` without Action wrapping since they don't need task recording.

### After any code changes

Always run `make format` and `make lint` after modifying Haskell source files. Fix all lint warnings before committing.

```
make format
make lint
```

### Backwards compatibility

Corvus is in early beta. Breaking changes — to YAML schemas, the binary RPC protocol, the database schema, the CLI, and anything else — are accepted without compatibility shims, deprecation paths, or transitional warnings. When renaming a field or removing a feature, change every call site outright; do not keep the old name as an alias and do not add migration code that detects and rewrites legacy inputs. Make the break clean and update the docs and examples in the same commit.
