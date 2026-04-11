# Corvus

QEMU/KVM virtual machine management daemon with a CLI client, written in Haskell.

## Architecture

### Executables

- **`corvus`** — daemon that manages VM lifecycle, listens on TCP or Unix socket
- **`crv`** — CLI client that communicates with the daemon via binary RPC

### Communication

Binary RPC protocol (`Data.Binary`) over TCP or Unix socket. Request/response types defined in `src/Corvus/Protocol.hs`.

### Module Layout

```
src/Corvus/
├── Protocol.hs          # Binary RPC message types (Request/Response)
├── Types.hs             # Server state, config, listen addresses
├── Server.hs            # Socket listener, request dispatch
├── Model.hs             # Persistent ORM schema, enums, migrations
├── Handlers.hs          # Central request dispatcher
├── CloudInit.hs         # Cloud-init NoCloud ISO generation + SSH key injection
├── Handlers/
│   ├── Core.hs          # Ping, status, shutdown
│   ├── Vm.hs            # VM lifecycle + state machine validation
│   ├── Disk.hs          # Disk image CRUD, snapshots, attach/detach, HTTP import
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
│   ├── Commands/
│   │   ├── Vm.hs        # VM command handlers and display
│   │   ├── Disk.hs      # Disk command handlers (incl. HTTP import)
│   │   ├── Template.hs  # Template command handlers
│   │   ├── Apply.hs     # Apply command handler
│   │   ├── Network.hs   # Network command handlers
│   │   ├── NetIf.hs     # Network interface command handlers
│   │   ├── SshKey.hs    # SSH key command handlers
│   │   ├── SharedDir.hs # Shared directory command handlers
│   │   ├── GuestExec.hs # Guest exec command handlers
│   │   ├── CloudInit.hs # Cloud-init config command handlers
│   │   └── Task.hs     # Task history command handlers
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
- **VM state machine**: Enforced in `Handlers.Vm.validateTransition` — stopped → starting (if guest agent) or running → stopping → stopped. VMs with guest agent go through `starting` state until first healthcheck ping succeeds. Reset always returns to stopped.
- **Task tracking**: Every mutating request is recorded in the `task` table via `withTask` wrapper in `Handlers.hs`. Read-only requests (list, show, ping) are skipped. Startup and shutdown are also recorded as tasks.
- **Network namespaces**: A single shared user+network+UTS namespace is created at daemon startup (no root required). The C helper (`cbits/netns.c`) forks from the Haskell process, calls `unshare(2)` in the single-threaded child, brings up loopback, and waits. `unshare(CLONE_NEWUSER)` requires a single-threaded process — the fork **must** happen in C, not via GHC's `forkProcess`, because GHC's threaded RTS keeps multiple OS threads alive after fork. All bridges, dnsmasq instances, and QEMU processes are managed inside this namespace via `nsenter` from Haskell.

### Database Entities

`Vm`, `DiskImage`, `Drive`, `Network`, `NetworkInterface`, `SharedDir`, `Snapshot`, `SshKey`, `VmSshKey`, `TemplateVm`, `TemplateDrive`, `TemplateNetworkInterface`, `TemplateSshKey`, `CloudInit`, `TemplateCloudInit`, `Task` — all defined in `Model.hs`.

### Key Enums (text-serializable via `EnumText` typeclass)

`VmStatus` (stopped/starting/running/stopping/paused/error), `DriveInterface`, `DriveFormat`, `DriveMedia`, `CacheType`, `NetInterfaceType` (user/tap/bridge/macvtap/managed), `SharedDirCache`, `TemplateCloneStrategy` (clone/overlay/direct), `TaskSubsystem` (vm/disk/network/ssh-key/template/shared-dir/snapshot/system/apply), `TaskResult` (running/success/error).

### Test Structure

```
test/
├── Spec.hs              # hspec-discover entry point
├── Test/
│   ├── Prelude.hs       # Re-exports, test utilities
│   ├── Database.hs      # Test database setup/teardown
│   ├── Settings.hs      # Test configuration
│   ├── DSL/
│   │   ├── Core.hs      # TestM monad, testCase, runDb
│   │   ├── Given.hs     # Setup primitives (insert VMs, disks, keys, etc.)
│   │   ├── When.hs      # Action primitives (RPC calls via daemon)
│   │   └── Then.hs      # Assertion primitives
│   └── VM/
│       ├── Common.hs    # withTestVm* helpers (SSH, console, guest exec)
│       ├── Daemon.hs    # Daemon process lifecycle for integration tests
│       ├── Rpc.hs       # Test RPC helpers (create/start/stop VMs, guest agent)
│       ├── Image.hs     # Test image download/management
│       ├── Ssh.hs       # SSH connection helpers
│       ├── Console.hs   # Serial console helpers
│       └── Types.hs     # Test VM configuration types
└── Corvus/
    ├── *Spec.hs                   # Unit tests
    ├── *IntegrationSpec.hs        # Integration tests (require QEMU/KVM)
    └── NetworkIntegrationSpec.hs  # Virtual networking + namespace tests
```

Tests use a custom BDD DSL (`Test.DSL.*`) with `testCase`, `given`, `when_`, `then_`.

## Build System

Stack + Hpack (`package.yaml` → `corvus.cabal`), LTS-23.28 resolver.

### Make Targets

| Target | Description |
|---|---|
| `make build` | Stack build |
| `make install` | Install binaries to `~/.local/bin/` + systemd user service |
| `make format` | Fourmolu formatting (in-place) |
| `make lint` | HLint static analysis |
| `make unit-tests` | Tests excluding integration |
| `make integration-tests` | Integration tests only (requires QEMU/KVM) |
| `make all-tests` | Full test suite |

### Build Dependencies

No external C libraries required. The C namespace manager (`cbits/netns.c`) uses only Linux kernel interfaces (`unshare`, `signalfd`, `capget`/`capset`).

### Integration Test Notes

- Integration tests require QEMU/KVM access.
- Virtual networking tests (`NetworkIntegrationSpec`) run unprivileged using user namespaces — no root or doas required. The kernel must have `CONFIG_USER_NS=y` (check with `unshare --user echo ok`).
- Log level during tests is controlled by `CORVUS_TEST_LOG_LEVEL` env var (default: `info`). Use `CORVUS_TEST_LOG_LEVEL=debug` for verbose output.

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
