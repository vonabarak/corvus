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
├── CloudInit.hs         # Cloud-init NoCloud ISO generation
├── Handlers/
│   ├── Core.hs          # Ping, status, shutdown
│   ├── Vm.hs            # VM lifecycle + state machine validation
│   ├── Disk.hs          # Disk image CRUD, snapshots, attach/detach
│   ├── Template.hs      # YAML template parsing and instantiation
│   ├── SshKey.hs        # SSH key management
│   ├── NetIf.hs         # Network interface configuration
│   └── SharedDir.hs     # Virtiofs shared directories
├── Qemu/
│   ├── Config.hs        # QEMU binary paths, base directory
│   ├── Runtime.hs       # XDG runtime dir for sockets (HMP, QMP, SPICE)
│   ├── Process.hs       # QEMU process spawning and PID management
│   ├── Qmp.hs           # QMP protocol client (shutdown, pause)
│   ├── QmpQQ.hs         # Quasi-quoter for QMP JSON commands
│   ├── Command.hs       # QEMU command-line builder
│   ├── Image.hs         # qemu-img wrapper (create, resize, snapshot, clone)
│   └── Virtiofsd.hs     # virtiofsd process management
├── Client/
│   ├── Connection.hs    # Socket management, binary protocol
│   ├── Rpc.hs           # High-level RPC call wrappers
│   ├── Parser.hs        # optparse-applicative CLI parsing
│   ├── Commands.hs      # Command execution dispatcher
│   ├── Commands/*.hs    # Domain-specific command handlers
│   ├── Output.hs        # Text/JSON output formatting
│   └── Config.hs        # Client configuration
└── Utils/
    └── Yaml.hs          # YAML parsing with quasi-quoters
```

### Key Patterns

- **Concurrency**: STM (`TVar`) for shared server state; `async` for background VM process monitoring
- **Database**: Persistent + Esqueleto ORM with PostgreSQL; `runSqlPool` pattern; auto-migration on startup
- **Logging**: `MonadLogger` (LoggingT transformer)
- **VM state machine**: Enforced in `Handlers.Vm.validateTransition` — stopped → running → paused, with reset always allowed

### Database Entities

`Vm`, `DiskImage`, `Drive`, `NetworkInterface`, `SharedDir`, `Snapshot`, `SshKey`, `VmSshKey`, `TemplateVm`, `TemplateDrive`, `TemplateNetworkInterface`, `TemplateSshKey` — all defined in `Model.hs`.

### Key Enums (text-serializable via `EnumText` typeclass)

`VmStatus` (stopped/running/paused/error), `DriveInterface`, `DriveFormat`, `DriveMedia`, `CacheType`, `NetInterfaceType`, `SharedDirCache`, `TemplateCloneStrategy` (clone/overlay/direct).

### Test Structure

```
test/
├── Spec.hs              # hspec-discover entry point
├── Test/
│   ├── Prelude.hs       # Re-exports, test utilities
│   ├── Database.hs      # Test database setup/teardown
│   ├── Settings.hs      # Test configuration
│   ├── Daemon.hs        # Daemon process helpers
│   ├── DSL/             # BDD-style DSL (testCase/given/when_/then_)
│   └── VM/              # VM test helpers (SSH, console, images)
└── Corvus/
    ├── *Spec.hs              # Unit tests
    └── *IntegrationSpec.hs   # Integration tests (require QEMU/KVM)
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

## Project Rules

### After any code changes

Always run `make format` and `make lint` after modifying Haskell source files. Fix all lint warnings before committing.

```
make format
make lint
```
