# Development Guide

This guide covers the local development workflow and shared implementation
conventions. See [architecture](architecture.md) to locate components and
[the documentation index](INDEX.md) for feature-specific guides.

## Build System

Stack + Hpack (`package.yaml` -> `corvus.cabal`), LTS-23.28 resolver.
Development executables and their dependencies use `tools/package.yaml` and
`tools/stack.yaml`; normal application builds do not build that package.
HLint 3.8 and Fourmolu 0.15.0.0 are pinned in the tools Stack project.
`make format` and `make check` build and invoke those versions automatically;
no global HLint or Fourmolu installation is required.

### Makefile organization

The root `Makefile` sets the default build goal and shared environment, then
includes six workflow files under `make/`. Each file owns its targets, phony
declarations, workflow-specific settings, and recipe documentation:

| File | Workflow |
|---|---|
| `make/build.mk` | Build, schema generation, Python environment, installation, and cleanup |
| `make/tests.mk` | Haskell, Python, and integration test suites |
| `make/quality.mk` | Formatting, static checks, type checks, metrics, and Git hook setup |
| `make/ui.mk` | Web and desktop build/run workflows |
| `make/vms.mk` | Image command forwarding and developer VM workflows |
| `make/release.mk` | Release staging and version updates |

Run targets through the root Makefile; fragment paths are relative to the
repository root. Image recipes remain owned by their Makefiles under `yaml/`.
`STACK_BUILD_FLAGS` applies to every Stack invocation in these workflows.

### Make Targets

| Target | Description |
|---|---|
| `make build` | Stack build |
| `make capnp` | Regenerate `src-generated/Capnp/Gen/*.hs` from `schema/*.capnp` |
| `make install` | Install Haskell binaries, shell completions, Python tooling, and web assets when available |
| `make format` | Ruff + Fourmolu formatting; also frontend formatting when `frontend/node_modules/` exists |
| `make yaml-schema-check` | Validate the build YAML editor schema and repository recipes without starting a daemon |
| `make check` | Formatting/static checks, strict Haskell build, code metrics, Weeder, and full Haskell tests with a coverage gate; also frontend checks when `frontend/node_modules/` exists |
| `make desktop-typecheck` | Strict mypy check for the desktop package and its tests; requires the `desktop` extra |
| `make code-metrics` | Report and enforce size limits for authored Haskell modules and top-level value definitions using the GHC parser |
| `make unit-tests` | Haskell unit tests; accepts `MATCH=<hspec pattern>` |
| `make venv` | Create/update root `.venv` with system site packages, Python test dependencies, and lint tools |
| `make python-test` | Python client/admin/web/desktop tests against a temp daemon |
| `make integration-tests` | Pytest nested-VM suite; accepts `MATCH=<pytest -k expr>` and `WORKERS=N` |
| `make integration-tests-clean` | Remove leftover `corvus-it-*` VMs/networks from aborted runs |
| `make test` | Umbrella target: `unit-tests`, `python-test`, then `integration-tests` |
| `make web-build` | Build frontend SPA and copy it into `python/corvus_web/static/` |
| `make web-dev` | Run Vite dev server for frontend work |
| `make web-lint` / `make web-format` | Frontend-specific checks/formatting |
| `make desktop-run` | Run the desktop GUI from the local tree |
| `make install-git-hooks` | Enable repository-managed Git hooks for this checkout |
| `make set-version VERSION=X.Y.Z.W` | Bump `package.yaml`, `corvus.cabal`, and `pyproject.toml` together |
| `make release` | Stage release tree and tarball with binaries, completions, Python artifacts, docs, YAML, and schema |

Use `make set-version VERSION=X.Y.Z.W` for version bumps and releases; do not
hand-edit version files.

## Git Hooks

Enable the repository-managed hooks after cloning:

```
make install-git-hooks
```

The pre-commit hook runs `make check` and blocks a commit when it fails.

## Build Dependencies

Use Stack with the resolver in `stack.yaml`; Stack can install the matching GHC.
Database backend dependencies come from the enabled `with-sqlite` and
`with-postgresql` flags in `package.yaml`. A PostgreSQL server is needed for
PostgreSQL-backed tests, not for the default SQLite runtime. Python 3.10+ is
required for the tools and clients. Frontend work needs Node.js/npm, and desktop
work needs the `desktop` Python extra.

Running VMs and integration tests also needs QEMU/KVM, `qemu-img`, and the
optional helpers for the exercised features (`virtiofsd`, `swtpm`, `dnsmasq`,
ISO creation tools, and viewers). See the [runtime prerequisites](README.md#prerequisites)
and [integration prerequisites](../integration_tests/README.md#prerequisites).

## Tests

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
live in `python/tests/`. Development-tool tests live in `tools/test/` and run alongside application
tests in `make unit-tests`. Haskell `test/` is unit-test focused and uses the custom
BDD DSL (`Test.DSL.*`) with `testCase`, `given`, `when_`, `then_`.

`make venv` creates the shared `.venv` with access to system Python packages
(including a host-installed PySide6) and installs the package with the `harness`,
`desktop`, and `dev` extras. `make python-test` and `make integration-tests` run it
automatically. Run `make venv` before `make check` or `make typecheck-core`; those
targets use the venv's Ruff and mypy rather than tools from the system PATH.

See the [integration harness guide](../integration_tests/README.md) for nested
QEMU/KVM prerequisites, test images, scheduling rules, and debugging. Database
changes also require the [migration guide](database-migrations.md), including
its PostgreSQL and SQLite upgrade coverage.

## Implementation conventions

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

### Backwards Compatibility

Corvus is in early beta. Breaking changes to YAML schemas, RPC protocol,
database schema, CLI, Python/TypeScript client shapes, and other public surfaces
are accepted without compatibility shims, deprecation paths, or transitional
warnings. When renaming a field or removing a feature, change every call site
outright; do not keep the old name as an alias and do not add migration code
that detects and rewrites legacy inputs. Make the break clean and update docs
and examples in the same commit.

### Response references

Use shared `NamedRef` output references as specified in the
[RPC guide](rpc-protocol.md#output-references), including nullable conversion,
role-based field naming, and the documented exceptions.

## Verification

### After Code Changes

Haskell builds enable `-Wall` and treat every enabled warning as an error.
Additional checks cover incomplete matches and record updates, missing fields and
methods, partial record selectors, deriving strategies, module export lists,
unused packages, and partial list operations. The shared options in
`package.yaml` apply to the library, executables, and tests; the tools package
and maintained vendor packages enable the same checks. Generated Cap'n Proto
modules retain narrow warning exceptions emitted by the vendored generator.
Tagged internal responses and errors use positional constructor arguments to
avoid partial record selectors.

Run `make format` and `make check` after modifying Haskell or Python source.
Formatting edits files; checks do not modify tracked files. Fix all diagnostics
before committing. `make check` replaces the former `lint` target and includes:

- Existing HLint, formatter, Ruff, mypy, and optional frontend checks.
- A separate Haskell build in `.stack-work-quality`, with both database backends,
  HPC instrumentation, and HIE files. Normal builds remain uninstrumented.
- Code metrics, coverage checking, and their unit tests live in the separate
  `tools/package.yaml` package. `tools/stack.yaml` pins Weeder 2.10.0 and
  GHC 9.8.4, matching the application compiler. The main package builds only
  production executables.
  `weeder.toml` roots executable entry points, generated schema modules, and
  compile-time quasiquoters, generated RPC/Persistent instances, and standard
  deriving, serialization, exception, and overloaded-syntax instances. These
  instances retain the data types’ supported behavior even where current call
  sites use only some of it. Other instances remain subject to analysis. Add
  roots only with a documented reason; remove genuinely unused code.
- The complete Haskell suite using SQLite, one worker, and seed `20261010`,
  followed by overall and CLI expression coverage gates. `MATCH` is rejected here;
  use `make unit-tests MATCH=...` for focused work.

Coverage includes every compiled authored library module under `src/`, including
modules absent from the runtime trace (counted as uncovered). Generated code,
executables, and tests are excluded. Authored library modules in
`vendor/haskell-capnp/capnp/lib/` and `vendor/lifetimes/src/` are included; traces
from the Corvus, capnp-tests, and lifetimes suites are merged by package unit
and module, with missing modules counted as uncovered. The overall minimum in
`coverage-baseline.json` is **51%**, represented as the exact ratio **51 / 100**.
In addition to the overall minimum, the gate requires
at least **80%** across `Corvus.Client` and its submodules, and **80%** in each
nonempty `Corvus.Client.Commands` module and submodule. New command modules enter
the gate automatically; unexecuted modules count as zero coverage. The CLI limits
are recorded in `coverage-baseline.json`. The gate compares exact integer ratios,
so display rounding cannot hide a decrease. Missing, malformed, stale, or
incompatible artifacts fail the check. HTML reports are written to
`.stack-work-quality/authored-coverage/hpc_index.html`, including module details.
Review baseline increases explicitly; never lower it automatically to make a
change pass. Coverage changes should be addressed with useful tests or removal
of unreachable code.

The `capnp`, `capnp-tests`, and `lifetimes` directories are local Stack packages.
`make unit-tests` runs their upstream Haskell suites alongside Corvus tests.
Only the calculator client/server are included as test support; standalone
examples and benchmarks remain outside the project. C++ interoperability tests
remain opt-in through the environment variables documented in the vendor tests.
`make format` and `make check` use the root formatter and HLint configuration for
authored vendor sources, including the generator and test support. Code metrics
apply the same 1000-line module and 300-line definition limits. Weeder analyzes
all application and vendor components together without rooting unused public
vendor APIs. Development tools retain their separate dependency graph.

Regenerate vendored schema fixtures with `scripts/regen-vendor-schemas.sh`.
The script builds the local generator and regenerates the core, test, and
calculator and Echo schemas. Generated bindings are excluded from formatting, metrics,
and authored coverage; their warning exceptions are emitted by the generator.

CLI unit tests use a strict in-process Cap'n Proto peer over temporary Unix
sockets. They invoke the command dispatcher, decode and check requests, script
results and failures, and capture output and exit status. Unexpected or missing
RPCs fail a test. Local files and editor/viewer programs use temporary directories;
interactive terminal cases use pseudo-terminals and restore file descriptors and
terminal settings. These specs run sequentially because they change process-wide
handles and environment variables. They require no daemon, database, VM, display,
or installed editor/viewer. Use `make unit-tests MATCH=CLI` for focused runs.

When `frontend/node_modules/` exists, both targets also cover frontend
formatting/linting. If it does not exist and the change touched frontend code,
run the frontend-specific setup/checks needed for that work.

```
make format
make check
```

Desktop changes additionally require a desktop-enabled environment and:

```
pip install -e '.[harness,desktop]'
make desktop-typecheck
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

### QGA connection diagnostics

During startup, the first-ping coordinator owns QGA readiness. Periodic
five-second health probes begin only after that coordinator succeeds; they
must not open and cancel a competing handshake while the guest is booting.
A failed startup remains errored and is not retried by the periodic poller.
QMP resource-counter queries have a one-second deadline per exchange: missing
statistics must not hold up startup notifications or periodic status delivery.

The nodeagent logs QEMU guest agent (QGA) connection lifecycle events at `info`,
failures at `warn`, and command send/reply metadata and socket reuse at `debug`.
Use the nodeagent's `--log-level debug` option when investigating a stall.
The messages use the `qga` logging source and include UTC timestamps, VM ID,
a process-local connection ID, operation ID, caller source stack, thread ID,
monotonic timestamp, command sequence, phase, and monotonic elapsed
milliseconds for the connection and current phase. Connection IDs restart when
the nodeagent restarts; retain journal timestamps and process identity alongside them.

At `info` level, `request_start`/`request_end` bracket every operation, including
callers that never acquire a socket. `lock_wait`, `lock_acquired`, and
`lock_wait_cancelled` distinguish queue contention from a stalled handshake.
Operation IDs follow cached connections across reuse, while connection IDs
identify individual socket attempts. Correlate `thread_id` with the enclosing
`startup_ping_*` and `status_*` events to identify which outer deadline cancelled
a request. Lifecycle release/invalidation events include their caller even when
no socket is cached. All IDs are process-local.

`runtime_publish_*`, `runtime_published`, `startup_coordinator_start`,
`startup_ready`, and `startup_timeout` record QEMU PID, runtime generation,
lifecycle revision, readiness fence, and configured boot budget. Status probes
record the same state plus their origin (`periodic` or `dispatch`) and decision
to probe/skip. Startup attempts record their remaining outer budget; status
probes explicitly report `outer_timeout` rather than an unexplained cancellation.
These causal events are available at the default `info` level; enable `debug`
for individual sends/replies and stale synchronization replies.

Collect the entire affected nodeagent journal, retaining systemd PID/boot fields
and the period before VM publication, not only lines containing the failed VM:

```sh
journalctl --user -u corvus-nodeagent.service --since '30 minutes ago' -o short-precise --no-pager
```

For nested nodes, run the corresponding system-service journal command on the
node (`journalctl -u corvus-nodeagent.service ...`). Retain guest QGA service logs
and console output before restarting either guest or nodeagent.

Follow one `vm_id` and `connection_id` from `connect_start` through
`sync_start`, command traffic, and `close_complete`. A `send_start` without
`send_complete` locates a blocked send; a completed send followed by
`operation_timeout` in `phase=receive` locates a missing reply. Timeout events
include the budget in microseconds. `operation_cancelled` identifies external
cancellation separately. Close events include the reason (timeout, exception,
shutdown discard, explicit invalidation, or lifecycle release). A new connection
ID identifies a reconnect; debug `sync_mismatch` events identify stale sync replies.

Logs contain command names and reply classifications, including sanitized QGA
error classes, but never command arguments, scripts, stdin, file data, keys,
raw reply JSON, or error descriptions. Synchronous logger failures do not fail
QGA operations. These diagnostics do not establish why a guest stopped replying;
retain guest console output and QEMU/QGA logs alongside the nodeagent journal.

### Concurrent mutations

Mutating Actions declare their image dependencies with `actionResources` and
object identities with `actionObjectResources`. The runner acquires their union
atomically through `Corvus.OperationGuard`. Names use the database uniqueness
scope: templates, SSH keys and nodes are global; VM and network names include
the numeric node ID. Node edits reserve both old and new names and endpoints;
VM migration reserves both source and destination namespaces. Operations on
unique children reserve their owning object before checking or inserting.

Use shared access for referenced objects and exclusive access for identities
being changed or deleted. Include every resource needed by a composite operation
before its first mutation. Resolve automatic placement in `actionPrepare` and
execute on that same node. Preparation must be read-only. Resource discovery is
repeated after acquisition; stale sets are released and retried within the
original 60-second deadline, before executing any mutation.

Pass the leased `ActionContext` to nested Actions with `runActionAsSubtask` or
`executeCreate`. Starting an independent `runAction` loses inherited leases and
can deadlock against the parent. Shared-to-exclusive upgrades are rejected;
request the strongest access needed by the entire operation initially.

Apply holds an entity's reservation from lookup through policy selection,
overwrite preflight, deletion and complete creation. Keep all of those steps
inside the reservation when adding new apply entity types. These reservations
coordinate requests within one daemon; running multiple daemons against one
database is not supported.

## Maintaining documentation

Update the affected guides and examples alongside any public behavior change.
Use the implementation as the reference:

| Documentation surface | Source of truth |
|---|---|
| CLI commands, flags, defaults | `src/Corvus/Client/Parser.hs`, `src/Corvus/Client/Parser/`, executable `--help` |
| Daemon/agent listeners and startup | `app/`, `src/Corvus/Types.hs`, `python/corvus_admin/` |
| YAML fields and execution rules | apply/template/build parsers and handlers under `src/Corvus/`; build editor schema under `schema/` |
| RPC and output records | `schema/`, `src/Corvus/Protocol/`, `src/Corvus/Wire/`, `python/corvus_client/types.py` |
| Python API and GUI/gateway flags | `python/corvus_client/`, `python/corvus_desktop/cli.py`, `python/corvus_web/cli.py` |
| Build, tests, releases, image recipes | `make/`, recipe Makefiles under `yaml/`, test fixtures |

When adding, renaming, or removing a first-party Markdown guide, update both the
[manual's Documentation table](README.md#documentation) and the
[categorized index](INDEX.md). Include guides outside `doc/`, recipe READMEs,
and authored technical notes. Exclude agent policy and upstream vendored docs.
Keep descriptions consistent in both catalogs and use relative links so guides
work in a checkout and the release tree. Update `make/release.mk` when a guide
outside the already copied trees needs staging. Source-code links in developer
guides require a source checkout; a binary release contains documentation and
examples, not the entire development tree.

Before finishing a documentation change:

- Check local links, image paths, and heading anchors from each file's directory,
  including the release layout when packaging changes.
- Compare examples with current parser/help output; label placeholders and state
  which machine owns a path. Do not run example mutations against an existing
  user's environment merely to check syntax.
- If YAML recipes or the editor schema changed, run `make yaml-schema-check`.
  Syntax-check complete Python examples and verify API names against the client.
- Walk the first-VM instructions from a fresh installation: binaries and Python
  tools, runtime dependencies, certificates, agent registration, readiness, and
  guest access. Record anything that could only be checked statically.
- Run `git diff --check`. Follow the normal format/check/test requirements when
  source changes accompany the documentation, and `make test` before committing.
