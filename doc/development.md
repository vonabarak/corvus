# Development Guide

This guide covers the local development workflow and shared implementation
conventions. See [architecture](architecture.md) to locate components and
[the documentation index](INDEX.md) for feature-specific guides.

## Build System

Stack + Hpack (`package.yaml` -> `corvus.cabal`), LTS-23.28 resolver.
Development executables and their dependencies use `tools/package.yaml` and
`tools/stack.yaml`; normal application builds do not build that package.

### Make Targets

| Target | Description |
|---|---|
| `make build` | Stack build |
| `make capnp` | Regenerate `src-generated/Capnp/Gen/*.hs` from `schema/*.capnp` |
| `make install` | Install Haskell binaries, shell completions, Python tooling, and web assets when available |
| `make format` | Ruff + Fourmolu formatting; also frontend formatting when `frontend/node_modules/` exists |
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

### Build Dependencies

No external C libraries are required by the Haskell package. Runtime and test
workflows need PostgreSQL, QEMU/KVM, `qemu-img`, and relevant optional tools
(`virtiofsd`, `dnsmasq`, `remote-viewer`, frontend `npm`, desktop extras, etc.)
depending on the area touched.

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
enables the same checks. Generated Cap'n Proto
modules retain narrow warning exceptions emitted by the vendored generator.
Tagged internal responses and errors use positional constructor arguments to
avoid partial record selectors.

Run `make format` and `make check` after modifying Haskell or Python source.
Formatting edits files; checks do not modify tracked files. Fix all diagnostics
before committing. `make check` replaces the former `lint` target and includes:

- Existing HLint, formatter, Ruff, mypy, and optional frontend checks.
- A separate Haskell build in `.stack-work/quality`, with both database backends,
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
  followed by an aggregate expression coverage gate. `MATCH` is rejected here;
  use `make unit-tests MATCH=...` for focused work.

Coverage includes every compiled authored library module under `src/`, including
modules absent from the runtime trace (counted as uncovered). Generated code,
vendor code, executables, and tests are excluded. The initial baseline in
`coverage-baseline.json` is **28110 / 98335 expressions (28.585956%)**, the lowest
of three full runs on revision `fb55a82`. The gate compares exact integer ratios,
so display rounding cannot hide a decrease. Missing, malformed, stale, or
incompatible artifacts fail the check. HTML reports are written to
`.stack-work/quality/authored-coverage/hpc_index.html`, including module details.
Review baseline increases explicitly; never lower it automatically to make a
change pass. Coverage changes should be addressed with useful tests or removal
of unreachable code.

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
