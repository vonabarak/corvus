# Development Guide

This guide covers the local development workflow and shared implementation
conventions. See [architecture](architecture.md) to locate components and
[the documentation index](INDEX.md) for feature-specific guides.

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
| `make code-metrics` | Report and enforce size limits for authored Haskell modules and top-level value definitions using the GHC parser |
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

Use `make set-version VERSION=X.Y.Z.W` for version bumps and releases; do not
hand-edit version files.

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
live in `python/tests/`. Haskell `test/` is unit-test focused and uses the custom
BDD DSL (`Test.DSL.*`) with `testCase`, `given`, `when_`, `then_`.

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
