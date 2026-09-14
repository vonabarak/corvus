# Corvus Agent Guide

Corvus manages QEMU/KVM VMs. Haskell code lives in `src/` and `app/`;
`python/` contains clients, admin tools, web gateway, and desktop GUI;
`frontend/` is the React/Vite SPA; `integration_tests/` is the nested-VM harness.

## Essential context

- The daemon owns the database, orchestration, scheduling, and task tracking.
  The nodeagent owns per-host VM processes and storage; netd owns privileged
  host networking. Web and desktop clients use the Python Cap'n Proto client.
- `schema/` is the RPC source of truth; regenerate Haskell bindings in
  `src-generated/` with `make capnp` after schema edits.
- `src/Corvus/Model.hs` defines the Persistent database schema. Fresh databases
  create the current schema without migrations; existing SQLite/PostgreSQL
  databases use versioned modules in `src/Corvus/Database/Migrations/`.
  Read the migration guide before changing schema versions or migrations.
- `package.yaml` is the Hpack source for generated `corvus.cabal`.
  Use `make build` for the Stack build.

## Implementation rules

- Implement every mutating operation as a `Corvus.Action` Action type carrying
  its parameters, with logic in `actionExecute`. Plain `handle*` helpers are
  private to their own Action instance: do not export them or call them from
  other modules. Invoke another handler through its Action using
  `actionExecute`, `runAction`, or `runActionAsSubtask`. Read-only operations
  (list/show/get/ping) are dispatched directly without task recording.
- Output references to other entities use shared `NamedRef` with nested
  `{id, name}`, never flat `<role>_id` plus `<role>_name` fields. Name fields
  by their role (e.g. `disk_image`). Exceptions: `TaskInfo.parent_id` stays
  flat; input lookups use `Common.EntityRef`; database-only references that
  never appear in CLI/REST/client output are exempt. Read the RPC guide for
  language mappings and nullable conversion.
- Corvus is early beta: breaking changes to YAML, RPC, database schema, CLI,
  client types, and other public surfaces are accepted. Update all call sites,
  docs, and examples in the same commit. Do not add compatibility aliases,
  shims, deprecation paths, transitional warnings, or migration code that
  detects and rewrites legacy inputs. Versioned database upgrades still follow
  the migration guide.
- Before adding or renaming enums, check `src/Corvus/Model.hs`,
  `Corvus.Protocol.*`, and `Corvus.Wire.Enums`.

## Required commands

- After Haskell or Python source changes, run `make format`, then `make lint`.
  Formatting edits files; lint checks are read-only. Fix all lint warnings
  before committing.
- Both commands include frontend checks when `frontend/node_modules/` exists.
  If it is absent and frontend code changed, run the necessary frontend setup
  and frontend-specific formatting/checks.
- Run `make test` before **every commit**, unless the user explicitly requests
  narrower verification. It runs Haskell unit tests, Python tests, then
  integration tests and stops on the first failure.
- For “bump version”, “set version”, or “release X.Y.Z.W”, use
  `make set-version VERSION=X.Y.Z.W`; never hand-edit the version files.

## Read when relevant

Read the linked sections before working in the corresponding area; do not load
all reference documents for every task.

| Task | Required reference |
|---|---|
| Locate code or change component ownership | [Architecture](doc/architecture.md) |
| Change handlers or shared implementation conventions | [Implementation conventions](doc/development.md#implementation-conventions) |
| Build, configure dependencies, or add unit/Python tests | [Development guide](doc/development.md) |
| Change RPC, DTOs, or response fields | [RPC protocol](doc/rpc-protocol.md), especially [output references](doc/rpc-protocol.md#output-references) |
| Change database schema, versions, or migrations | [Database migrations](doc/database-migrations.md) |
| Add or change integration tests | [Writing tests](integration_tests/README.md#writing-tests) and [prerequisites](integration_tests/README.md#prerequisites) |
| Change TLS, certificates, or deployment security | [Security](doc/security.md) |

Find other feature guides in [doc/INDEX.md](doc/INDEX.md).

## Maintaining this guide

Keep durable coding rules, essential context, and reading triggers here.
Put inventories, detailed explanations, and shared human/agent guidance in
the linked docs. Update the existing source of truth instead of duplicating it,
and use tool-neutral wording.
