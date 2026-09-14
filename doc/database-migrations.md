# Database migrations: developer guide

Corvus creates empty databases from the current Persistent model and upgrades
existing databases using compiled Haskell migration modules. SQLite and
PostgreSQL follow the same version sequence; each migration implements both
backends. No migration files need to be installed alongside the daemon.

## Startup and version metadata

`Corvus.Database.runDatabaseMigrations` reads `schema_version` before starting
the daemon's listeners. The table contains exactly one row, with `id = 1` and a
positive integer `version`. This schema version is separate from the database
engine's version reported by `crv status`.

- No application tables and no version: create the current schema directly
  using `Corvus.Model.migrateAll`, then record `currentSchemaVersion`. An empty
  `schema_version` table is allowed. No historical migration actions run.
- Stored version equals current: leave the schema untouched.
- Stored version exceeds current: refuse startup.
- Stored version is older: validate the complete upgrade path, then execute
  each required migration in ascending target-version order.
- Existing tables without a valid version record: refuse startup. Corvus does
  not infer a version from their structure or automatically repair the schema.

PostgreSQL inspection uses `current_schema()`, matching unqualified SQL in the
schema operations. SQLite creation retains `AUTOINCREMENT` semantics.

The runner validates registry entries and all required transitions before
changing the database. Every upgrade step and its version update run inside
one transaction for the whole upgrade. A failure rolls back earlier steps too;
the daemon reports the failing transition and exits. Migration actions must
not commit transactions or perform external side effects that cannot roll back.

## Adding a migration

1. Change the Persistent model to describe the new schema.
2. Increment `currentSchemaVersion` in `Corvus.Database`.
3. Add `src/Corvus/Database/Migrations/VNNN.hs`, where `NNN` is the target
   version. For example, `V004` upgrades exactly version 3 to version 4.
4. Import the module and add its `migration` to the registry in
   `Corvus.Database.Migrations`. Let Hpack regenerate `corvus.cabal` during
   the build.
5. Test both backends using the previous schema and representative rows.

Each module exports one `Migration`:

```haskell
module Corvus.Database.Migrations.V004 (migration) where

import Corvus.Database.Migration
import Database.Persist.Sql (SqlPersistT, rawExecute)

migration :: Migration
migration = Migration 4 "Describe the schema change" upgrade

upgrade :: DatabaseEngine -> SqlPersistT IO ()
upgrade DatabasePostgresql = rawExecute "... PostgreSQL SQL ..." []
upgrade DatabaseSqlite = rawExecute "... SQLite SQL ..." []
```

The action runs against exactly the preceding schema version. It must produce
the complete target schema, including constraints, indexes, defaults, and any
data transformations. Use fixed SQL or historical representations, not the
current `Corvus.Model` types or `migrateAll`: those evolve independently of
already released migrations. There is no automatic Persistent reconciliation
after an upgrade.

For SQLite table rebuilds, explicitly preserve foreign keys, uniqueness,
indexes and sequence high-water marks. `V003` demonstrates this for nullable
`drive.disk_image_id`. Do not toggle `PRAGMA foreign_keys` inside the runner's
transaction; SQLite ignores that toggle within transactions.

## Retiring old migrations

Once installations no longer need a transition, delete its module and remove
its registry entry/import. Do not renumber retained migrations or lower
`currentSchemaVersion`. No runner refactoring or separate minimum-version
constant is required.

For example, at current version 5 with only `V005` retained:

| Starting database | Result |
|---|---|
| Empty | Create version 5 directly |
| Version 5 | Skip migrations |
| Version 4 | Apply `V005` |
| Version 3 | Refuse: migration 3 → 4 is missing |
| Version 6 | Refuse: database is newer than the binary |

An empty registry permits fresh creation and already-current databases only.
Never skip a missing intermediate transition. Operators with older databases
must first use a release containing the required migrations. Downgrades and
replacement migrations spanning multiple versions are not supported.

The initial retained history is `V003` (2 → 3); databases at version 1 are no
longer supported directly.

## Tests and fixtures

`test/fixtures/database/` contains frozen SQLite and PostgreSQL version-2 DDL
from commit `c23bc5c` and portable sample rows. Both the Haskell database tests
and Python integration tests use these fixtures. Preserve historical fixtures;
do not regenerate them from the latest model or merely change the version
number on a fresh database. New migrations should add fixtures for their
starting schema, with data exercising the transformation.

The Haskell suite covers planning, missing transitions, fresh creation,
metadata errors, real upgrades, and transaction rollback. The integration
suite's `test_database_migrations.py` starts the newly built daemon against
the older schema on each backend, checks RPC readiness and persisted results,
then restarts it to check migration skipping. It also tests fresh creation
and refusal when a required migration has been retired. The two backend
classes have independent class-scoped test nodes and are marked `slow`.

```sh
TEST_DB_BACKEND=sqlite make unit-tests MATCH=Database
TEST_DB_BACKEND=postgresql make unit-tests MATCH=Database
make integration-tests MATCH=database_migrations
make format
make lint
make test  # required before committing
```

PostgreSQL unit tests use `TEST_DB_HOST`, `TEST_DB_PORT`, `TEST_DB_USER`,
`TEST_DB_PASSWORD`, and `TEST_DB_ADMIN` when set. Integration tests use the
PostgreSQL server or SQLite file inside their isolated test nodes, regardless
of the host test-backend setting. See the integration-test README for nested
QEMU/KVM and image prerequisites.
