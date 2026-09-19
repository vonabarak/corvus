"""Start the real daemon against frozen historical schemas on both backends."""

from __future__ import annotations

import shlex
import subprocess
import time
from pathlib import Path

import pytest
from corvus_test_harness.cases import SingleNodeCase
from corvus_test_harness.inner import open_client
from corvus_test_harness.postgres import psql, wait_for_postgres
from corvus_test_harness.ssh import HOST_ALPINE_KEY_PATH, NodeShell

FIXTURES = Path(__file__).resolve().parents[2] / "test" / "fixtures" / "database"
DATABASE = "corvus_migration_test"
SQLITE_PATH = "/var/lib/corvus/migration-test.db"


class DatabaseMigrationCase(SingleNodeCase):
    """Each concrete backend owns one isolated outer test node."""

    BACKEND: str

    @property
    def shell(self) -> NodeShell:
        return NodeShell(
            cid=self.node.cid, user="corvus", key_path=HOST_ALPINE_KEY_PATH
        )

    def _stop(self) -> None:
        if self.node._client is not None:
            self.node._client.close()
            self.node._client = None
        self.node.run("sudo systemctl stop corvus.service")

    def _sql(self, sql: str) -> str:
        if self.BACKEND == "postgresql":
            # Objects must belong to the daemon's role for ALTER TABLE.
            return psql(
                self.shell, "SET ROLE corvus; " + sql, db=DATABASE
            ).removeprefix("SET\n")
        script = (
            "import sqlite3\n"
            f"with sqlite3.connect({SQLITE_PATH!r}) as db:\n"
            " db.execute('PRAGMA foreign_keys = ON')\n"
            f" for statement in {sql!r}.split(';'):\n"
            "  if statement.strip():\n"
            "   cursor = db.execute(statement)\n"
            "   for row in cursor:\n"
            "    print('|'.join('' if v is None else str(v) for v in row))\n"
        )
        return (
            self.node.run("python3 -c " + shlex.quote(script)).stdout.decode().strip()
        )

    def _prepare(self, *, historical: bool) -> None:
        self._stop()
        if self.BACKEND == "postgresql":
            self.node.run("sudo systemctl start postgresql-18.service")
            wait_for_postgres(self.shell)
            psql(self.shell, f"DROP DATABASE IF EXISTS {DATABASE}", db="postgres")
            psql(self.shell, f"CREATE DATABASE {DATABASE} OWNER corvus", db="postgres")
            database = f"postgresql://corvus@/{DATABASE}?host=/var/run/postgresql"
        else:
            self.node.run(f"rm -f {SQLITE_PATH} {SQLITE_PATH}-wal {SQLITE_PATH}-shm")
            database = SQLITE_PATH
        if historical:
            self._sql((FIXTURES / f"{self.BACKEND}-v2.sql").read_text())
            self._sql((FIXTURES / "seed.sql").read_text())
        # Disable restart-on-failure so rejected upgrades have a definitive
        # exit status and cannot repeatedly touch the database under inspection.
        unit = (
            "[Service]\nExecStart=\n"
            "ExecStart=/opt/corvus/bin/corvus --host 0.0.0.0 --port 9876 "
            f"--database {database} --log-level debug\nRestart=no\n"
        )
        self.node.run("sudo mkdir -p /etc/systemd/system/corvus.service.d")
        self.node.run(
            "printf %s "
            + shlex.quote(unit)
            + " | sudo tee /etc/systemd/system/corvus.service.d/migration-test.conf >/dev/null"
        )
        self.node.run("sudo systemctl daemon-reload")

    def _start(self) -> None:
        self.node.run("sudo systemctl reset-failed corvus.service")
        self.node.run("sudo systemctl start corvus.service")

    def _logs(self) -> str:
        invocation = (
            self.node.run("systemctl show corvus.service -p InvocationID --value")
            .stdout.decode()
            .strip()
        )
        return self.node.run(
            "sudo journalctl --no-pager -o cat _SYSTEMD_INVOCATION_ID="
            + shlex.quote(invocation)
        ).stdout.decode()

    def _connect(self):
        return open_client(
            self.node.relay,
            cert_dir=self.node._client_cert_dir,
            tls=True,
            ensure_self_node=False,
            boot_timeout_sec=60,
        )

    def test_01_upgrade_version_2_and_restart(self):
        self._prepare(historical=True)
        assert self._sql("SELECT version FROM schema_version") == "2"
        insert = (
            "INSERT INTO drive (vm_id, disk_image_id, interface, media, cache_type) "
            "VALUES (1, NULL, 'ide', 'cdrom', 'none')"
        )
        # Establish that this really is the old schema before starting Corvus.
        with pytest.raises(subprocess.CalledProcessError):
            self._sql(insert)
        self._start()
        with self._connect() as client:
            assert client.status().database_backend == self.BACKEND
            assert client.vms.get("migration-vm").show().name == "migration-vm"
        assert "migrated from version 2 to 4" in self._logs()
        assert self._sql("SELECT version FROM schema_version") == "4"
        assert self._sql("SELECT id, disk_image_id FROM drive") == "1|1"
        self._sql(insert + ";" + insert)
        assert (
            self._sql("SELECT COUNT(*) FROM drive WHERE disk_image_id IS NULL") == "2"
        )
        with pytest.raises(subprocess.CalledProcessError):
            self._sql(insert.replace("NULL", "1"))
        with pytest.raises(subprocess.CalledProcessError):
            self._sql(insert.replace("NULL", "999"))
        index_query = (
            "SELECT indexname FROM pg_indexes WHERE tablename = 'drive'"
            if self.BACKEND == "postgresql"
            else "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'drive'"
        )
        assert "migration_drive_media" in self._sql(index_query)
        self._stop()
        self._start()
        with self._connect() as client:
            assert client.status().database_backend == self.BACKEND
        assert "is current; skipping migrations" in self._logs()
        assert self._sql("SELECT COUNT(*) FROM drive") == "3"

    def test_02_fresh_creation(self):
        self._prepare(historical=False)
        self._start()
        with self._connect() as client:
            assert client.status().database_backend == self.BACKEND
            assert client.vms.list() == []
        assert "Created database schema at version 4" in self._logs()
        assert "migrated from version" not in self._logs()
        assert self._sql("SELECT version FROM schema_version") == "4"

    def test_03_retired_migration_refuses_startup(self):
        self._prepare(historical=True)
        self._sql("UPDATE schema_version SET version = 1")
        before = self._sql("SELECT * FROM drive")
        self._start()
        deadline = time.monotonic() + 30
        while time.monotonic() < deadline:
            state = (
                self.node.run("systemctl show corvus.service -p ActiveState --value")
                .stdout.decode()
                .strip()
            )
            if state == "failed":
                break
            time.sleep(0.2)
        else:
            pytest.fail("daemon did not fail after a required migration was retired")
        assert (
            self.node.run("systemctl show corvus.service -p ExecMainStatus --value")
            .stdout.decode()
            .strip()
            == "1"
        )
        assert "required migration 1 -> 2 is unavailable" in self._logs()
        assert self._sql("SELECT version FROM schema_version") == "1"
        assert self._sql("SELECT * FROM drive") == before


class TestDatabaseMigrationsPostgresql(DatabaseMigrationCase):
    BACKEND = "postgresql"


class TestDatabaseMigrationsSqlite(DatabaseMigrationCase):
    BACKEND = "sqlite"
