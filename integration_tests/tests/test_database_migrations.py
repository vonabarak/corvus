"""Start the real daemon against frozen historical schemas on both backends."""

from __future__ import annotations

import shlex
import subprocess
import time
from collections.abc import Iterator
from pathlib import Path

import pytest
from corvus_client import Client
from corvus_test_harness.cases import SingleNodeCase
from corvus_test_harness.inner import open_client
from corvus_test_harness.postgres import psql, wait_for_postgres
from corvus_test_harness.sqlite import SqliteDatabase
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

    _database: SqliteDatabase | None = None

    @pytest.fixture(autouse=True)
    def _sqlite_access(self) -> Iterator[None]:
        if self.BACKEND == "sqlite":
            with SqliteDatabase(self.node, SQLITE_PATH) as database:
                self._database = database
                try:
                    yield
                finally:
                    self._database = None
        else:
            yield

    def _postgres_sql(self, sql: str) -> str:
        # Objects must belong to the daemon's role for ALTER TABLE.
        return psql(self.shell, "SET ROLE corvus; " + sql, db=DATABASE).removeprefix(
            "SET\n"
        )

    def _query(self, sql: str) -> str:
        if self.BACKEND == "postgresql":
            return self._postgres_sql(sql)
        assert self._database is not None
        rows = self._database.query(sql)
        return "\n".join(
            "|".join("" if value is None else str(value) for value in row)
            for row in rows
        )

    def _execute(self, sql: str) -> None:
        if self.BACKEND == "postgresql":
            self._postgres_sql(sql)
        else:
            assert self._database is not None
            self._database.execute(sql)

    def _execute_script(self, sql: str, *, create: bool = False) -> None:
        if self.BACKEND == "postgresql":
            self._postgres_sql(sql)
        else:
            assert self._database is not None
            self._database.execute_script(sql, create=create)

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
            self._execute_script(
                (FIXTURES / f"{self.BACKEND}-v2.sql").read_text(), create=True
            )
            self._execute_script((FIXTURES / "seed.sql").read_text())
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

    def _connect(self) -> Client:
        return open_client(
            self.node.relay,
            cert_dir=self.node._client_cert_dir,
            tls=True,
            ensure_self_node=False,
            boot_timeout_sec=60,
        )

    def test_01_upgrade_version_2_and_restart(self) -> None:
        self._prepare(historical=True)
        assert self._query("SELECT version FROM schema_version") == "2"
        insert = (
            "INSERT INTO drive (vm_id, disk_image_id, interface, media, cache_type) "
            "VALUES (1, NULL, 'ide', 'cdrom', 'none')"
        )
        # Establish that this really is the old schema before starting Corvus.
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(insert)
        self._start()
        with self._connect() as client:
            assert client.status().database_backend == self.BACKEND
            assert client.vms.get("migration-vm").show().name == "migration-vm"
        assert "migrated from version 2 to 12" in self._logs()
        assert self._query("SELECT version FROM schema_version") == "12"
        assert (
            self._query("SELECT graphics_adapter FROM vm WHERE id = 1") == "virtio-vga"
        )
        assert (
            self._query(
                "SELECT graphics_adapter FROM template_vm WHERE name = 'migration-template'"
            )
            == "virtio-vga"
        )
        assert self._query("SELECT id, disk_image_id FROM drive") == "1|1"
        assert (
            self._query("SELECT tag FROM disk_image_tag WHERE disk_image_id = 1")
            == "latest"
        )
        self._execute(
            "INSERT INTO audio_device (vm_id, backend, options) "
            "VALUES (1, 'pulse', 'server=192.0.2.10,out.name=speakers,in.name=mic')"
        )
        assert self._query("SELECT COUNT(*) FROM audio_device WHERE vm_id = 1") == "1"
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(
                "INSERT INTO audio_device (vm_id, backend, options) VALUES (999, 'spice', '')"
            )
        self._execute_script(insert + ";" + insert)
        assert (
            self._query("SELECT COUNT(*) FROM drive WHERE disk_image_id IS NULL") == "2"
        )
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(insert.replace("NULL", "1"))
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(insert.replace("NULL", "999"))
        index_query = (
            "SELECT indexname FROM pg_indexes WHERE tablename = 'drive'"
            if self.BACKEND == "postgresql"
            else "SELECT name FROM sqlite_master WHERE type = 'index' AND tbl_name = 'drive'"
        )
        assert "migration_drive_media" in self._query(index_query)
        assert self._query("SELECT COUNT(*) FROM disk_image_build_identity") == "0"
        identity = (
            "INSERT INTO disk_image_build_identity (disk_image_id, fingerprint, inputs) "
            "VALUES (1, 'abc', '{}')"
        )
        self._execute(identity)
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(identity)
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(identity.replace("(1,", "(999,"))
        upload_identity = (
            "INSERT INTO disk_image_upload_identity (disk_image_id, digest, source_path) "
            "VALUES (1, 'sha256-digest', NULL)"
        )
        self._execute(upload_identity)
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(upload_identity)
        with pytest.raises(subprocess.CalledProcessError):
            self._execute(upload_identity.replace("(1,", "(999,"))
        self._stop()
        self._start()
        with self._connect() as client:
            assert client.status().database_backend == self.BACKEND
        assert (
            self._query("SELECT fingerprint, inputs FROM disk_image_build_identity")
            == "abc|{}"
        )
        assert (
            self._query("SELECT digest FROM disk_image_upload_identity")
            == "sha256-digest"
        )
        assert "is current; skipping migrations" in self._logs()
        assert self._query("SELECT COUNT(*) FROM drive") == "3"

    def test_02_fresh_creation(self) -> None:
        self._prepare(historical=False)
        self._start()
        with self._connect() as client:
            assert client.status().database_backend == self.BACKEND
            assert client.vms.list() == []
        assert self._query("SELECT COUNT(*) FROM disk_image_build_identity") == "0"
        assert self._query("SELECT COUNT(*) FROM disk_image_upload_identity") == "0"
        assert "Created database schema at version 12" in self._logs()
        assert "migrated from version" not in self._logs()
        assert self._query("SELECT version FROM schema_version") == "12"

    def test_03_retired_migration_refuses_startup(self) -> None:
        self._prepare(historical=True)
        self._execute("UPDATE schema_version SET version = 1")
        before = self._query("SELECT * FROM drive")
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
        assert self._query("SELECT version FROM schema_version") == "1"
        assert self._query("SELECT * FROM drive") == before


class TestDatabaseMigrationsPostgresql(DatabaseMigrationCase):
    BACKEND = "postgresql"


class TestDatabaseMigrationsSqlite(DatabaseMigrationCase):
    BACKEND = "sqlite"

    def test_sqlite_access_modes_parameters_and_rollback(self) -> None:
        """Exercise the harness worker against an isolated node-side database."""
        directory = (
            self.node.run("mktemp -d /tmp/corvus-it-database.XXXXXX")
            .stdout.decode()
            .strip()
        )
        path = f"{directory}/database with spaces?#.db"
        try:
            with SqliteDatabase(self.node, path) as database:
                for operation in (
                    lambda: database.query("SELECT 1"),
                    lambda: database.execute("CREATE TABLE accidental (id INTEGER)"),
                    lambda: database.execute_script(
                        "CREATE TABLE accidental (id INTEGER)"
                    ),
                ):
                    with pytest.raises(subprocess.CalledProcessError):
                        operation()
                    assert (
                        self.node.run(
                            shlex.join(["test", "!", "-e", path]), check=False
                        ).returncode
                        == 0
                    )
                database.execute_script(
                    "CREATE TABLE parent (id INTEGER PRIMARY KEY);"
                    "CREATE TABLE sample ("
                    "id INTEGER PRIMARY KEY, label TEXT, weight REAL, optional TEXT,"
                    "parent_id INTEGER REFERENCES parent(id));"
                    "INSERT INTO parent VALUES (1);"
                    "INSERT INTO sample VALUES (1, 'a;b', 1.5, NULL, 1);",
                    create=True,
                )
                label = "https://example.test/образ?name='quoted'&release=2;next"
                database.execute(
                    "INSERT INTO sample VALUES (?, ?, ?, ?, ?)",
                    (2, label, 2.5, None, 1),
                )
                assert database.query(
                    "SELECT id, label, weight, optional FROM sample WHERE label = ?",
                    (label,),
                ) == [[2, label, 2.5, None]]
                assert database.query("SELECT label FROM sample WHERE id = 1") == [
                    ["a;b"]
                ]
                with pytest.raises(subprocess.CalledProcessError) as failure:
                    database.query("DELETE FROM sample")
                assert b"readonly" in failure.value.stderr
                with pytest.raises(subprocess.CalledProcessError) as failure:
                    database.execute(
                        "INSERT INTO sample (id, parent_id) VALUES (3, 999)"
                    )
                assert b"FOREIGN KEY" in failure.value.stderr
                with pytest.raises(subprocess.CalledProcessError):
                    database.execute_script(
                        "INSERT INTO sample (id, label, parent_id) VALUES (3, 'rolled;back', 1);"
                        "INSERT INTO sample (id, parent_id) VALUES (4, 999);"
                    )
                assert database.query("SELECT id FROM sample ORDER BY id") == [[1], [2]]
                assert database.query("SELECT * FROM sample WHERE id = ?", (999,)) == []
        finally:
            self.node.run(shlex.join(["rm", "-rf", directory]), check=False)
