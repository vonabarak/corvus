"""Explicit read-only queries and transactional writes to a test-node database."""

from __future__ import annotations

import json
import shlex
from collections.abc import Sequence
from pathlib import Path
from types import TracebackType
from typing import Literal, cast

from ._sqlite_worker import SqliteRequest, SqlValue
from .runner import NodeShellRunner
from .ssh import HOST_ALPINE_KEY_PATH, NodeShell
from .topology import TestNode


class SqliteDatabase:
    """Access SQLite on the node, including committed WAL data.

    Use as a context manager. Each context owns a temporary worker directory;
    the database itself is never copied, removed, or created implicitly.
    """

    def __init__(self, node: TestNode, path: str = "/var/lib/corvus/corvus.db") -> None:
        self.node = node
        self.path = path
        self._directory: str | None = None
        self._runner = NodeShellRunner(
            NodeShell(cid=node.cid, user="corvus", key_path=HOST_ALPINE_KEY_PATH)
        )

    def __enter__(self) -> SqliteDatabase:
        if self._directory is not None:
            raise RuntimeError("SQLite context is already open")
        self._directory = (
            self.node.run("mktemp -d /tmp/corvus-it-sqlite.XXXXXX")
            .stdout.decode()
            .strip()
        )
        try:
            self._runner.copy_bytes(
                Path(__file__).with_name("_sqlite_worker.py").read_bytes(),
                f"{self._directory}/worker.py",
                mode=0o600,
            )
        except BaseException:
            self._close()
            raise
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
    ) -> None:
        self._close()

    def _close(self) -> None:
        if self._directory is not None:
            self.node.run(shlex.join(["rm", "-rf", self._directory]), check=False)
            self._directory = None

    def query(
        self, sql: str, parameters: Sequence[SqlValue] = ()
    ) -> list[list[SqlValue]]:
        """Return rows with JSON scalar types, using a read-only connection."""
        return self._request("query", sql, parameters)

    def execute(self, sql: str, parameters: Sequence[SqlValue] = ()) -> None:
        """Execute and commit one statement against an existing database."""
        self._request("execute", sql, parameters)

    def execute_script(self, sql: str, *, create: bool = False) -> None:
        """Execute a transaction of SQL statements; opt in to database creation.

        Scripts must not include their own transaction-control statements.
        """
        self._request("script", sql, create=create)

    def _request(
        self,
        operation: Literal["query", "execute", "script"],
        sql: str,
        parameters: Sequence[SqlValue] = (),
        *,
        create: bool = False,
    ) -> list[list[SqlValue]]:
        if self._directory is None:
            raise RuntimeError("SQLite access requires an open context")
        request: SqliteRequest = {
            "path": self.path,
            "operation": operation,
            "sql": sql,
            "parameters": list(parameters),
            "create": create,
        }
        request_path = f"{self._directory}/request.json"
        self._runner.copy_bytes(json.dumps(request).encode(), request_path, mode=0o600)
        result = self.node.run(
            shlex.join(["python3", f"{self._directory}/worker.py", request_path])
        )
        return cast(list[list[SqlValue]], json.loads(result.stdout))
