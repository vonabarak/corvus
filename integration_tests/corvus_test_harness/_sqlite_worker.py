"""Standalone SQLite worker copied to test nodes; standard-library only."""

from __future__ import annotations

import json
import sqlite3
import sys
from contextlib import closing
from pathlib import Path
from typing import Literal, TypedDict, cast

SqlValue = str | int | float | None


class SqliteRequest(TypedDict):
    path: str
    operation: Literal["query", "execute", "script"]
    sql: str
    parameters: list[SqlValue]
    create: bool


def run_request(request: SqliteRequest) -> list[list[SqlValue]]:
    """Use SQLite URI modes to make read-only access and creation explicit."""
    operation = request["operation"]
    mode = "ro" if operation == "query" else "rw"
    if operation == "script" and request["create"]:
        mode = "rwc"
    uri = Path(request["path"]).resolve().as_uri() + "?mode=" + mode
    with closing(sqlite3.connect(uri, uri=True)) as db, db:
        db.execute("PRAGMA foreign_keys = ON")
        if operation == "script":
            # executescript does not open a transaction itself. The explicit
            # transaction ensures a failing statement rolls back the whole script.
            db.executescript("BEGIN;\n" + request["sql"] + "\n;COMMIT;")
            return []
        cursor = db.execute(request["sql"], request["parameters"])
        if operation == "execute":
            return []
        rows: list[list[SqlValue]] = []
        for row in cursor:
            values: list[SqlValue] = []
            for value in row:
                if value is not None and not isinstance(value, (str, int, float)):
                    raise TypeError("SQLite harness queries do not support BLOB values")
                values.append(value)
            rows.append(values)
        return rows


def main() -> None:
    request = cast(SqliteRequest, json.loads(Path(sys.argv[1]).read_text()))
    print(json.dumps(run_request(request)))


if __name__ == "__main__":
    main()
