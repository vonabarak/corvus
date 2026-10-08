#!/usr/bin/env python3
"""Local crv/curl stub for testing Gentoo Makefiles without publishing images."""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path

FILENAME = "gentoo-cloud-20261008.qcow2"
SHA256 = "a" * 64


def main() -> int:
    command = Path(sys.argv[0]).name
    args = sys.argv[1:]
    with Path(os.environ["RECIPE_CALL_LOG"]).open("a") as stream:
        stream.write(
            json.dumps({"command": command, "args": args, "cwd": str(Path.cwd())})
            + "\n"
        )
    if command == "curl":
        manifest = args[-1].endswith("latest-di-amd64-cloudinit.txt")
        resource = "manifest" if manifest else "checksum"
        if os.environ.get("DOWNLOAD_ERROR") == resource:
            return 22
        if os.environ.get("INVALID_DOWNLOAD") == resource:
            print("invalid upstream response")
        elif manifest:
            print(f"# signed manifest\n{FILENAME} 1024")
        else:
            # A checksum for another filename must never be selected.
            print(f"{'b' * 64} unrelated.qcow2\n{SHA256} {FILENAME}")
        return 0
    if args[0] == "build":
        return 1 if os.environ.get("FAIL_BUILD") == args[1] else 0
    if os.environ.get("LOOKUP_ERROR") == "1":
        print(json.dumps({"code": "permission_denied"}))
        return 1
    if f"{args[2]}:{args[4]}" in os.environ.get("MISSING_OUTPUTS", "").split(","):
        print(json.dumps({"code": f"{args[2]}_not_found"}))
        return 1
    print(json.dumps({"id": 42}))
    return 0


if __name__ == "__main__":
    sys.exit(main())
