#!/usr/bin/env python3
"""Local crv/curl stub for testing image Makefiles without publishing images."""

from __future__ import annotations

import json
import os
import sys
from pathlib import Path

FILENAME = "gentoo-cloud-20261008.qcow2"
SHA256 = "a" * 64


# Multi-file manifests include an unrelated entry to catch accidental first-row use.
SHA512 = "a" * 128
ALPINE_FILE = "nocloud_alpine-3.21.6-x86_64-bios-cloudinit-r0.qcow2"


def download_response(url: str) -> tuple[str, str]:
    if url.endswith("latest-di-amd64-cloudinit.txt"):
        return "manifest", f"# signed manifest\n{FILENAME} 1024"
    if "autobuilds/" in url:
        return "checksum", f"{'b' * 64} unrelated.qcow2\n{SHA256} {FILENAME}"
    if "cloud.debian.org" in url:
        return (
            "debian",
            f"{'b' * 128} unrelated.qcow2\n{SHA512} debian-12-generic-amd64.qcow2",
        )
    if "cloud-images.ubuntu.com" in url:
        return (
            "ubuntu",
            f"{'b' * 64} *unrelated.img\n{SHA256} *ubuntu-26.04-server-cloudimg-amd64.img",
        )
    if "repo.almalinux.org" in url:
        return (
            "almalinux",
            f"{'b' * 64} unrelated.qcow2\n{SHA256} AlmaLinux-10-GenericCloud-latest.x86_64_v2.qcow2",
        )
    if "download.freebsd.org" in url:
        return (
            "freebsd",
            f"SHA512 (unrelated.xz) = {'b' * 128}\nSHA512 (FreeBSD-14.4-RELEASE-amd64-BASIC-CLOUDINIT-ufs.qcow2.xz) = {SHA512}",
        )
    if url.endswith("/cloud/"):
        older = ALPINE_FILE.replace("3.21.6", "3.21.5")
        return (
            "alpine-index",
            f'<a href="{older}">{older}</a>\n<a href="{ALPINE_FILE}">{ALPINE_FILE}</a>',
        )
    if url.endswith(".sha512"):
        # Alpine's per-image sidecar contains only the digest, without a filename.
        return "alpine", SHA512
    raise ValueError(f"Unexpected download: {url}")


def main() -> int:
    command = Path(sys.argv[0]).name
    args = sys.argv[1:]
    with Path(os.environ["RECIPE_CALL_LOG"]).open("a") as stream:
        stream.write(
            json.dumps({"command": command, "args": args, "cwd": str(Path.cwd())})
            + "\n"
        )
    if command == "curl":
        resource, response = download_response(args[-1])
        if os.environ.get("DOWNLOAD_ERROR") == resource:
            return 22
        if os.environ.get("INVALID_DOWNLOAD") == resource:
            response = "invalid upstream response"
        if os.environ.get("INVALID_DIGEST") == resource:
            response = response.replace("a", "g")
        print(response)
        return 0
    if command == "mkisofs":
        output = Path(args[args.index("-o") + 1])
        source = Path(args[-1])
        output.write_bytes(
            b"iso:"
            + b"".join(
                path.read_bytes()
                for path in sorted(source.rglob("*"))
                if path.is_file()
            )
        )
        return 0
    if command == "cpio":
        sys.stdin.buffer.read()
        sys.stdout.buffer.write(b"initrd")
        return 0
    if args[:2] == ["disk", "upload"]:
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
