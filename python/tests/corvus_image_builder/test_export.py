"""Exercise rescue export ordering, geometry, and failure cleanup without loop devices."""

import json
import runpy
import subprocess
import sys
from collections.abc import Sequence
from pathlib import Path
from unittest.mock import Mock

import pytest

EXPORT = runpy.run_path(
    str(Path(__file__).resolve().parents[3] / "yaml/gentoo/rescue/export.py")
)


@pytest.mark.parametrize("bad_checksum", [False, True])
def test_rescue_export_uses_private_copy_and_cleans_up(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch, bad_checksum: bool
) -> None:
    source = tmp_path / "source.raw"
    source.write_bytes(b"original image")
    output = tmp_path / "output"
    commands: list[tuple[str, ...]] = []
    geometry = iter([100000, 10000, 10000])
    loops = iter(["/dev/loop1", "/dev/loop2"])

    def read(argv: Sequence[str], **kwargs: object) -> str:
        if argv[0] == "losetup":
            return next(loops)
        if argv[0] == "blkid":
            return "rescue-data" if "LABEL" in argv else "partition-guid"
        if argv[0] == "dumpe2fs":
            blocks = next(geometry)
            return (
                f"Block count: {blocks}\n"
                f"Block size: 4096\n"
            )
        if argv[0] == "lsblk":
            return "1054720"
        if argv[0] == "blockdev":
            return "8388608"
        raise AssertionError(argv)

    def run(
        argv: Sequence[str], **kwargs: object
    ) -> subprocess.CompletedProcess[str | bytes]:
        if argv[0] == "test-crv":
            if list(argv[1:]) == ["node", "list", "-o", "json"]:
                return subprocess.CompletedProcess(
                    argv,
                    0,
                    json.dumps(
                        [{"id": 1, "name": "local"}, {"id": 2, "name": "remote"}]
                    ),
                )
            assert list(argv[1:]) == [
                "disk",
                "show",
                "gentoo-rescue:latest",
                "-o",
                "json",
            ]
            placements = [
                {
                    "node": {"id": 2, "name": "remote"},
                    "file_path": str(output / "other.raw"),
                },
                {"node": {"id": 1, "name": "local"}, "file_path": str(source)},
            ]
            return subprocess.CompletedProcess(
                argv, 0, json.dumps({"placements": placements})
            )
        commands.append(tuple(argv))
        if argv[0] == "sha256sum" and bad_checksum:
            raise subprocess.CalledProcessError(1, argv)
        if argv[0] == "cp":
            Path(argv[-1]).write_bytes(source.read_bytes())
        if argv[0] == "tar":
            Path(argv[argv.index("-cf") + 1]).write_bytes(b"update")
        return subprocess.CompletedProcess(argv, 0)

    monkeypatch.setattr("os.geteuid", lambda: 0)
    monkeypatch.setenv("CRV", "test-crv")
    monkeypatch.setattr(sys, "argv", ["export.py", str(output)])
    lookup = Mock(side_effect=read)
    monkeypatch.setattr(subprocess, "check_output", lookup)
    monkeypatch.setattr(subprocess, "run", run)
    if bad_checksum:
        with pytest.raises(SystemExit, match="1"):
            EXPORT["main"]()
        assert not (output / "gentoo-rescue.img").exists()
        assert commands[-1] == ("losetup", "-d", "/dev/loop1")
    else:
        EXPORT["main"]()
        assert (output / "gentoo-rescue.img").read_bytes() == b"original image"
        assert (output / "gentoo-rescue-update.tar").read_bytes() == b"update"
        partitioning = next(command for command in commands if command[0] == "sgdisk")
        assert "--partition-guid=3:partition-guid" in partitioning
        assert partitioning[-1] == "/dev/loop2"
        assert ("resize2fs", "/dev/loop2p3", "26384") in commands
    assert source.read_bytes() == b"original image"
    assert "--read-only" in lookup.call_args_list[0].args[0]
    assert not list(output.glob(".export-*"))


def test_export_fails_without_registered_nodes(monkeypatch: pytest.MonkeyPatch) -> None:
    lookup = Mock(return_value=subprocess.CompletedProcess([], 0, "[]"))
    monkeypatch.setenv("CRV", "test-crv")
    monkeypatch.setattr(subprocess, "run", lookup)

    with pytest.raises(ValueError, match="No Corvus nodes are registered"):
        EXPORT["find_source"]()

    assert lookup.call_count == 1
    assert lookup.call_args.args[0] == ["test-crv", "node", "list", "-o", "json"]
