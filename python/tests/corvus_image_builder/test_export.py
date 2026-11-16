"""Exercise rescue export ordering, geometry, and failure cleanup without loop devices."""

import subprocess
from collections.abc import Sequence
from pathlib import Path
from unittest.mock import Mock

import pytest
from corvus_image_builder.gentoo.export import export_rescue_image


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
    ) -> subprocess.CompletedProcess[bytes]:
        commands.append(tuple(argv))
        if argv[0] == "sha256sum" and bad_checksum:
            raise subprocess.CalledProcessError(1, argv)
        if argv[0] == "cp":
            Path(argv[-1]).write_bytes(source.read_bytes())
        if argv[0] == "tar":
            Path(argv[argv.index("-cf") + 1]).write_bytes(b"update")
        return subprocess.CompletedProcess(argv, 0)

    monkeypatch.setattr("os.geteuid", lambda: 0)
    lookup = Mock(side_effect=read)
    monkeypatch.setattr(subprocess, "check_output", lookup)
    monkeypatch.setattr(subprocess, "run", run)
    if bad_checksum:
        with pytest.raises(subprocess.CalledProcessError):
            export_rescue_image(source, output)
        assert not (output / "gentoo-rescue.img").exists()
        assert commands[-1] == ("losetup", "-d", "/dev/loop1")
    else:
        export_rescue_image(source, output)
        assert (output / "gentoo-rescue.img").read_bytes() == b"original image"
        assert (output / "gentoo-rescue-update.tar").read_bytes() == b"update"
        partitioning = next(command for command in commands if command[0] == "sgdisk")
        assert "--partition-guid=3:partition-guid" in partitioning
        assert partitioning[-1] == "/dev/loop2"
        assert ("resize2fs", "/dev/loop2p3", "26384") in commands
    assert source.read_bytes() == b"original image"
    assert "--read-only" in lookup.call_args_list[0].args[0]
    assert not list(output.glob(".export-*"))
