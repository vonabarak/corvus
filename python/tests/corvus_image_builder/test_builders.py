"""Verify builder bootstrap integrity and standalone recipe composition."""

import errno
import hashlib
import os
import runpy
import shutil
import subprocess
import sys
from pathlib import Path
from unittest.mock import Mock

import pytest
from corvus_image_builder.core import BuildContext
from corvus_image_builder.gentoo import Settings
from corvus_image_builder.gentoo.builders import (
    BuilderFinalizeStage,
    BuilderIdentityStage,
    MuslBuilderToolsStage,
    Stage3Stage,
)

ROOT = Path(__file__).resolve().parents[3]
DIRECTORY = ROOT / "yaml/gentoo"
SCRIPTS = sorted(DIRECTORY.glob("*/build*.py"))


def test_builder_finalize_replaces_stale_repository_in_retained_directory(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    source = tmp_path / "source"
    (source / "gentoo").mkdir(parents=True)
    (source / "gentoo/Manifest").write_text("current repository")
    (source / "selected").symlink_to("gentoo", target_is_directory=True)
    root = tmp_path / "root"
    destination = root / "var/db/repos"
    destination.mkdir(parents=True)
    (destination / "stale").write_text("obsolete repository")
    inode = destination.stat().st_ino
    copytree = shutil.copytree

    def copy_repository(*args: object, **kwargs: object) -> None:
        assert args == ("/var/db/repos", destination)
        assert kwargs == {"symlinks": True, "dirs_exist_ok": True}
        # copytree recurses through its module global.
        monkeypatch.setattr(shutil, "copytree", copytree)
        copytree(source, destination, symlinks=True, dirs_exist_ok=True)

    monkeypatch.setattr(shutil, "copytree", copy_repository)
    stage = BuilderFinalizeStage(Settings(Path("/dev/vdb"), tmp_path / "cache", None))
    stage.clean(BuildContext(root))
    assert destination.stat().st_ino == inode
    assert not (destination / "stale").exists()
    assert (destination / "gentoo/Manifest").read_text() == "current repository"
    assert (destination / "selected").readlink() == Path("gentoo")


def test_musl_bootstrap_disables_unprivileged_fetching(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    root = tmp_path / "root"
    portage = root / "etc/portage"
    portage.mkdir(parents=True)
    (portage / "make.conf").write_text('FEATURES="userfetch userpriv"\n')
    context = BuildContext(root)
    stage = MuslBuilderToolsStage(
        Settings(Path("/dev/vdb"), tmp_path / "cache", None, libc="musl", init="openrc")
    )

    def check_policy(*args: object, **kwargs: object) -> None:
        result = subprocess.run(
            [
                "bash",
                "-c",
                'source "$1"; printf "%s" "$FEATURES"',
                "bash",
                str(portage / "make.conf"),
            ],
            capture_output=True,
            text=True,
            check=True,
        )
        assert result.stdout.split() == ["-userfetch", "-userpriv"]

    chroot = Mock(side_effect=check_policy)
    monkeypatch.setattr(stage, "chroot", chroot)
    stage.execute(context)
    assert chroot.call_count == 2
    assert context.artifacts["system"] == root


def test_builder_cleanup_preserves_underlying_cache_mountpoints(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    root = tmp_path / "view"
    caches = [root / "var/cache" / name for name in ("binpkgs", "binhost", "distfiles")]
    repository = root / "var/db/repos/gentoo"
    repository.mkdir(parents=True)
    (repository / "Manifest").write_text("retain repository")
    shared = tmp_path / "shared-cache"
    shared.mkdir()
    (shared / "package.gpkg.tar").write_text("retain shared package")
    for cache in caches:
        (cache / "stale").mkdir(parents=True)
        (cache / "stale/package").write_text("remove underlying residue")
        (cache / "shared-link").symlink_to(shared, target_is_directory=True)
    inodes = [cache.stat().st_ino for cache in caches]
    rmtree = shutil.rmtree

    def reject_mountpoint_removal(path: str | Path) -> None:
        if Path(path) in caches:
            raise OSError(errno.EBUSY, "Device or resource busy", str(path))
        rmtree(path)

    monkeypatch.setattr(shutil, "rmtree", reject_mountpoint_removal)
    stage = BuilderIdentityStage(Settings(Path("/dev/vdb"), shared, None))
    stage.clean(BuildContext(root))
    assert [cache.stat().st_ino for cache in caches] == inodes
    assert all(not list(cache.iterdir()) for cache in caches)
    assert (shared / "package.gpkg.tar").read_text() == "retain shared package"
    assert (repository / "Manifest").read_text() == "retain repository"


@pytest.mark.parametrize("script", SCRIPTS, ids=lambda p: str(p.relative_to(DIRECTORY)))
def test_standalone_recipe_plan(script: Path, tmp_path: Path) -> None:
    bundle = runpy.run_path(str(DIRECTORY / "bundle.py"))["bundle"]
    archive = tmp_path / "build.pyz"
    bundle(script, archive)
    result = subprocess.run(
        [sys.executable, "-I", str(archive)],
        cwd=tmp_path,
        env={**os.environ, "IMAGE_BUILD_PLAN": "1"},
        capture_output=True,
        text=True,
        check=True,
    )
    stages = result.stdout.splitlines()
    assert "Workspace" in stages[0]
    assert any("Repository" in stage for stage in stages)
    assert len(stages) >= 4


def test_stage3_uses_verified_cache_without_download(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    cache = tmp_path / "cache"
    (cache / "distfiles").mkdir(parents=True)
    content = b"verified-stage3"
    digest = hashlib.sha256(content).hexdigest()
    archive = cache / "distfiles" / f"corvus-musl-{digest}.tar.xz"
    archive.write_bytes(content)
    root = tmp_path / "root"
    (root / "etc").mkdir(parents=True)
    monkeypatch.setenv("MUSL_STAGE3_SHA256", digest)
    monkeypatch.setenv(
        "MUSL_STAGE3_URL", "https://distfiles.gentoo.org/releases/amd64/test.tar.xz"
    )
    download = Mock(
        side_effect=AssertionError("cached archive should not be downloaded")
    )
    monkeypatch.setattr("urllib.request.urlopen", download)
    stage = Stage3Stage(
        Settings(Path("/dev/vdb"), cache, None, libc="musl", init="openrc")
    )
    command = Mock()
    monkeypatch.setattr(stage, "command", command)
    context = BuildContext(root)
    stage.validate(context)
    stage.execute(context)
    assert context.artifacts["stage3"] == archive
    command.assert_called_once_with(
        context,
        "tar",
        "--xattrs",
        "--numeric-owner",
        "-xpf",
        str(archive),
        "-C",
        str(root),
    )
    assert (root / "etc/resolv.conf").is_file()


def test_stage3_rejects_corrupt_download(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    import io

    cache = tmp_path / "cache"
    (cache / "distfiles").mkdir(parents=True)
    monkeypatch.setenv("MUSL_STAGE3_SHA256", "0" * 64)
    monkeypatch.setenv(
        "MUSL_STAGE3_URL", "https://distfiles.gentoo.org/releases/amd64/test.tar.xz"
    )
    monkeypatch.setattr(
        "urllib.request.urlopen", lambda *a, **k: io.BytesIO(b"corrupt")
    )
    stage = Stage3Stage(
        Settings(Path("/dev/vdb"), cache, None, libc="musl", init="openrc")
    )
    command = Mock()
    monkeypatch.setattr(stage, "command", command)
    with pytest.raises(ValueError, match="checksum mismatch"):
        stage.execute(BuildContext(tmp_path / "root"))
    command.assert_not_called()
    assert not list((cache / "distfiles").iterdir())
