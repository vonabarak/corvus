"""Regression checks for overlay safety and the immutable guest contract."""

import os
import runpy
import subprocess
import sys
from pathlib import Path
from unittest.mock import Mock, patch

import pytest
from corvus_image_builder.bundle import bundle
from corvus_image_builder.core import BuildContext, BuildManager, CommandRunner
from corvus_image_builder.gentoo import (
    BootloaderStage,
    ChrootEnvironmentStage,
    DiskStage,
    ExistingImageStage,
    FinalizeStage,
    Settings,
    WorkspaceStage,
    remove,
)
from corvus_image_builder.gentoo.mounts import Mounts
from corvus_image_builder.portage import InstallRequest, PortageBackend

import yaml

ROOT = Path(__file__).resolve().parents[3]
DIRECTORY = ROOT / "yaml/gentoo"


@pytest.mark.parametrize("failed", [False, True])
def test_workspace_normalizes_restrictive_umask_and_restores_it(
    tmp_path: Path, failed: bool
) -> None:
    original = os.umask(0o077)
    context = BuildContext(tmp_path / "image", Mock(spec=CommandRunner))
    scratch = tmp_path / "scratch"
    scratch.mkdir()
    try:
        with (
            patch("shutil.which", return_value=None),
            patch("tempfile.mkdtemp", return_value=str(scratch)),
        ):
            WorkspaceStage(
                Settings(Path("/dev/vdb"), tmp_path / "cache", None)
            ).execute(context)
        runtime = context.root / "usr/bin"
        runtime.mkdir(parents=True)
        assert runtime.stat().st_mode & 0o777 == 0o755
        context.close(failed=failed)
        assert os.umask(0o077) == 0o077
    finally:
        os.umask(original)


@pytest.mark.parametrize("flavor", ["corvus-test-node", "corvus-test-vm"])
def test_standalone_bundle_and_recipe_contract(flavor: str, tmp_path: Path) -> None:
    first, second = tmp_path / "first.pyz", tmp_path / "second.pyz"
    bundle(DIRECTORY / flavor / "build.py", first)
    bundle(DIRECTORY / flavor / "build.py", second)
    assert first.read_bytes() == second.read_bytes()
    result = subprocess.run(
        [sys.executable, "-I", str(first)],
        cwd=tmp_path,
        env={**os.environ, "IMAGE_BUILD_PLAN": "1"},
        capture_output=True,
        text=True,
        check=True,
    )
    recipe = yaml.safe_load((DIRECTORY / flavor / f"{flavor}.yml").read_text())
    build = recipe["pipeline"][1]["build"]
    assert build["name"] == flavor
    assert not any(
        ".sh" in p.get("file", {}).get("from", "") for p in build["provisioners"]
    )
    assert build["provisioners"][-1]["shell"]["inline"] == f"python3 /run/{flavor}.pyz"
    stages = result.stdout.splitlines()
    assert "ChrootEnvironmentStage" in stages
    if flavor.endswith("-node"):
        assert "ExistingImageStage" in stages and "DiskStage" not in stages
        assert "KernelStage" not in stages and "BootloaderStage" not in stages
        assert build["sourceImage"] == "gentoo-glibc-systemd-headless"
        assert build["strategy"] == "offline-overlay"
    else:
        assert "DiskStage" in stages and "KernelStage" in stages
        assert build["strategy"] == "from-scratch" and build["target"]["size"] == "1G"


def test_overlay_validates_and_mounts_without_destructive_commands(
    tmp_path: Path,
) -> None:
    root = tmp_path / "image"
    for relative in ("boot/efi/EFI/BOOT", "lib/modules/test"):
        (root / relative).mkdir(parents=True)
    for relative in (
        "boot/vmlinuz-test",
        "boot/config-test",
        "boot/grub/grub.cfg",
        "boot/efi/EFI/BOOT/BOOTX64.EFI",
    ):
        path = root / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("inherited")
    (root / "boot/vmlinuz").symlink_to("vmlinuz-test")
    runner = Mock(spec=CommandRunner)
    context = BuildContext(root, runner)
    stage = ExistingImageStage(Settings(Path("/dev/nvme0n1"), tmp_path, None))
    with patch("subprocess.check_output", side_effect=["vfat\n", "ext4\n"]):
        stage.validate(context)
    stage.execute(context)
    stage.verify(context)
    context.close(failed=False)
    commands = [call.args[0] for call in runner.run.call_args_list]
    assert commands == [
        ("mount", "/dev/nvme0n1p3", str(root)),
        ("mount", "/dev/nvme0n1p2", str(root / "boot/efi")),
        ("umount", str(root / "boot/efi")),
        ("umount", str(root)),
    ]
    assert (root / "boot/grub/grub.cfg").read_text() == "inherited"


def test_overlay_rejects_wrong_filesystem_before_mounting(tmp_path: Path) -> None:
    runner = Mock(spec=CommandRunner)
    context = BuildContext(tmp_path, runner)
    with (
        patch("subprocess.check_output", return_value="xfs\n"),
        pytest.raises(ValueError, match="vfat"),
    ):
        ExistingImageStage(Settings(Path("/dev/vdb"), tmp_path, None)).validate(context)
    runner.run.assert_not_called()


def test_private_chroot_unmounts_before_cleanup(tmp_path: Path) -> None:
    runner = Mock(spec=CommandRunner)
    context = BuildContext(tmp_path, runner, artifacts={"kernel": "vmlinuz-test"})
    settings = Settings(Path("/dev/vdb"), tmp_path / "cache", None)
    ChrootEnvironmentStage(settings).execute(context)
    mounts = context.artifacts["chroot-mounts"]
    assert isinstance(mounts, Mounts)
    assert mounts.paths[-1] == tmp_path / "var/cache/binhost"
    with patch.object(FinalizeStage, "clean") as clean:
        clean.side_effect = lambda ctx: (
            pytest.fail("mounts still active") if mounts.paths else None
        )
        FinalizeStage(settings).execute(context)
    commands = [call.args[0] for call in runner.run.call_args_list]
    assert ("mount", "-t", "tmpfs", "tmpfs", str(tmp_path / "run")) in commands
    assert ("mount", "--bind", "/run", str(tmp_path / "run")) not in commands
    unmounts = [command[-1] for command in commands if command[0] == "umount"]
    assert unmounts[-1] == str(tmp_path / "dev")


@pytest.mark.parametrize("keep_directory", [False, True])
def test_cleanup_rejects_cache_mounts_and_symlink_ancestors(
    tmp_path: Path, keep_directory: bool
) -> None:
    cache = tmp_path / "image/var/cache/distfiles"
    cache.mkdir(parents=True)
    payload = cache / "shared.tar"
    payload.write_text("must survive")
    mountinfo = f"1 0 0:1 / {cache} rw - virtiofs shared rw\n"
    with (
        patch.object(Path, "read_text", return_value=mountinfo),
        pytest.raises(RuntimeError, match="mounted"),
    ):
        remove(tmp_path / "image", "var/cache", keep_directory=keep_directory)
    assert payload.read_text() == "must survive"
    (tmp_path / "image/alias").symlink_to(cache, target_is_directory=True)
    with pytest.raises(RuntimeError, match="Unsafe"):
        remove(tmp_path / "image", "alias/shared.tar", keep_directory=keep_directory)
    assert payload.exists()


def test_install_source_rebuild_options_and_helper_scratch(tmp_path: Path) -> None:
    runner = Mock(spec=CommandRunner)
    backend = PortageBackend(runner)
    with patch("importlib.import_module", return_value=Mock()):
        backend.install(
            InstallRequest(
                ("dev-python/pycapnp",),
                tmp_path,
                usepkg_exclude=("dev-python/pycapnp",),
                reinstall_atoms=("dev-python/pycapnp",),
                tmpdir=tmp_path / "helper-scratch",
            )
        )
    argv = runner.run.call_args.args[0]
    assert "--usepkg-exclude=dev-python/pycapnp" in argv
    assert "--reinstall-atoms=dev-python/pycapnp" in argv
    assert "--noreplace" not in argv
    assert runner.run.call_args.kwargs["env"]["PORTAGE_TMPDIR"] == str(
        tmp_path / "helper-scratch"
    )


def test_minimal_bootstrap_does_not_install_system_set(tmp_path: Path) -> None:
    script = runpy.run_path(str(DIRECTORY / "corvus-test-vm/build.py"))
    stage = script["TestVmPackages"](
        Settings(tmp_path, tmp_path, None, libc="musl", init="openrc")
    )
    with patch.object(stage, "packages") as packages:
        stage.install_system(BuildContext(tmp_path))
    atoms = packages.call_args.args[1]
    assert "sys-libs/musl" in atoms and "@system" not in atoms
    assert "sys-apps/portage" not in atoms and "sys-devel/gcc" not in atoms


@pytest.mark.parametrize("unexpected", [False, True])
def test_immutable_cleanup_verifies_before_stripping(
    tmp_path: Path, unexpected: bool
) -> None:
    script = runpy.run_path(str(DIRECTORY / "corvus-test-vm/build.py"))
    root = tmp_path / "image"
    for relative in (
        "var/db/pkg/pkg",
        "usr/share/locale/en_GB",
        "usr/share/locale/de",
        "usr/share/licenses/pkg",
        "usr/lib",
    ):
        (root / relative).mkdir(parents=True, exist_ok=True)
    (root / "usr/lib/libtest.so").write_text("runtime")
    (root / "usr/lib/libtest.a").write_text("build")
    stage = script["TestVmFinalize"](
        Settings(tmp_path, tmp_path, None, libc="musl", init="openrc")
    )
    with patch.object(
        PortageBackend,
        "installed",
        return_value=["sys-devel/gcc-1"] if unexpected else [],
    ):
        if unexpected:
            with pytest.raises(RuntimeError, match="Unexpected"):
                stage.clean(BuildContext(root))
            assert (root / "var/db/pkg/pkg").exists()
        else:
            stage.clean(BuildContext(root))
            assert not (root / "var/db/pkg").exists()
            assert (root / "usr/share/locale/en_GB").exists()
            assert not (root / "usr/share/locale/de").exists()
            assert (root / "usr/share/licenses/pkg").exists()
            assert (root / "usr/lib/libtest.so").exists()
            assert not (root / "usr/lib/libtest.a").exists()


@pytest.mark.parametrize("fail", [False, True])
def test_postgresql_stops_before_releasing_runtime(tmp_path: Path, fail: bool) -> None:
    script = runpy.run_path(str(DIRECTORY / "corvus-test-node/build.py"))
    runner = Mock(spec=CommandRunner)
    context = BuildContext(tmp_path, runner)
    stopped: list[object] = []

    def run(argv: tuple[str, ...], **kwargs: object) -> None:
        if "start" in argv:
            pid = tmp_path / "var/lib/postgresql/18/data/postmaster.pid"
            pid.parent.mkdir(parents=True)
            pid.write_text("123")
            assert "listen_addresses=''" in argv[-1]
        if "psql" in str(argv):
            assert "input" in kwargs
            if fail:
                raise subprocess.CalledProcessError(1, argv)
        if "stop" in argv:
            stopped.append(True)

    runner.run.side_effect = run
    context.own(lambda: stopped.append("unmounted"))
    stage = script["PostgreSQLInitializationStage"](Settings(tmp_path, tmp_path, None))
    if fail:
        with pytest.raises(subprocess.CalledProcessError):
            stage.execute(context)
    else:
        stage.execute(context)
    context.close(failed=fail)
    assert stopped == [True, "unmounted"]


def test_make_node_selects_python_base_and_forwards_policy() -> None:
    result = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "-n",
            "build",
            "MAKE=echo",
            "IMAGE=corvus-test-node",
            "IMAGE_POLICY=overwrite",
            "CACHE_DIR=/tmp/cache",
            "USEPKG=false",
            "GETBINPKG=false",
        ],
        cwd=DIRECTORY,
        capture_output=True,
        text=True,
        check=True,
    )
    assert "build IMAGE=glibc-systemd-headless IMAGE_POLICY=overwrite" in result.stdout
    publication = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "-n",
            "publish",
            "MAKE=echo",
            "IMAGE=corvus-test-node",
            "IMAGE_POLICY=overwrite",
            "GENTOO_CACHE_DIR=/tmp/cache",
            "USEPKG=false",
            "GETBINPKG=false",
        ],
        cwd=DIRECTORY,
        capture_output=True,
        text=True,
        check=True,
    )
    assert "../../integration_tests/keys build" in publication.stdout
    assert '--var cache_dir="/tmp/cache"' in publication.stdout
    assert '--var USEPKG="false" --var GETBINPKG="false"' in publication.stdout
    assert "../gentoo IMAGE=gentoo-glibc-systemd-headless" not in result.stdout


def test_vm_partition_and_kernel_argument_settings(tmp_path: Path) -> None:
    runner = Mock(spec=CommandRunner)
    context = BuildContext(
        tmp_path / "image", runner, artifacts={"kernel": "vmlinuz-test"}
    )
    settings = Settings(
        Path("/dev/vdb"),
        tmp_path,
        tmp_path / "config",
        esp_size_mib=64,
        kernel_args=("net.ifnames=0",),
    )
    with (
        patch("corvus_image_builder.gentoo.disk.mount_image"),
        patch.object(Path, "exists", return_value=True),
    ):
        DiskStage(settings).execute(context)
    BootloaderStage(settings).execute(context)
    assert any("2:0:+64M" in call.args[0] for call in runner.run.call_args_list)
    assert "net.ifnames=0" in (context.root / "boot/grub/grub.cfg").read_text()


def test_stage_order_rejects_missing_runtime_interfaces(tmp_path: Path) -> None:
    script = runpy.run_path(str(DIRECTORY / "corvus-test-vm/build.py"))
    recipe = script["build_recipe"](
        Settings(tmp_path, tmp_path, None, libc="musl", init="openrc")
    )
    recipe.stages = [
        stage
        for stage in recipe.stages
        if not isinstance(stage, ChrootEnvironmentStage)
    ]
    with pytest.raises(ValueError, match="interfaces"):
        BuildManager().plan(recipe, BuildContext(tmp_path))
