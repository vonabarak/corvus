import importlib.util
import os
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch

import pytest
from corvus_image_builder.core import BuildContext, BuildManager, CommandRunner
from corvus_image_builder.gentoo import Settings, headless
from corvus_image_builder.portage import InstallRequest, PortageBackend


def test_install_isolated_and_invalidates_queries() -> None:
    runner = Mock(spec=CommandRunner)
    backend = PortageBackend(runner)
    backend._trees = {"cached": True}
    portage = SimpleNamespace(dep=SimpleNamespace(Atom=Mock()))
    with (
        patch("importlib.import_module", return_value=portage),
        patch.dict(os.environ, {"SYSROOT": "/wrong", "FEATURES": "getbinpkg"}),
    ):
        backend.install(
            InstallRequest(("@system", "sys-libs/pam"), Path("/target"), False, False)
        )
    argv = runner.run.call_args.args[0]
    env = runner.run.call_args.kwargs["env"]
    assert "--usepkg=n" in argv and "--getbinpkg=n" in argv
    assert "SYSROOT" not in env and env["ROOT"] == "/target"
    assert env["FEATURES"].endswith("-getbinpkg")
    portage.dep.Atom.assert_called_once_with("sys-libs/pam")
    assert backend._trees is None


def test_empty_install_rejected() -> None:
    with (
        patch("importlib.import_module", return_value=Mock()),
        pytest.raises(ValueError),
    ):
        PortageBackend(Mock()).install(InstallRequest((), Path("/target")))


def test_plan_does_not_require_gentoo_or_touch_disk(tmp_path: Path) -> None:
    recipe = headless(Settings(Path("/nonexistent"), tmp_path, tmp_path / "config"))
    stages = BuildManager().plan(recipe, BuildContext(tmp_path / "root"))
    assert len(stages) == 8
    assert stages[0] == "WorkspaceStage" and stages[-1] == "FinalizeStage"
    assert not (tmp_path / "root").exists()


def test_bundle_deterministic(tmp_path: Path) -> None:
    spec = importlib.util.spec_from_file_location(
        "bundle", Path(__file__).parents[1] / "bundle.py"
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    first, second = tmp_path / "first.pyz", tmp_path / "second.pyz"
    module.bundle(first)
    module.bundle(second)
    assert first.read_bytes() == second.read_bytes()


def test_bootloader_uses_produced_kernel(tmp_path: Path) -> None:
    from corvus_image_builder.gentoo import BootloaderStage

    runner = Mock(spec=CommandRunner)
    root = tmp_path / "image"
    root.mkdir()
    context = BuildContext(root, runner=runner, artifacts={"kernel": "vmlinuz-test"})
    stage = BootloaderStage(Settings(Path("/dev/vdb"), tmp_path, tmp_path / "config"))
    stage.execute(context)
    stage.verify(context)
    commands = [call.args[0] for call in runner.run.call_args_list]
    assert "--target=i386-pc" in commands[0]
    assert "--target=x86_64-efi" in commands[1]
    assert "--no-nvram" in commands[1]
    assert "linux /boot/vmlinuz-test" in (root / "boot/grub/grub.cfg").read_text()
    assert (root / "boot/vmlinuz").readlink() == Path("vmlinuz-test")


def test_finalize_preserves_writable_temporary_directories(tmp_path: Path) -> None:
    from corvus_image_builder.gentoo import FinalizeStage

    root = tmp_path / "image"
    for relative in ("tmp", "var/tmp", "etc/ssh", "var/log"):
        (root / relative).mkdir(parents=True, exist_ok=True)
    (root / "tmp/build-residue").write_text("remove")
    host_key = root / "etc/ssh/ssh_host_ed25519_key"
    host_key.write_text("private")
    context = BuildContext(
        root, runner=Mock(spec=CommandRunner), artifacts={"kernel": "vmlinuz-test"}
    )
    stage = FinalizeStage(Settings(Path("/dev/vdb"), tmp_path, tmp_path / "config"))
    stage.execute(context)
    stage.verify(context)
    assert not host_key.exists()
    assert not (root / "tmp/build-residue").exists()
    assert (root / "etc/machine-id").read_text() == ""
    for relative in ("tmp", "var/tmp"):
        assert (root / relative).stat().st_mode & 0o7777 == 0o1777


def test_target_queries_use_portage_database(tmp_path: Path) -> None:
    root = tmp_path / "target"
    database = Mock()
    database.match.return_value = ["sys-libs/glibc-2.42"]
    database.aux_get.return_value = ["2.42", "amd64"]
    portage = SimpleNamespace(
        create_trees=Mock(
            return_value={str(root) + "/": {"vartree": SimpleNamespace(dbapi=database)}}
        ),
        dep=SimpleNamespace(Atom=lambda value: value),
    )
    backend = PortageBackend(Mock(), root)
    with patch("importlib.import_module", return_value=portage):
        assert backend.installed("sys-libs/glibc") == ["sys-libs/glibc-2.42"]
        assert backend.metadata("sys-libs/glibc-2.42", ("PV", "ARCH")) == {
            "PV": "2.42",
            "ARCH": "amd64",
        }
    assert portage.create_trees.call_args.kwargs["target_root"] == str(root)
    assert portage.create_trees.call_count == 1
