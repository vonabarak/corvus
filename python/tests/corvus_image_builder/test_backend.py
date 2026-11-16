import importlib.util
import os
import runpy
import subprocess
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock, patch

import pytest
from corvus_image_builder.core import BuildContext, BuildManager, CommandRunner
from corvus_image_builder.gentoo import Settings
from corvus_image_builder.portage import InstallRequest, PortageBackend

ROOT = Path(__file__).resolve().parents[3]
headless = runpy.run_path(str(ROOT / "yaml/gentoo-python/glibc-systemd-headless.py"))[
    "build_recipe"
]


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
    assert len(stages) == 9
    assert stages[0] == "WorkspaceStage" and stages[-1] == "FinalizeStage"
    assert not (tmp_path / "root").exists()


def test_bundle_deterministic(tmp_path: Path) -> None:
    spec = importlib.util.spec_from_file_location(
        "bundle", ROOT / "yaml/gentoo-python/bundle.py"
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    first, second = tmp_path / "first.pyz", tmp_path / "second.pyz"
    module.bundle(ROOT / "yaml/gentoo-python/glibc-systemd-headless.py", first)
    module.bundle(ROOT / "yaml/gentoo-python/glibc-systemd-headless.py", second)
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


@pytest.mark.parametrize("flavor", ["glibc-systemd-headless", "musl-openrc-headless"])
def test_bundle_runs_without_repository_path(flavor: str, tmp_path: Path) -> None:
    import subprocess
    import sys

    spec = importlib.util.spec_from_file_location(
        "bundle", ROOT / "yaml/gentoo-python/bundle.py"
    )
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    archive = tmp_path / "flavor.pyz"
    module.bundle(ROOT / f"yaml/gentoo-python/{flavor}.py", archive)
    result = subprocess.run(
        [sys.executable, "-I", str(archive)],
        cwd=tmp_path,
        env={**os.environ, "IMAGE_BUILD_PLAN": "1"},
        capture_output=True,
        text=True,
        check=True,
    )
    assert "HeadlessPackages" in result.stdout
    assert (
        "OpenRCConfigurationStage" in result.stdout
        if flavor.startswith("musl")
        else "SystemdConfigurationStage" in result.stdout
    )


@pytest.mark.parametrize("flavor", ["glibc-systemd-headless", "musl-openrc-headless"])
def test_flavor_preview_and_matching_yaml(flavor: str, tmp_path: Path) -> None:
    import subprocess
    import sys

    import yaml

    directory = ROOT / "yaml/gentoo-python"
    recipe = yaml.safe_load((directory / f"{flavor}.yml").read_text())
    build = recipe["pipeline"][1]["build"]
    assert build["name"] == f"gentoo-{flavor}"
    result = subprocess.run(
        [sys.executable, str(directory / f"{flavor}.py")],
        env={**os.environ, "IMAGE_BUILD_PLAN": "1", "PYTHONPATH": str(ROOT / "python")},
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=True,
    )
    assert (
        "OpenRCConfigurationStage" in result.stdout
        if flavor.startswith("musl")
        else "SystemdConfigurationStage" in result.stdout
    )


def test_settings_reject_mixed_variants(tmp_path: Path) -> None:
    with pytest.raises(ValueError, match="Unsupported"):
        Settings(tmp_path, tmp_path, tmp_path, libc="musl", init="systemd")
    settings = Settings(tmp_path, tmp_path, tmp_path, libc="musl", init="openrc")
    assert settings.variant == "x86-64_musl"
    assert "sys-libs/musl" in settings.system_atoms


def test_repository_sync_scopes_keyserver_adapter(tmp_path: Path) -> None:
    import subprocess

    from corvus_image_builder.gentoo import RepositoryStage

    executable = tmp_path / "real-gemato"
    executable.write_text(
        "#!/bin/sh\n"
        "printf \"%s\\n\" \"$@\"\n"
    )
    executable.chmod(0o755)
    commands = []
    adapters = []

    def run(argv: tuple[str, ...], *, env: dict[str, str]) -> None:
        commands.append(argv)
        adapter = Path(env["PATH"].split(os.pathsep)[0]) / "gemato"
        adapters.append(adapter)
        assert "ROOT" not in env and "SYSROOT" not in env
        result = subprocess.check_output(
            [str(adapter), "verify", "--key", "release.asc", "snapshot.tar.xz"],
            env=env,
            text=True,
        )
        assert result.splitlines() == [
            "verify",
            "--no-wkd",
            "--keyserver",
            "hkps://test.invalid",
            "--key",
            "release.asc",
            "snapshot.tar.xz",
        ]

    runner = Mock(spec=CommandRunner)
    runner.run.side_effect = run
    stage = RepositoryStage(Settings(tmp_path, tmp_path, tmp_path))
    with (
        patch("shutil.which", return_value=str(executable)),
        patch.dict(
            os.environ,
            PORTAGE_KEYSERVER="hkps://test.invalid",
            ROOT="/bad",
            SYSROOT="/bad",
        ),
    ):
        original_path = os.environ["PATH"]
        stage.sync_repository(BuildContext(tmp_path, runner=runner))
        assert os.environ["PATH"] == original_path
    assert commands == [("emerge-webrsync", "-k"), ("emerge", "--sync")]
    assert all(not adapter.exists() for adapter in adapters)


@pytest.mark.parametrize("failing_command", ["emerge-webrsync", "emerge"])
@pytest.mark.parametrize("persistent", [False, True])
def test_repository_sync_retries_and_stops_on_failure(
    tmp_path: Path, failing_command: str, persistent: bool
) -> None:
    from corvus_image_builder.gentoo import RepositoryStage

    attempts = 0

    def run(argv: tuple[str, ...], *, env: dict[str, str]) -> None:
        nonlocal attempts
        if argv[0] == failing_command:
            attempts += 1
            if persistent or attempts == 1:
                raise subprocess.CalledProcessError(1, argv)

    runner = Mock(spec=CommandRunner)
    runner.run.side_effect = run
    stage = RepositoryStage(Settings(tmp_path, tmp_path, tmp_path))
    with (
        patch("shutil.which", return_value="/usr/bin/gemato"),
        patch("corvus_image_builder.gentoo.repository.time.sleep") as sleep,
    ):
        context = BuildContext(tmp_path, runner=runner)
        if persistent:
            with pytest.raises(subprocess.CalledProcessError) as error:
                stage.sync_repository(context)
            assert error.value.cmd[0] == failing_command
        else:
            stage.sync_repository(context)
        assert attempts == (3 if persistent else 2)
        assert sleep.call_count == (2 if persistent else 1)

    commands = [call.args[0] for call in runner.run.call_args_list]
    if persistent and failing_command == "emerge-webrsync":
        assert all(command == ("emerge-webrsync", "-k") for command in commands)
    else:
        assert commands[-1] == ("emerge", "--sync")


@pytest.mark.parametrize("flavor", ["glibc-systemd-headless", "musl-openrc-headless"])
def test_make_forwards_keyserver_to_build_and_dependencies(flavor: str) -> None:
    result = subprocess.run(
        [
            "make",
            "--no-print-directory",
            "-n",
            "build",
            "MAKE=echo",
            f"IMAGE={flavor}",
            "GENTOO_KEYSERVER=hkps://test.invalid",
        ],
        cwd=ROOT / "yaml/gentoo-python",
        capture_output=True,
        text=True,
        check=True,
    )
    assert 'GENTOO_KEYSERVER="hkps://test.invalid" ensure' in result.stdout
    assert '--var portage_keyserver="hkps://test.invalid"' in result.stdout


def test_openrc_configuration(tmp_path: Path) -> None:
    from corvus_image_builder.gentoo import OpenRCConfigurationStage

    for relative in (
        "etc/conf.d/hwclock",
        "etc/conf.d/sysklogd",
        "etc/conf.d/qemu-guest-agent",
        "etc/inittab",
    ):
        path = tmp_path / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text('clock_hctosys="YES"\n' if "hwclock" in relative else "")
    runner = Mock(spec=CommandRunner)
    context = BuildContext(tmp_path, runner=runner)
    stage = OpenRCConfigurationStage(
        Settings(tmp_path, tmp_path, tmp_path, libc="musl", init="openrc")
    )
    stage.execute(context)
    stage.verify(context)
    assert (tmp_path / "etc/resolv.conf").is_file()
    assert not (tmp_path / "etc/resolv.conf").is_symlink()
    assert 'clock_hctosys="NO"' in (tmp_path / "etc/conf.d/hwclock").read_text()
    assert "VSOCK-LISTEN:22" in (tmp_path / "etc/init.d/vsock-sshd").read_text()
    assert "--retry-path" in (tmp_path / "etc/conf.d/qemu-guest-agent").read_text()
    commands = [call.args[0] for call in runner.run.call_args_list]
    assert all(
        "systemctl" not in command and "localedef" not in command
        for command in commands
    )
    assert any(command[-2:] == ("dhcpcd", "default") for command in commands)


def test_musl_system_install_avoids_glibc_bootstrap(tmp_path: Path) -> None:
    from corvus_image_builder.gentoo import MuslPackageInstallStage

    stage = MuslPackageInstallStage(
        Settings(tmp_path, tmp_path, tmp_path, libc="musl", init="openrc")
    )
    with (
        patch("corvus_image_builder.gentoo.packages.write") as write,
        patch.object(stage, "packages") as packages,
    ):
        stage.install_system(BuildContext(tmp_path))
    assert write.call_args.args[1] == "sys-apps/net-tools -hostname\n"
    assert packages.call_args.args[1] == (
        "@system",
        "sys-libs/musl",
        "sys-apps/openrc",
        "sys-apps/sysvinit",
    )


def test_development_and_rescue_install_policy() -> None:
    runner = Mock(spec=CommandRunner)
    portage = SimpleNamespace(dep=SimpleNamespace(Atom=Mock()))
    with patch("importlib.import_module", return_value=portage):
        PortageBackend(runner).install(
            InstallRequest(
                ("@world", "@system", "app-emulation/corvus"),
                Path("/target"),
                onlydeps=True,
                root_deps=True,
                update=True,
                deep=True,
                newuse=True,
                autounmask=False,
                binpkg_changed_deps=True,
            )
        )
    argv = runner.run.call_args.args[0]
    assert {
        "--onlydeps",
        "--root-deps=y",
        "--update",
        "--deep",
        "--newuse",
        "--autounmask=n",
        "--binpkg-changed-deps=y",
    } <= set(argv)
    assert "--binpkg-changed-deps=n" not in argv
    assert argv[-4:] == ["--", "@world", "@system", "app-emulation/corvus"]
    portage.dep.Atom.assert_called_once_with("app-emulation/corvus")
