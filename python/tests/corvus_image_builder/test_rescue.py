"""Check rescue media boundaries and standalone recipe contracts."""

import importlib
import runpy
import shutil
import subprocess
from pathlib import Path
from types import ModuleType
from unittest.mock import Mock, patch

import pytest
from corvus_image_builder.core import BuildContext, BuildManager, CommandRunner
from corvus_image_builder.gentoo import Settings
from corvus_image_builder.gentoo.packages import GlibcPackageInstallStage

import yaml

ROOT = Path(__file__).resolve().parents[3]
RECIPE = ROOT / "yaml/gentoo/rescue"


@pytest.fixture
def rescue(monkeypatch: pytest.MonkeyPatch) -> ModuleType:
    monkeypatch.syspath_prepend(str(RECIPE))
    module = importlib.import_module("rescue_stages")
    monkeypatch.setattr(module, "ASSETS", RECIPE / "assets")
    return module


def test_rescue_bootstrap_copies_runtime_from_builder(
    tmp_path: Path, rescue: ModuleType
) -> None:
    root = tmp_path / "root"
    repository = tmp_path / "gentoo"
    (root / "usr/lib64").mkdir(parents=True)
    licenses = repository / "licenses"
    licenses.mkdir(parents=True)
    for name in ("GPL-3", "gcc-runtime-library-exception-3.1"):
        (licenses / name).write_text(name)
    for soname in rescue.GCC_RUNTIME:
        library = tmp_path / f"{soname}.0"
        library.write_text(soname)
        (tmp_path / soname).symlink_to(library)
    context = BuildContext(
        root, Mock(spec=CommandRunner), artifacts={"repository": repository}
    )
    stage = rescue.RescuePackagesStage(Settings(Path("/dev/vdb"), tmp_path, None))

    def gcc_lookup(argv: list[str], *, text: bool) -> str:
        assert text and argv[0] == "gcc"
        return str(tmp_path / argv[1].removeprefix("-print-file-name=")) + "\n"

    with (
        patch("subprocess.check_output", side_effect=gcc_lookup),
        patch.object(GlibcPackageInstallStage, "install_system") as install,
    ):
        stage.install_system(context)
    install.assert_called_once_with(context)
    for soname in rescue.GCC_RUNTIME:
        library = root / "usr/lib64" / soname
        assert library.read_text() == soname and not library.is_symlink()
    assert (root / "usr/share/licenses/gcc-runtime/GPL-3").is_file()
    assert not {"@system", "@world", "sys-devel/gcc"} & set(stage.atoms)
    assert set(stage.bootstrap_atoms) <= set(stage.atoms)


def test_rescue_plan_and_recipe(tmp_path: Path, rescue: ModuleType) -> None:
    script = runpy.run_path(str(RECIPE / "build.py"))
    context = BuildContext(tmp_path)
    names = BuildManager().plan(
        script["build_recipe"](Settings(Path("/dev/vdb"), tmp_path, None)), context
    )
    assert (
        names.index("RescueCleanupStage")
        < names.index("RescuePackStage")
        < names.index("RescueBootloaderStage")
    )
    recipe = yaml.safe_load((RECIPE / "rescue.yml").read_text())
    build = recipe["pipeline"][1]["build"]
    assert build["target"]["format"] == "raw"
    assert build["target"]["size"] == "2G"
    files = [p["file"]["from"] for p in build["provisioners"] if "file" in p]
    assert files == ["./build.pyz"]
    directories = [p["directory"] for p in build["provisioners"] if "directory" in p]
    assert directories == [
        {
            "from": "./assets",
            "to": "/run/rescue-assets",
            "mode": "0644",
        }
    ]
    assert (RECIPE / directories[0]["from"]).is_dir()
    repository = rescue.RescueRepositoryStage(
        Settings(Path("/dev/vdb"), tmp_path, None)
    )
    repository.validate(context)
    assert not (tmp_path / "etc").exists()


@pytest.mark.parametrize("uuid", ["abcd-1234", "bad;reboot"])
def test_rescue_grub_uses_validated_carrier_uuid(
    tmp_path: Path, uuid: str, rescue: ModuleType
) -> None:
    esp = tmp_path / "esp"
    loader = esp / "EFI/BOOT/BOOTX64.EFI"
    loader.parent.mkdir(parents=True)
    loader.write_bytes(b"efi")
    context = BuildContext(
        tmp_path,
        Mock(spec=CommandRunner),
        artifacts={"esp": esp, "carrier-partition": "/dev/vdb3"},
    )
    stage = rescue.RescueBootloaderStage(Settings(Path("/dev/vdb"), tmp_path, None))
    with patch("subprocess.check_output", return_value=uuid):
        if ";" in uuid:
            with pytest.raises(ValueError, match="UUID"):
                stage.execute(context)
            assert "image" not in context.artifacts
            return
        stage.execute(context)
    cfg = (esp / "grub/grub.cfg").read_text()
    assert cfg.count(f"linux /vmlinuz root=rescue:UUID={uuid}") == 2
    assert cfg.count("initrd /initramfs.img") == 2
    verified = cfg.split("menuentry 'Gentoo rescue (verify boot files)'")[1]
    assert verified.index("hashsum --hash sha256 --check /SHA256SUMS --keep-going") < (
        verified.index("linux /vmlinuz")
    )
    if shutil.which("grub-script-check"):
        subprocess.run(["grub-script-check", str(esp / "grub/grub.cfg")], check=True)
    assert "rd.lvm=0" in cfg and "rd.md=0" in cfg
    assert context.artifacts["image"] == Path("/dev/vdb")


def test_cleanup_does_not_publish_image(tmp_path: Path, rescue: ModuleType) -> None:
    context = BuildContext(
        tmp_path, Mock(spec=CommandRunner), artifacts={"kernel": "6.12-gentoo"}
    )
    stage = rescue.RescueCleanupStage(Settings(Path("/dev/vdb"), tmp_path, None))
    residue = (
        "usr/src/linux/Makefile",
        "usr/include/stdio.h",
        "usr/libexec/gcc/cc1",
        "usr/lib/gcc/x86_64-pc-linux-gnu/15/include/stddef.h",
        "usr/lib/gcc/x86_64-pc-linux-gnu/15/plugin/include/cp/cp-tree.h",
        "usr/x86_64-pc-linux-gnu/gcc-bin/15/gcc",
        "usr/bin/c99",
        "usr/bin/x86_64-pc-linux-gnu-g++-15",
        "var/db/pkg/sys-devel/gcc-15/CONTENTS",
    )
    runtime = (
        "usr/bin/strace",
        "usr/lib/gcc/x86_64-pc-linux-gnu/15/libstdc++.so.6",
        "usr/lib/modules/6.12-gentoo/initrd",
        "usr/lib/modules/6.12-gentoo/modules.dep",
        "boot/initramfs-6.12-gentoo.img",
    )
    for relative in (*residue, *runtime):
        path = tmp_path / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.touch()
    modules = tmp_path / "usr/lib/modules/6.12-gentoo"
    for name in ("build", "source"):
        (modules / name).symlink_to("/usr/src/linux")
    with patch.object(stage, "reset_identity"):
        stage.execute(context)
    stage.verify(context)
    assert "image" not in context.artifacts
    assert not any((tmp_path / path).exists() for path in residue)
    assert all((tmp_path / path).is_file() for path in runtime)
    assert not any((modules / name).is_symlink() for name in ("build", "source"))


def test_rescue_assets_are_validated_and_installed(
    tmp_path: Path, rescue: ModuleType, monkeypatch: pytest.MonkeyPatch
) -> None:
    settings = Settings(Path("/dev/vdb"), tmp_path, None)
    context = BuildContext(tmp_path, Mock(spec=CommandRunner))
    stage = rescue.RescueRuntimeStage(settings)
    stage.execute(context)
    for name, mode in (
        ("usr/local/sbin/rescue-home", 0o755),
        ("usr/local/sbin/rescue-network-state", 0o755),
        ("etc/systemd/system/rescue-home.service", 0o644),
    ):
        installed = tmp_path / name
        assert installed.read_bytes() == (RECIPE / "assets/root" / name).read_bytes()
        assert installed.stat().st_mode & 0o777 == mode
    monkeypatch.setattr(rescue, "ASSETS", tmp_path / "absent-assets")
    with pytest.raises(ValueError, match="Missing rescue asset"):
        rescue.RescueRepositoryStage(settings).validate(context)
