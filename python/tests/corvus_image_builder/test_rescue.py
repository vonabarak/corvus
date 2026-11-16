"""Check rescue media boundaries and standalone recipe contracts."""

import os
import runpy
import subprocess
import sys
from pathlib import Path
from unittest.mock import Mock, patch

import pytest
from corvus_image_builder.core import BuildContext, BuildManager, CommandRunner
from corvus_image_builder.gentoo import Settings
from corvus_image_builder.gentoo.rescue import RescueBootloaderStage, RescueCleanupStage

import yaml

ROOT = Path(__file__).resolve().parents[3]


def test_rescue_plan_and_recipe(tmp_path: Path) -> None:
    script = runpy.run_path(str(ROOT / "yaml/gentoo-python/rescue.py"))
    context = BuildContext(tmp_path)
    names = BuildManager().plan(
        script["build_recipe"](Settings(Path("/dev/vdb"), tmp_path, None)), context
    )
    assert (
        names.index("RescueCleanupStage")
        < names.index("RescuePackStage")
        < names.index("RescueBootloaderStage")
    )
    recipe = yaml.safe_load((ROOT / "yaml/gentoo-python/rescue.yml").read_text())
    build = recipe["pipeline"][1]["build"]
    assert build["target"]["format"] == "raw"
    assert build["target"]["size"] == "2G"
    files = [p["file"]["from"] for p in build["provisioners"] if "file" in p]
    assert len(files) == 8
    assert not any(name.endswith(("build.sh", "lib.sh", "kernel.sh")) for name in files)
    for name in files[1:]:
        assert (ROOT / "yaml/gentoo-python" / name).is_file()
    assert not (tmp_path / "etc").exists()


@pytest.mark.parametrize("uuid", ["abcd-1234", "bad;reboot"])
def test_rescue_grub_uses_validated_carrier_uuid(tmp_path: Path, uuid: str) -> None:
    esp = tmp_path / "esp"
    loader = esp / "EFI/BOOT/BOOTX64.EFI"
    loader.parent.mkdir(parents=True)
    loader.write_bytes(b"efi")
    context = BuildContext(
        tmp_path,
        Mock(spec=CommandRunner),
        artifacts={"esp": esp, "carrier-partition": "/dev/vdb3"},
    )
    stage = RescueBootloaderStage(Settings(Path("/dev/vdb"), tmp_path, None))
    with patch("subprocess.check_output", return_value=uuid):
        if ";" in uuid:
            with pytest.raises(ValueError, match="UUID"):
                stage.execute(context)
            assert "image" not in context.artifacts
            return
        stage.execute(context)
    cfg = (esp / "grub/grub.cfg").read_text()
    assert f"root=rescue:UUID={uuid}" in cfg
    assert "rd.lvm=0" in cfg and "rd.md=0" in cfg
    assert context.artifacts["image"] == Path("/dev/vdb")


def test_cleanup_does_not_publish_image(tmp_path: Path) -> None:
    context = BuildContext(
        tmp_path, Mock(spec=CommandRunner), artifacts={"kernel": "6.12-gentoo"}
    )
    stage = RescueCleanupStage(Settings(Path("/dev/vdb"), tmp_path, None))
    with patch.object(stage, "reset_identity"), patch.object(stage, "clean"):
        stage.execute(context)
    stage.verify(context)
    assert "image" not in context.artifacts


def test_rescue_bundle_plans_outside_repository(tmp_path: Path) -> None:
    bundler = runpy.run_path(str(ROOT / "yaml/gentoo-python/bundle.py"))["bundle"]
    archive = tmp_path / "rescue.pyz"
    bundler(ROOT / "yaml/gentoo-python/rescue.py", archive)
    result = subprocess.run(
        [sys.executable, "-I", str(archive)],
        cwd=tmp_path,
        env={**os.environ, "IMAGE_BUILD_PLAN": "1"},
        capture_output=True,
        text=True,
        check=True,
    )
    assert "RescueBootloaderStage" in result.stdout
