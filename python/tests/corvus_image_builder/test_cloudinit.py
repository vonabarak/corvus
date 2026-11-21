"""Check dedicated cloud-init entry points and init-system configuration."""

import os
import subprocess
import sys
from pathlib import Path
from unittest.mock import Mock

import pytest
from corvus_image_builder.bundle import bundle
from corvus_image_builder.core import BuildContext
from corvus_image_builder.gentoo import Settings
from corvus_image_builder.gentoo.cloudinit import CloudInitStage

import yaml

ROOT = Path(__file__).resolve().parents[3]
DIRECTORY = ROOT / "yaml/gentoo"


@pytest.mark.parametrize("flavor", ["glibc-systemd-headless", "musl-openrc-headless"])
def test_cloudinit_bundle_and_overlay(flavor: str, tmp_path: Path) -> None:
    archive = tmp_path / "cloudinit.pyz"
    bundle(DIRECTORY / flavor / "build-cloudinit.py", archive)
    result = subprocess.run(
        [sys.executable, "-I", str(archive)],
        env={**os.environ, "IMAGE_BUILD_PLAN": "1"},
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=True,
    )
    assert "ExistingImageStage" in result.stdout
    assert "CloudInitStage" in result.stdout
    assert "DiskStage" not in result.stdout
    recipe = yaml.safe_load((DIRECTORY / flavor / f"{flavor}.yml").read_text())
    builds = [step["build"] for step in recipe["pipeline"] if "build" in step]
    assert len(builds) == 2
    assert builds[1]["strategy"] == "offline-overlay"
    assert builds[1]["sourceImage"] == f"gentoo-{flavor}"
    assert builds[1]["provisioners"][0]["file"]["from"] == "./build-cloudinit.pyz"


@pytest.mark.parametrize("libc,init", [("glibc", "systemd"), ("musl", "openrc")])
def test_cloudinit_configuration(
    tmp_path: Path, libc: str, init: str, monkeypatch: pytest.MonkeyPatch
) -> None:
    stage = CloudInitStage(
        Settings(Path("/dev/vdb"), tmp_path / "cache", None, libc=libc, init=init)
    )
    monkeypatch.setattr(stage, "packages", Mock())
    command = Mock()
    monkeypatch.setattr(stage, "command", command)
    monkeypatch.setattr(stage, "chroot", Mock())
    chroot_string = Mock()
    monkeypatch.setattr(stage, "chroot_string", chroot_string)
    cfg = tmp_path / "etc/cloud/cloud.cfg"
    cfg.parent.mkdir(parents=True)
    cfg.write_text(
        "cloud_config_modules:\n"
        "  - locale\n"
        "  - ssh\n"
    )
    distro = tmp_path / "usr/lib/python/cloudinit/distros/gentoo.py"
    distro.parent.mkdir(parents=True)
    distro.write_text(
        "class Distro(distros.Distro):\n"
        "    pass\n"
    )
    context = BuildContext(tmp_path)
    stage.execute(context)
    assert "- locale" not in cfg.read_text()
    assert "- ssh" in cfg.read_text()
    assert "NoCloud" in (cfg.parent / "cloud.cfg.d/90_corvus_nocloud.cfg").read_text()
    if init == "openrc":
        assert 'init_cmd = ["rc-service"]' in distro.read_text()
        chroot_string.assert_called_once_with(
            context, "rc-update add cloud-init-local boot"
        )
    else:
        command.assert_called_once()
    stage.verify(context)
