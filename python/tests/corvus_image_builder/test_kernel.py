"""Check that kernel archives are isolated by recipe, source and config."""

import hashlib
from pathlib import Path
from unittest.mock import Mock

import pytest
from corvus_image_builder.core import BuildContext
from corvus_image_builder.gentoo import KernelStage, Settings


@pytest.mark.parametrize(
    ("options", "prefix"),
    [({}, "kernel"), ({"cache_prefix": "gentoo-rescue"}, "gentoo-rescue")],
)
def test_kernel_cache(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
    options: dict[str, str],
    prefix: str,
) -> None:
    config = tmp_path / "kernel.config"
    config.write_text("CONFIG_MODULES=y\n")
    digest = hashlib.sha256(config.read_bytes()).hexdigest()
    version = "6.18.63-r1"
    query = Mock(return_value=f"sys-kernel/gentoo-sources-{version}\n")
    monkeypatch.setattr("subprocess.check_output", query)
    stage = KernelStage(Settings(Path("/dev/vdb"), tmp_path, config), **options)
    command = Mock()
    packages = Mock()
    monkeypatch.setattr(stage, "command", command)
    monkeypatch.setattr(stage, "packages", packages)
    root = tmp_path / "root"
    boot = root / "boot"
    boot.mkdir(parents=True)
    image = "vmlinuz-6.18.63-gentoo"
    (boot / image).touch()
    archive = tmp_path / "kernels/glibc-systemd" / f"{prefix}-{version}-{digest}.tar.gz"
    archive.parent.mkdir(parents=True)
    archive.touch()
    context = BuildContext(root)

    stage.execute(context)
    command.assert_called_once_with(
        context, "tar", "-xzpf", str(archive), "-C", str(root)
    )
    packages.assert_not_called()
    assert context.artifacts["kernel"] == image

    class CacheMiss(Exception):
        pass

    packages.side_effect = CacheMiss
    config.write_text("CONFIG_MODULES=n\n")
    with pytest.raises(CacheMiss):
        stage.execute(context)
    config.write_text("CONFIG_MODULES=y\n")
    query.return_value = "sys-kernel/gentoo-sources-6.18.63-r2\n"
    with pytest.raises(CacheMiss):
        stage.execute(context)
