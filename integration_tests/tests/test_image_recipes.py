"""Check image dependency ordering and policies with real Makefiles on the host.

Only crv/curl are stubbed. These tests exercise publication orchestration rather
than Python bindings, and do not need a daemon or a nested test-node VM.
"""

from __future__ import annotations

import io
import os
import tarfile
from pathlib import Path

import pytest

import yaml

from .fixtures.image_recipe_command import ALPINE_FILE, SHA256, SHA512
from .fixtures.image_recipes import RecipeCall, RecipeRunner

RECIPES = {
    "multi-os": "debian-12-generic-base",
    "corvus-test-node": "corvus-test-node",
    "corvus-test-vm": "corvus-test-vm",
    "debian-nginx": "debian-12-nginx",
    "ubuntu-nginx": "ubuntu26-nginx",
    "corvus-monitor": "corvus-monitor",
}


@pytest.fixture
def recipes(tmp_path: Path) -> RecipeRunner:
    return RecipeRunner(tmp_path)


def publications(recipes: RecipeRunner) -> list[RecipeCall]:
    return [
        call
        for call in recipes.calls()
        if call.command == "crv" and call.args[0] == "build"
    ]


class TestImageRecipes:
    @pytest.mark.parametrize("recipe", RECIPES)
    @pytest.mark.parametrize(
        "target,variables,policy",
        [
            ("build", (), "update"),
            ("build", ("IMAGE_POLICY=skip",), "skip"),
            ("build", ("IMAGE_POLICY=overwrite",), "overwrite"),
            ("rebuild", ("IMAGE_POLICY=skip",), "overwrite"),
        ],
    )
    def test_refreshes_upstream_before_consumer(
        self,
        recipes: RecipeRunner,
        recipe: str,
        target: str,
        variables: tuple[str, ...],
        policy: str,
    ) -> None:
        """Existing outputs still reach the daemon's comparison, in chain order.

        The daemon decides whether inputs changed; Make must refresh the import
        recipe first and forward the same policy to every dependent build, even
        with parallel Make. Rebuild always overrides an explicit skip policy.
        """
        result = recipes.run(recipe, target, variables)
        assert result.returncode == 0, result.stdout + result.stderr
        expected = ["multi-os.yml"]
        if recipe == "corvus-test-node":
            expected = ["gentoo-base-cloud.yml", "gentoo-headless.yml"]
        if recipe != "multi-os":
            expected.append(f"{recipe}.yml")
        builds = publications(recipes)
        assert [call.args[1] for call in builds] == expected
        assert all(f"image_if_exists={policy}" in call.args for call in builds)

    def test_import_manifest_resolution(self, recipes: RecipeRunner) -> None:
        """Resolve provider formats, including Alpine's digest-only sidecar."""
        result = recipes.run("multi-os", "build")
        assert result.returncode == 0, result.stdout + result.stderr
        args = publications(recipes)[0].args
        for variable, digest in (
            ("debian_sha512", SHA512),
            ("ubuntu_sha256", SHA256),
            ("almalinux_sha256", SHA256),
            ("freebsd_sha512", SHA512),
            ("alpine_sha512", SHA512),
        ):
            assert f"{variable}={digest}" in args
        assert any(arg.endswith(f"/{ALPINE_FILE}") for arg in args)
        assert len([call for call in recipes.calls() if call.command == "curl"]) == 6

    @pytest.mark.parametrize("recipe", RECIPES)
    @pytest.mark.parametrize("target", ["ensure", "check"])
    def test_existing_outputs_only_check_presence(
        self, recipes: RecipeRunner, recipe: str, target: str
    ) -> None:
        result = recipes.run(recipe, target)
        assert result.returncode == 0, result.stdout + result.stderr
        assert recipes.calls()
        assert all(
            call.command == "crv" and call.args[0] == "-o" for call in recipes.calls()
        )

    @pytest.mark.parametrize("recipe,output", RECIPES.items())
    def test_ensure_missing_output_publishes_with_skip(
        self, recipes: RecipeRunner, recipe: str, output: str
    ) -> None:
        result = recipes.run(
            recipe, "ensure", env={"MISSING_OUTPUTS": f"disk:{output}"}
        )
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        assert [call.args[1] for call in builds] == [f"{recipe}.yml"]
        assert "image_if_exists=skip" in builds[0].args

    def test_ensure_missing_dependency_precedes_consumer(
        self, recipes: RecipeRunner
    ) -> None:
        result = recipes.run(
            "debian-nginx",
            "ensure",
            env={"MISSING_OUTPUTS": "disk:debian-12-generic-base,disk:debian-12-nginx"},
        )
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        assert [call.args[1] for call in builds] == ["multi-os.yml", "debian-nginx.yml"]
        assert all("image_if_exists=skip" in call.args for call in builds)

    @pytest.mark.parametrize("recipe", RECIPES)
    def test_lookup_error_stops_ensure(
        self, recipes: RecipeRunner, recipe: str
    ) -> None:
        result = recipes.run(recipe, "ensure", env={"LOOKUP_ERROR": "1"})
        assert result.returncode != 0
        assert not publications(recipes)
        assert not any(call.command == "curl" for call in recipes.calls())

    @pytest.mark.parametrize(
        "resource,error",
        [
            (resource, error)
            for resource in (
                "debian",
                "ubuntu",
                "almalinux",
                "freebsd",
                "alpine-index",
                "alpine",
            )
            for error in ("DOWNLOAD_ERROR", "INVALID_DOWNLOAD", "INVALID_DIGEST")
            if resource != "alpine-index" or error != "INVALID_DIGEST"
        ],
    )
    def test_invalid_upstream_stops_consumer(
        self, recipes: RecipeRunner, resource: str, error: str
    ) -> None:
        result = recipes.run("debian-nginx", "build", env={error: resource})
        assert result.returncode != 0
        assert not publications(recipes)

    @pytest.mark.parametrize(
        "recipe,failed",
        [
            ("corvus-test-node", "gentoo-base-cloud.yml"),
            ("corvus-test-node", "gentoo-headless.yml"),
            ("corvus-test-vm", "multi-os.yml"),
            ("debian-nginx", "multi-os.yml"),
            ("ubuntu-nginx", "multi-os.yml"),
            ("corvus-monitor", "multi-os.yml"),
        ],
    )
    def test_failed_publication_stops_consumer(
        self, recipes: RecipeRunner, recipe: str, failed: str
    ) -> None:
        result = recipes.run(recipe, "build", env={"FAIL_BUILD": failed})
        assert result.returncode != 0
        assert publications(recipes)[-1].args[1] == failed
        assert not any(
            call.args[1] == f"{recipe}.yml" for call in publications(recipes)
        )

    def test_monitor_variable_is_forwarded(self, recipes: RecipeRunner) -> None:
        result = recipes.run(
            "corvus-monitor", "build", ("CORVUS_WEB_TARGET=10.0.0.5:8080",)
        )
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        assert "corvus_web_target=10.0.0.5:8080" in builds[-1].args
        assert not any(arg.startswith("corvus_web_target=") for arg in builds[0].args)

    @pytest.mark.parametrize(
        "target,policy", [("images", "update"), ("images-rebuild", "overwrite")]
    )
    def test_aggregates_prepare_sources_once(
        self, recipes: RecipeRunner, target: str, policy: str
    ) -> None:
        # Excluded publishers stay outside this test: they require ISO tools and
        # uploads. Their directory dispatch is covered by lifecycle tests.
        for recipe in ("windows-server-2025", "windows-11", "corvus-test-installer"):
            (recipes.root / "yaml" / recipe / "Makefile").write_text(
                ".PHONY: build rebuild\nbuild rebuild:\n\t@:\n"
            )
        result = recipes.run("", target)
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        pipelines = [call.args[1] for call in builds]
        expected = [
            "gentoo-base-cloud.yml",
            "gentoo-headless.yml",
            "gentoo-test.yml",
            "multi-os.yml",
            "corvus-test-node.yml",
            "corvus-test-vm.yml",
            "virtio-win.yml",
            "windows-server-media.yml",
            "debian-nginx.yml",
            "ubuntu-nginx.yml",
            "corvus-monitor.yml",
        ]
        assert sorted(pipelines) == sorted(expected)
        assert pipelines.index("gentoo-headless.yml") < pipelines.index(
            "corvus-test-node.yml"
        )
        for consumer in (
            "corvus-test-vm.yml",
            "debian-nginx.yml",
            "ubuntu-nginx.yml",
            "corvus-monitor.yml",
        ):
            assert pipelines.index("multi-os.yml") < pipelines.index(consumer)
        assert all(f"image_if_exists={policy}" in call.args for call in builds)

    @pytest.mark.parametrize("recipe", RECIPES)
    def test_yaml_policy_and_import_ownership(
        self, recipes: RecipeRunner, recipe: str
    ) -> None:
        doc = yaml.safe_load(
            (recipes.root / "yaml" / recipe / f"{recipe}.yml").read_text()
        )
        assert doc["vars"]["image_if_exists"] == "update"
        imports = []
        for step in doc["pipeline"]:
            if "build" in step:
                assert step["build"]["target"]["ifExists"] == "{{ image_if_exists }}"
            apply = step.get("apply", {})
            assert apply.get("ifExists", "skip") in {"skip", "overwrite"}
            for disk in apply.get("disks", []):
                if "import" in disk:
                    imports.append(disk)
                    assert disk["ifExists"] == "{{ image_if_exists }}"
                    assert disk["checksum"]["value"]
        assert len(imports) == (5 if recipe == "multi-os" else 0)
        if imports:
            freebsd = next(
                disk for disk in imports if disk["name"] == "freebsd-14-base"
            )
            assert freebsd["checksum"]["target"] == "download"

    @pytest.mark.parametrize("recipe", ["windows-11", "windows-server-2025"])
    def test_windows_upload_policy_and_import_ownership(
        self, recipes: RecipeRunner, recipe: str
    ) -> None:
        doc = yaml.safe_load(
            (recipes.root / "yaml" / recipe / f"{recipe}.yml").read_text()
        )
        assert doc["vars"]["image_if_exists"] == "update"
        upload = doc["pipeline"][0]["upload"]
        assert upload["ifExists"] == "{{ image_if_exists }}"
        assert upload["ephemeral"] is False
        assert not any(
            "import" in disk
            for step in doc["pipeline"]
            for disk in step.get("apply", {}).get("disks", [])
        )


class TestUploadRecipes:
    def test_synthetic_iso_incremental_generation_and_publication(
        self, tmp_path: Path
    ) -> None:
        """Make preserves unchanged ISO bytes, tracks package selection and files,
        forwards update/overwrite, and never modifies retained published files.
        Presence-only ensure does not generate local media.
        """
        recipes = RecipeRunner(tmp_path)
        directory = recipes.root / "yaml/corvus-test-installer"
        cache = directory / "cache"
        cache.mkdir()
        packages = {
            "kernel.apk": [
                "boot/vmlinuz-virt",
                "lib/virtio_blk.ko",
                "lib/cdrom.ko",
                "lib/sr_mod.ko",
                "lib/isofs.ko",
            ],
            "busybox.apk": ["bin/busybox.static"],
            "syslinux.apk": ["isolinux.bin", "ldlinux.c32"],
        }
        for package, members in packages.items():
            with tarfile.open(cache / package, "w:gz") as archive:
                for name in members:
                    entry = tarfile.TarInfo(name)
                    entry.size = 2048
                    archive.addfile(entry, io.BytesIO(b"x" * entry.size))
        variables = (
            "ALPINE_KERNEL_PKG=kernel.apk",
            "ALPINE_BUSYBOX_PKG=busybox.apk",
            "ALPINE_SYSLINUX_PKG=syslinux.apk",
            f"CORVUS_BASE_IMAGES_DIR={tmp_path / 'BaseImages'}",
        )
        old = (
            tmp_path / "BaseImages/SyntheticInstaller/123-corvus-test-installer-iso.raw"
        )
        old.parent.mkdir(parents=True)
        old.write_bytes(b"old-iso")
        iso = directory / "build/corvus-test-installer-iso.raw"

        def run(target: str, extra: tuple[str, ...] = ()) -> None:
            result = recipes.run("corvus-test-installer", target, variables + extra)
            assert result.returncode == 0, result.stdout + result.stderr
            assert old.read_bytes() == b"old-iso"

        run("ensure")
        assert not iso.exists()
        run("build")
        first_mtime = iso.stat().st_mtime_ns
        run("build")
        assert iso.stat().st_mtime_ns == first_mtime
        run("rebuild")
        assert iso.stat().st_mtime_ns == first_mtime
        # A source edit and a package configuration change each regenerate.
        source = directory / "init.sh"
        source.write_text(source.read_text() + "\n# changed input\n")
        os.utime(source, ns=(first_mtime + 2_000_000_000,) * 2)
        run("build")
        run("build", ("ALPINE_VERSION=v3.22",))
        generated = [call for call in recipes.calls() if call.command == "mkisofs"]
        assert len(generated) == 3
        uploads = [
            call for call in recipes.calls() if call.args[:2] == ["disk", "upload"]
        ]
        assert [call.args[-1] for call in uploads] == [
            "update",
            "update",
            "overwrite",
            "update",
            "update",
        ]

    @pytest.mark.parametrize(
        "recipe,imports",
        [
            ("windows-11", ["virtio-win.yml"]),
            ("windows-server-2025", ["virtio-win.yml", "windows-server-media.yml"]),
        ],
    )
    def test_windows_dependency_order_and_incremental_answer_media(
        self, tmp_path: Path, recipe: str, imports: list[str]
    ) -> None:
        recipes = RecipeRunner(tmp_path)
        variables = (f"ISO_TOOL={recipes.bin / 'mkisofs'}",)
        for target, policy in [("build", "update"), ("rebuild", "overwrite")]:
            result = recipes.run(recipe, target, variables)
            assert result.returncode == 0, result.stdout + result.stderr
            calls = publications(recipes)[-len(imports) - 1 :]
            assert [call.args[1] for call in calls] == imports + [recipe + ".yml"]
            assert all(f"image_if_exists={policy}" in call.args for call in calls)
        assert len([call for call in recipes.calls() if call.command == "mkisofs"]) == 1
