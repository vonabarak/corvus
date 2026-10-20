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
    "multi-os": "debian-generic:12",
    "freebsd": "freebsd:15",
    "almalinux-10-media": "almalinux-10-dvd-iso",
    "almalinux-10-desktop": "almalinux-10-desktop-base",
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
        if recipe == "freebsd":
            expected = ["freebsd.yml"]
        if recipe == "corvus-test-node":
            expected = ["gentoo-cloud.yml", "gentoo-headless.yml"]
        if recipe.startswith("almalinux-"):
            expected = ["almalinux-10-media.yml"]
        if recipe not in {"multi-os", "freebsd", "almalinux-10-media"}:
            expected.append(f"{recipe}.yml")
        builds = publications(recipes)
        assert [call.args[1] for call in builds] == expected
        assert all(f"image_if_exists={policy}" in call.args for call in builds)

    @pytest.mark.parametrize("policy", ["update", "skip", "overwrite"])
    def test_freebsd_headless_refreshes_source(
        self, recipes: RecipeRunner, policy: str
    ) -> None:
        result = recipes.run(
            "freebsd", "build", ("HEADLESS=1", f"IMAGE_POLICY={policy}")
        )
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        assert [call.args[1] for call in builds] == [
            "freebsd.yml",
            "freebsd-headless.yml",
        ]
        assert all(f"image_if_exists={policy}" in call.args for call in builds)
        assert f"freebsd_sha512={SHA512}" in builds[0].args

    def test_freebsd_headless_ensure_missing_dependencies(
        self, recipes: RecipeRunner
    ) -> None:
        result = recipes.run(
            "freebsd",
            "ensure",
            ("HEADLESS=1",),
            env={"MISSING_OUTPUTS": "disk:freebsd:15,disk:freebsd-headless"},
        )
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        assert [call.args[1] for call in builds] == [
            "freebsd.yml",
            "freebsd-headless.yml",
        ]
        assert all("image_if_exists=skip" in call.args for call in builds)

    def test_freebsd_headless_failed_source_stops_build(
        self, recipes: RecipeRunner
    ) -> None:
        result = recipes.run(
            "freebsd", "build", ("HEADLESS=1",), env={"FAIL_BUILD": "freebsd.yml"}
        )
        assert result.returncode != 0
        assert [call.args[1] for call in publications(recipes)] == ["freebsd.yml"]

    def test_import_manifest_resolution(self, recipes: RecipeRunner) -> None:
        """Resolve provider formats, including Alpine's digest-only sidecar."""
        result = recipes.run("multi-os", "build")
        assert result.returncode == 0, result.stdout + result.stderr
        args = publications(recipes)[0].args
        for variable, digest in (
            ("debian_sha512", SHA512),
            ("ubuntu_sha256", SHA256),
            ("almalinux_sha256", SHA256),
            ("alpine_sha512", SHA512),
        ):
            assert f"{variable}={digest}" in args
        assert any(arg.endswith(f"/{ALPINE_FILE}") for arg in args)
        assert len([call for call in recipes.calls() if call.command == "curl"]) == 5

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
            env={"MISSING_OUTPUTS": "disk:debian-generic:12,disk:debian-12-nginx"},
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
                "almalinux-iso",
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
        recipe = (
            "almalinux-10-desktop" if resource == "almalinux-iso" else "debian-nginx"
        )
        if resource == "freebsd":
            recipe = "freebsd"
        result = recipes.run(recipe, "build", env={error: resource})
        assert result.returncode != 0
        assert not publications(recipes)

    @pytest.mark.parametrize(
        "recipe,failed",
        [
            ("corvus-test-node", "gentoo-cloud.yml"),
            ("corvus-test-node", "gentoo-headless.yml"),
            ("corvus-test-vm", "multi-os.yml"),
            ("almalinux-10-desktop", "almalinux-10-media.yml"),
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
                ".PHONY: build rebuild ensure\nbuild rebuild ensure:\n\t@:\n"
            )
        result = recipes.run("", target)
        assert result.returncode == 0, result.stdout + result.stderr
        builds = publications(recipes)
        pipelines = [call.args[1] for call in builds]
        expected = [
            "gentoo-cloud.yml",
            "gentoo-headless.yml",
            "gentoo-test.yml",
            "multi-os.yml",
            "freebsd.yml",
            "freebsd-headless.yml",
            "corvus-test-node.yml",
            "corvus-test-vm.yml",
            "virtio-win.yml",
            "windows-server-media.yml",
            "almalinux-10-media.yml",
            "almalinux-10-desktop.yml",
            "windows-11-desktop.yml",
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
        assert pipelines.index("almalinux-10-media.yml") < pipelines.index(
            "almalinux-10-desktop.yml"
        )
        assert all(f"image_if_exists={policy}" in call.args for call in builds)

    def test_windows_desktop_readiness_avoids_shell_quoting(
        self, recipes: RecipeRunner
    ) -> None:
        directory = recipes.root / "yaml/windows-11-desktop"
        doc = yaml.safe_load((directory / "windows-11-desktop.yml").read_text())
        provisioners = doc["pipeline"][1]["build"]["provisioners"]
        assert provisioners[0]["shell"]["script"] == "./keep-awake.ps1"
        assert (directory / "keep-awake.ps1").is_file()
        installer = (directory / "install-desktop.ps1").read_text()
        assert "C:\\Program Files\\PDF-XChange\\PDF Editor\\PXCEditor.exe" in installer
        probe = provisioners[1]["wait-for"]
        assert probe["script"] == "./wait-for-setup.ps1"
        assert probe["interpreter"] == "powershell"
        assert (directory / "wait-for-setup.ps1").is_file()
        # Disable paging only after setup; activate it with the existing reboot,
        # then validate the effective runtime state before destructive sealing.
        assert [next(iter(step)) for step in provisioners] == [
            "shell",
            "wait-for",
            "shell",
            "shell",
            "reboot",
            "shell",
            "shell",
            "shell",
            "shell",
        ]
        assert [
            step["shell"]["script"] for step in provisioners if "shell" in step
        ] == [
            "./keep-awake.ps1",
            "./disable-paging.ps1",
            "./install-desktop.ps1",
            "./keep-awake.ps1",
            "./verify-runtime-settings.ps1",
            "./seal.ps1",
            "./zero-free-space.ps1",
        ]
        assert all(
            (directory / step["shell"]["script"]).is_file()
            for step in provisioners
            if "shell" in step
        )

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
        assert len(imports) == (
            4
            if recipe == "multi-os"
            else 1
            if recipe in {"freebsd", "almalinux-10-media"}
            else 0
        )
        if recipe == "multi-os":
            assert [disk["name"] for disk in imports] == [
                "debian-generic:12",
                "ubuntu-server:26.04",
                "almalinux:10",
                "alpine:3.21",
            ]
        if recipe == "freebsd":
            assert imports[0]["name"] == "freebsd:15"
            assert imports[0]["checksum"]["target"] == "download"

    @pytest.mark.parametrize(
        "recipe", ["windows-11", "windows-server-2025", "almalinux-10-desktop"]
    )
    def test_installer_upload_policy_and_import_ownership(
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
            ("almalinux-10-desktop", ["almalinux-10-media.yml"]),
            ("windows-11", ["virtio-win.yml"]),
            ("windows-server-2025", ["virtio-win.yml", "windows-server-media.yml"]),
        ],
    )
    def test_installer_dependency_order_and_incremental_answer_media(
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

    def test_kickstart_edit_regenerates_answer_media(self, tmp_path: Path) -> None:
        recipes = RecipeRunner(tmp_path)
        directory = recipes.root / "yaml/almalinux-10-desktop"
        result = recipes.run("almalinux-10-desktop", "ensure")
        assert result.returncode == 0, result.stdout + result.stderr
        assert not (directory / "build/kickstart.iso").exists()
        for edit in (False, False, True):
            if edit:
                source = directory / "ks.cfg"
                source.write_text(source.read_text() + "\n# changed Kickstart\n")
                iso_mtime = (directory / "build/kickstart.iso").stat().st_mtime_ns
                os.utime(source, ns=(iso_mtime + 2_000_000_000,) * 2)
            result = recipes.run("almalinux-10-desktop", "build")
            assert result.returncode == 0, result.stdout + result.stderr
        generated = [call for call in recipes.calls() if call.command == "mkisofs"]
        assert len(generated) == 2
        assert all(
            call.args[call.args.index("-V") + 1] == "OEMDRV" for call in generated
        )
        args = publications(recipes)[0].args
        assert f"almalinux_sha256={SHA256}" in args
        assert any(
            arg.endswith("/AlmaLinux-10-latest-x86_64_v2-dvd.iso") for arg in args
        )
