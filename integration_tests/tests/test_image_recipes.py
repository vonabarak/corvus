"""Check image dependency ordering and policies with real Makefiles on the host.

Only crv/curl are stubbed. These tests exercise publication orchestration rather
than Python bindings, and do not need a daemon or a nested test-node VM.
"""

from __future__ import annotations

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
            "debian-nginx.yml",
            "ubuntu-nginx.yml",
            "corvus-monitor.yml",
        ]
        assert sorted(pipelines) == sorted(expected)
        assert pipelines.index("gentoo-headless.yml") < pipelines.index(
            "corvus-test-node.yml"
        )
        for consumer in expected[5:]:
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
    def test_deferred_windows_policy_unchanged(
        self, recipes: RecipeRunner, recipe: str
    ) -> None:
        doc = yaml.safe_load(
            (recipes.root / "yaml" / recipe / f"{recipe}.yml").read_text()
        )
        assert doc["vars"]["image_if_exists"] == "overwrite"
