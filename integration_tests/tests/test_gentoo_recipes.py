"""Exercise Gentoo recipe orchestration locally, without rebuilding VM fixtures.

Stub only the external commands: real Makefiles perform dependency ordering,
upstream resolution, and policy forwarding. These tests need no test-node VM.
"""

from __future__ import annotations

import base64
from pathlib import Path

import pytest
from corvus_client._async.build import preprocess_build_yaml

import yaml

from .fixtures.image_recipe_command import FILENAME, SHA256
from .fixtures.image_recipes import RecipeRunner


@pytest.fixture
def recipes(tmp_path: Path) -> RecipeRunner:
    return RecipeRunner(tmp_path)


class TestGentooRecipes:
    @pytest.mark.parametrize(
        "recipe,target,variables,pipelines,policy",
        [
            ("gentoo-headless", "build-cloud", (), ["gentoo-base-cloud"], "update"),
            (
                "gentoo-headless",
                "build",
                (),
                ["gentoo-base-cloud", "gentoo-headless"],
                "update",
            ),
            (
                "gentoo-headless",
                "build-headless",
                (),
                ["gentoo-base-cloud", "gentoo-headless"],
                "update",
            ),
            (
                "gentoo-test",
                "build",
                (),
                ["gentoo-base-cloud", "gentoo-headless", "gentoo-test"],
                "update",
            ),
            (
                "gentoo-test",
                "build-test",
                (),
                ["gentoo-base-cloud", "gentoo-headless", "gentoo-test"],
                "update",
            ),
            (
                "gentoo-headless",
                "rebuild",
                (),
                ["gentoo-base-cloud", "gentoo-headless"],
                "overwrite",
            ),
            (
                "gentoo-test",
                "rebuild",
                ("IMAGE_POLICY=skip",),
                ["gentoo-base-cloud", "gentoo-headless", "gentoo-test"],
                "overwrite",
            ),
            (
                "gentoo-headless",
                "build-headless-standalone",
                (),
                ["gentoo-base-cloud", "gentoo-headless-standalone"],
                "update",
            ),
            (
                "gentoo-headless",
                "build-headless-standalone",
                ("IMAGE_POLICY=overwrite",),
                ["gentoo-base-cloud", "gentoo-headless-standalone"],
                "overwrite",
            ),
            (
                "",
                "image",
                ("IMAGE=gentoo-headless",),
                ["gentoo-base-cloud", "gentoo-headless"],
                "update",
            ),
            (
                "",
                "image",
                ("IMAGE=gentoo",),
                ["gentoo-base-cloud", "gentoo-headless", "gentoo-test"],
                "update",
            ),
        ],
    )
    def test_build_chain(
        self,
        recipes: RecipeRunner,
        recipe: str,
        target: str,
        variables: tuple[str, ...],
        pipelines: list[str],
        policy: str,
    ) -> None:
        """Even existing images must reach the daemon's update comparison, in order."""
        result = recipes.run(recipe, target, variables)
        assert result.returncode == 0, result.stdout + result.stderr
        calls = recipes.calls()
        builds = [call for call in calls if call.command == "crv"]
        assert [call.args[1] for call in builds] == [
            f"{name}.yml" for name in pipelines
        ]
        assert all(call.args[0] == "build" for call in builds)
        assert all(f"image_if_exists={policy}" in call.args for call in builds)
        assert [call.command for call in calls] == ["curl", "curl"] + ["crv"] * len(
            pipelines
        )
        downloads = calls[:2]
        assert downloads[0].args[-1].endswith("latest-di-amd64-cloudinit.txt")
        assert downloads[1].args[-1].endswith(f"/{FILENAME}.sha256")
        assert f"gentoo_base_cloud_sha256={SHA256}" in builds[0].args
        assert any(arg.endswith(f"/{FILENAME}") for arg in builds[0].args)
        for build in builds[1:]:
            assert not any(arg.startswith("gentoo_base_cloud_") for arg in build.args)
        for build in builds:
            assert (build.cwd / build.args[1]).is_file()

    @pytest.mark.parametrize("recipe", ["gentoo-headless", "gentoo-test"])
    @pytest.mark.parametrize("target", ["ensure", "check"])
    def test_existing_outputs_are_reused(
        self, recipes: RecipeRunner, recipe: str, target: str
    ) -> None:
        result = recipes.run(recipe, target)
        assert result.returncode == 0, result.stdout + result.stderr
        assert recipes.calls()
        assert all(
            call.command == "crv" and call.args[0] == "-o" for call in recipes.calls()
        )

    @pytest.mark.parametrize(
        "recipe,target,missing,pipeline,downloads",
        [
            (
                "gentoo-headless",
                "ensure-cloud",
                "disk:gentoo-base-cloud",
                "gentoo-base-cloud.yml",
                2,
            ),
            (
                "gentoo-headless",
                "ensure-headless",
                "disk:gentoo-base-headless",
                "gentoo-headless.yml",
                0,
            ),
            ("gentoo-test", "ensure", "disk:gentoo-corvus-test", "gentoo-test.yml", 0),
        ],
    )
    def test_ensure_missing_outputs(
        self,
        recipes: RecipeRunner,
        recipe: str,
        target: str,
        missing: str,
        pipeline: str,
        downloads: int,
    ) -> None:
        result = recipes.run(recipe, target, env={"MISSING_OUTPUTS": missing})
        assert result.returncode == 0, result.stdout + result.stderr
        calls = recipes.calls()
        builds = [call for call in calls if call.args[0] == "build"]
        assert len(builds) == 1
        assert builds[0].args[1] == pipeline
        assert "image_if_exists=skip" in builds[0].args
        assert len([call for call in calls if call.command == "curl"]) == downloads

    @pytest.mark.parametrize(
        "env,completed",
        [
            ({"DOWNLOAD_ERROR": "manifest"}, []),
            ({"DOWNLOAD_ERROR": "checksum"}, []),
            ({"INVALID_DOWNLOAD": "manifest"}, []),
            ({"INVALID_DOWNLOAD": "checksum"}, []),
            ({"FAIL_BUILD": "gentoo-base-cloud.yml"}, ["gentoo-base-cloud.yml"]),
            (
                {"FAIL_BUILD": "gentoo-headless.yml"},
                ["gentoo-base-cloud.yml", "gentoo-headless.yml"],
            ),
        ],
    )
    def test_failure_stops_downstream_stages(
        self, recipes: RecipeRunner, env: dict[str, str], completed: list[str]
    ) -> None:
        result = recipes.run("gentoo-test", "build", env=env)
        assert result.returncode != 0
        assert [
            call.args[1] for call in recipes.calls() if call.args[0] == "build"
        ] == completed

    def test_ensure_lookup_error_stops_publication(self, recipes: RecipeRunner) -> None:
        result = recipes.run("gentoo-test", "ensure", env={"LOOKUP_ERROR": "1"})
        assert result.returncode != 0
        assert all(
            call.command == "crv" and call.args[0] == "-o" for call in recipes.calls()
        )

    @pytest.mark.parametrize(
        "env,attempted",
        [
            ({"DOWNLOAD_ERROR": "manifest"}, []),
            ({"DOWNLOAD_ERROR": "checksum"}, []),
            ({"INVALID_DOWNLOAD": "manifest"}, []),
            ({"INVALID_DOWNLOAD": "checksum"}, []),
            ({"FAIL_BUILD": "gentoo-base-cloud.yml"}, ["gentoo-base-cloud.yml"]),
        ],
    )
    def test_standalone_import_failure_stops_build(
        self, recipes: RecipeRunner, env: dict[str, str], attempted: list[str]
    ) -> None:
        """The standalone derivative must wait for a successful source command."""
        result = recipes.run("gentoo-headless", "build-headless-standalone", env=env)
        assert result.returncode != 0
        assert [
            call.args[1] for call in recipes.calls() if call.args[0] == "build"
        ] == attempted

    @pytest.mark.parametrize(
        "recipe",
        [
            "gentoo-headless/gentoo-base-cloud.yml",
            "gentoo-headless/gentoo-headless.yml",
            "gentoo-headless/gentoo-headless-standalone.yml",
            "gentoo-test/gentoo-test.yml",
        ],
    )
    @pytest.mark.parametrize("policy", ["update", "skip", "overwrite"])
    def test_yaml_policies_and_kernel_input(
        self, recipes: RecipeRunner, recipe: str, policy: str
    ) -> None:
        source = recipes.root / "yaml" / recipe
        doc = yaml.safe_load(source.read_text())
        assert doc["vars"]["image_if_exists"] == "update"
        import_variables = {"gentoo_base_cloud_url", "gentoo_base_cloud_sha256"}
        imports = [
            disk
            for step in doc["pipeline"]
            for disk in step.get("apply", {}).get("disks", [])
            if "import" in disk
        ]
        if source.name == "gentoo-base-cloud.yml":
            assert import_variables <= doc["vars"].keys()
            assert [disk["name"] for disk in imports] == ["gentoo-base-cloud"]
        else:
            assert import_variables.isdisjoint(doc["vars"])
            assert not imports
        doc["vars"]["image_if_exists"] = policy
        for key in ("gentoo_base_cloud_url", "gentoo_base_cloud_sha256"):
            if key in doc["vars"]:
                doc["vars"][key] = (
                    SHA256
                    if key.endswith("sha256")
                    else f"https://example.test/{FILENAME}"
                )
        source.write_text(yaml.safe_dump(doc, sort_keys=False))
        expanded = yaml.safe_load(preprocess_build_yaml(str(source)))
        kernel_inputs = []
        for step in expanded["pipeline"]:
            if "apply" in step:
                apply = step["apply"]
                assert apply["ifExists"] in {"skip", "overwrite"}
                for disk in apply.get("disks", []):
                    assert disk["ifExists"] == policy
                    assert disk["checksum"]["value"] == SHA256
            if "build" in step:
                build = step["build"]
                assert build["target"]["ifExists"] == policy
                for provisioner in build.get("provisioners", []):
                    if "file" in provisioner:
                        kernel_inputs.append(
                            base64.b64decode(provisioner["file"]["content"])
                        )
        if source.name in {"gentoo-headless.yml", "gentoo-headless-standalone.yml"}:
            assert kernel_inputs == [(source.parent / "gentoo-kernel").read_bytes()]
