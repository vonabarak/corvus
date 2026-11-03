"""Exercise musl image publication and dependency ownership without VMs."""

from pathlib import Path

import pytest
from corvus_client._async.build import preprocess_build_yaml

import yaml

from .fixtures.image_recipe_command import MUSL_FILENAME, SHA256
from .fixtures.image_recipes import RecipeRunner


@pytest.fixture
def recipes(tmp_path: Path) -> RecipeRunner:
    return RecipeRunner(tmp_path)


@pytest.mark.parametrize("recipe,target", [("", "image"), ("gentoo", "build")])
@pytest.mark.parametrize("policy", ["update", "skip", "overwrite"])
def test_publication(
    recipes: RecipeRunner, policy: str, recipe: str, target: str
) -> None:
    result = recipes.run(
        recipe,
        target,
        (
            "IMAGE=gentoo-musl-openrc-headless",
            f"IMAGE_POLICY={policy}",
            "GENTOO_CACHE_DIR=/srv/build cache",
        ),
    )
    assert result.returncode == 0, result.stdout + result.stderr
    builds = [
        call
        for call in recipes.calls()
        if call.command == "crv" and call.args[0] == "build"
    ]
    assert [call.args[1] for call in builds] == [
        "gentoo-glibc-systemd-upstream-cloudinit.yml",
        "gentoo-glibc-systemd-builder-cloudinit.yml",
        "gentoo-musl-openrc-builder.yml",
        "gentoo-musl-openrc-headless.yml",
    ]
    assert f"image_if_exists={policy}" in builds[-1].args
    assert "cache_dir=/srv/build cache" in builds[-1].args
    assert f"musl_stage3_sha256={SHA256}" in builds[-2].args
    assert any(arg.endswith("/" + MUSL_FILENAME) for arg in builds[-2].args)
    assert not any("musl_stage3" in arg for arg in builds[-1].args)


@pytest.mark.parametrize(
    "resource,failure",
    [
        (resource, failure)
        for resource in ("musl-manifest", "musl-checksum")
        for failure in ("DOWNLOAD_ERROR", "INVALID_DOWNLOAD")
    ]
    + [("musl-checksum", "INVALID_DIGEST")],
)
def test_bad_upstream_does_not_publish(
    recipes: RecipeRunner, resource: str, failure: str
) -> None:
    result = recipes.run(
        "gentoo",
        "publish",
        ("IMAGE=gentoo-musl-openrc-builder",),
        env={failure: resource},
    )
    assert result.returncode != 0
    assert not any(call.command == "crv" for call in recipes.calls())


def test_ensure_does_not_resolve_or_publish_present_images(
    recipes: RecipeRunner,
) -> None:
    result = recipes.run("", "image-ensure", ("IMAGE=gentoo-musl-openrc-headless",))
    assert result.returncode == 0, result.stdout + result.stderr
    assert all(
        call.command == "crv" and call.args[0] != "build" for call in recipes.calls()
    )


@pytest.mark.parametrize(
    "failed",
    [
        "gentoo-glibc-systemd-upstream-cloudinit.yml",
        "gentoo-glibc-systemd-builder-cloudinit.yml",
        "gentoo-musl-openrc-builder.yml",
    ],
)
def test_failed_import_stops_derivative(recipes: RecipeRunner, failed: str) -> None:
    result = recipes.run(
        "gentoo",
        "build",
        ("IMAGE=gentoo-musl-openrc-headless",),
        env={"FAIL_BUILD": failed},
    )
    assert result.returncode != 0
    assert not any(
        "gentoo-musl-openrc-headless.yml" in call.args for call in recipes.calls()
    )


def test_offline_flavor_and_runtime_templates(recipes: RecipeRunner) -> None:
    source = recipes.root / "yaml/gentoo/gentoo-musl-openrc-headless.yml"
    document = yaml.safe_load(preprocess_build_yaml(str(source)))
    base, cloudinit = [
        step["build"] for step in document["pipeline"] if "build" in step
    ]
    assert base["strategy"] == "from-scratch"
    assert cloudinit["strategy"] == "offline-overlay"
    assert cloudinit["sourceImage"] == base["name"]
    assert (
        base["template"] == cloudinit["template"] == "gentoo-musl-openrc-builder-bake"
    )
    templates = document["pipeline"][-1]["apply"]["templates"]
    assert len(templates) == 4
    for template in templates:
        assert template["ram"] == "512M"
        assert template["guestAgent"]
        assert not template.get("sharedDirs")
        assert template["cloudInit"] == ("cloudinit" in template["name"])
    # Every referenced file must be expanded into the submitted recipe.
    for build in (base, cloudinit):
        for provisioner in build["provisioners"]:
            if "file" in provisioner:
                assert "from" not in provisioner["file"]
