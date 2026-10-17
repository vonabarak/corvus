"""Exercise Gentoo recipe orchestration locally, without rebuilding VM fixtures.

Stub only the external commands: real Makefiles perform dependency ordering,
upstream resolution, and policy forwarding. These tests need no test-node VM.
"""

from __future__ import annotations

import base64
import os
import subprocess
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
    def test_shared_provisioners_and_script_expansion(
        self, recipes: RecipeRunner
    ) -> None:
        directory = recipes.root / "yaml/gentoo"
        source = directory / "gentoo-headless.yml"
        original = yaml.safe_load(source.read_text())
        expanded = yaml.safe_load(preprocess_build_yaml(str(source)))
        builds = {}
        for step, resolved in zip(
            original["pipeline"], expanded["pipeline"], strict=True
        ):
            if "build" not in step:
                continue
            build = step["build"]
            builds[build["name"]] = build
            for provisioner, inline in zip(
                build["provisioners"], resolved["build"]["provisioners"], strict=True
            ):
                if "shell" not in provisioner:
                    continue
                shell = provisioner["shell"]
                assert "script" not in inline["shell"]
                expected = (
                    (directory / shell["script"]).read_text()
                    if "script" in shell
                    else shell["inline"]
                )
                assert inline["shell"]["inline"] == expected
        assert set(builds) == {
            "gentoo-builder",
            "gentoo-headless",
            "gentoo-headless-cloudinit",
        }
        scripts = [
            Path(item["shell"]["script"]).stem
            for item in builds["gentoo-headless"]["provisioners"]
            if "shell" in item and "script" in item["shell"]
        ]
        assert scripts == ["bootstrap-system", "build-kernel", "configure-system"]

    @pytest.mark.parametrize("recipe", ["gentoo", "corvus-test-node"])
    @pytest.mark.parametrize("cache_dir", ["/data/corvus-cache", "/srv/build cache"])
    def test_cache_directory_forwarded(
        self, recipes: RecipeRunner, recipe: str, cache_dir: str
    ) -> None:
        result = recipes.run(recipe, "build", (f"GENTOO_CACHE_DIR={cache_dir}",))
        assert result.returncode == 0, result.stdout + result.stderr
        calls = [call for call in recipes.calls() if call.command == "crv"]
        assert len(calls) == 3
        for call in calls[1:]:
            assert f"cache_dir={cache_dir}" in call.args

    @pytest.mark.parametrize("fail_mount", [False, True])
    def test_cache_mounts(self, recipes: RecipeRunner, fail_mount: bool) -> None:
        doc = yaml.safe_load(
            (recipes.root / "yaml/gentoo/gentoo-headless.yml").read_text()
        )
        script = doc["pipeline"][0]["apply"]["templates"][0]["cloudInitConfig"][
            "userData"
        ]["bootcmd"][0][2]
        log = recipes.root / "mounts"
        # Redirect mountpoints to the fixture and stub mounts, never mount on the host.
        script = script.replace("/mnt/corvus-cache", str(recipes.root / "cache"))
        script = script.replace("/var/cache", str(recipes.root / "guest"))
        for command, body in {
            "mountpoint": "exit 1",
            "mount": 'printf "%s\\n" "$*" >> "$MOUNT_LOG"\n'
            + ("exit 7" if fail_mount else ":"),
        }.items():
            executable = recipes.bin / command
            executable.write_text("#!/bin/sh\n" + body + "\n")
            executable.chmod(0o755)
        result = subprocess.run(
            ["sh", "-ec", script],
            env={
                **os.environ,
                "PATH": f"{recipes.bin}:{os.environ['PATH']}",
                "MOUNT_LOG": str(log),
            },
            capture_output=True,
            text=True,
            check=False,
        )
        if fail_mount:
            assert result.returncode == 7
            assert not (recipes.root / "cache/distfiles").exists()
            assert len(log.read_text().splitlines()) == 1
        else:
            assert result.returncode == 0, result.stderr
            assert len(log.read_text().splitlines()) == 4
            for cache in ("distfiles", "binpkgs", "binhost"):
                assert (recipes.root / "cache" / cache).is_dir()
                assert (recipes.root / "guest" / cache).is_dir()

    @pytest.mark.parametrize("failure", [None, "mount", "directory", "file"])
    def test_node_cache_permissions(
        self, recipes: RecipeRunner, failure: str | None
    ) -> None:
        doc = yaml.safe_load(
            (recipes.root / "yaml/corvus-test-node/corvus-test-node.yml").read_text()
        )
        script = doc["pipeline"][1]["build"]["provisioners"][1]["shell"]["inline"]
        script = script.split("install -d /etc/portage/binrepos.conf")[0]
        script = script.replace("/mnt/corvus-cache", str(recipes.root / "cache"))
        commands = {"mount": "exit 7" if failure == "mount" else "exit 0"}
        if failure == "directory":
            commands["mktemp"] = "exit 1"
        if failure == "file":
            # /proc exists but does not permit creating arbitrary probe files.
            commands["mktemp"] = "echo /proc"
            commands["rmdir"] = "exit 0"
        for command, body in commands.items():
            executable = recipes.bin / command
            executable.write_text("#!/bin/sh\n" + body + "\n")
            executable.chmod(0o755)
        result = subprocess.run(
            ["sh", "-ec", script],
            env={**os.environ, "PATH": f"{recipes.bin}:{os.environ['PATH']}"},
            capture_output=True,
            text=True,
            check=False,
        )
        if failure is None:
            assert result.returncode == 0, result.stderr
            for cache in ("distfiles", "binpkgs", "binhost"):
                assert list((recipes.root / "cache" / cache).iterdir()) == []
        else:
            assert result.returncode != 0
            if failure != "mount":
                assert "check host cache permissions" in result.stderr

    def test_node_build_cache_is_temporary(self, recipes: RecipeRunner) -> None:
        source = recipes.root / "yaml/corvus-test-node/corvus-test-node.yml"
        doc = yaml.safe_load(preprocess_build_yaml(str(source)))
        bake = doc["pipeline"][0]["apply"]["templates"][0]
        runtime = doc["pipeline"][-1]["apply"]["templates"][0]
        assert bake["sharedDirs"] == [
            {"path": "/data/corvus-cache", "tag": "corvus-cache"}
        ]
        assert not runtime.get("sharedDirs")
        build = doc["pipeline"][1]["build"]
        assert build["template"] == bake["name"]
        env = build["shellDefaults"]["env"]
        assert {"-userfetch", "-userpriv"} <= set(env["FEATURES"].split())
        assert env["DISTDIR"] == "/mnt/corvus-cache/distfiles"
        assert env["PKGDIR"] == "/mnt/corvus-cache/binpkgs"
        shell = "\n".join(
            item["shell"]["inline"] for item in build["provisioners"] if "shell" in item
        )
        assert "mount -t nfs" not in shell
        assert "192.168.72.1" not in shell
        assert shell.index("mount -t tmpfs tmpfs /var/db/repos") < shell.index(
            "emerge-webrsync -k"
        )
        assert shell.index("--quiet-build dev-vcs/git") < shell.index(
            "emaint sync -r vonabarak"
        )
        assert (
            "sync-uri = https://github.com/gentoo-haskell/gentoo-haskell.git" in shell
        )
        assert "location = /mnt/corvus-cache/binhost/binhost" in shell
        assert shell.index(
            "rm /etc/portage/binrepos.conf/zz-corvus-cache.conf"
        ) > shell.index("--reinstall-atoms=dev-python/pycapnp")
        assert "umount /var/db/repos" in shell
        assert "umount /mnt/corvus-cache" in shell
        assert "/etc/portage/make.conf" not in shell
        commands = shell.replace("\\\n", " ")
        for line in commands.splitlines():
            if line.strip().startswith("emerge ") and "--config" not in line:
                assert "--buildpkg" in line

    def test_repository_storage_policy(self, recipes: RecipeRunner) -> None:
        for recipe in ("gentoo-headless", "gentoo-test"):
            source = recipes.root / f"yaml/gentoo/{recipe}.yml"
            assert (
                yaml.safe_load(source.read_text())["vars"]["cache_dir"]
                == "/data/corvus-cache"
            )
            doc = yaml.safe_load(preprocess_build_yaml(str(source)))
            for step in doc["pipeline"]:
                for template in step.get("apply", {}).get("templates", []):
                    shares = template.get("sharedDirs", [])
                    assert all(share["tag"] == "corvus-cache" for share in shares)
                    if template["name"] in {
                        "gentoo-headless",
                        "gentoo-headless-cloudinit",
                    }:
                        assert not shares
                if "build" not in step:
                    continue
                build = step["build"]
                if build["strategy"] == "offline-overlay":
                    helper = (
                        source.parent / "scripts/build-test-offline.sh"
                    ).read_text()
                    target = (
                        source.parent / "scripts/provision-test-root.sh"
                    ).read_text()
                    assert 'mount_at "$root/var/db/repos" -t tmpfs tmpfs' in helper
                    assert 'mount_at "$root/var/cache/$cache" --bind' in helper
                    assert "-userfetch -userpriv" in target
                    assert "emerge-webrsync -k" in target
                    continue
                env = build["shellDefaults"]["env"]
                assert env["DISTDIR"] == "/mnt/corvus-cache/distfiles"
                assert env["PKGDIR"] == "/mnt/corvus-cache/binpkgs"
                assert {"-userfetch", "-userpriv"} <= set(env["FEATURES"].split())
                shell = "\n".join(
                    item["shell"]["inline"]
                    for item in build["provisioners"]
                    if "shell" in item
                )
                if build["name"] in {"gentoo-headless-cloudinit", "gentoo-corvus-test"}:
                    assert shell.index(
                        "mount -t tmpfs tmpfs /var/db/repos"
                    ) < shell.index("emerge-webrsync -k")
                else:
                    assert "mount -t tmpfs" not in shell
                assert "emerge-webrsync -k" in shell
                if build["name"] in {"gentoo-builder", "gentoo-headless"}:
                    # Native BDEPENDs read the builder's configuration separately;
                    # shell environment overrides alone do not cover that root.
                    assert 'DISTDIR="/mnt/corvus-cache/distfiles"' in shell
                    assert 'PKGDIR="/mnt/corvus-cache/binpkgs"' in shell
                    assert 'FEATURES="${FEATURES} -userfetch -userpriv' in shell
                for line in shell.splitlines():
                    if (
                        not line.lstrip().startswith("#")
                        and " emerge " in " " + line
                        and not any(flag in line for flag in ("--sync", "--config"))
                    ):
                        args = line.split()
                        # The development package itself is deliberately rebuilt;
                        # its dependencies still populate the binary cache.
                        if (
                            build["name"] == "gentoo-corvus-test"
                            and args[0] == "emerge"
                            and args[-1] == "app-emulation/corvus"
                            and "--onlydeps" not in args
                        ):
                            continue
                        assert "--buildpkg" in line

    def test_target_finalization_without_cache_mounts(
        self, recipes: RecipeRunner
    ) -> None:
        doc = yaml.safe_load(
            (recipes.root / "yaml/gentoo/gentoo-headless.yml").read_text()
        )
        script = doc["pipeline"][1]["build"]["provisioners"][-1]["shell"]["inline"]
        root = recipes.root / "target"
        (root / "etc").mkdir(parents=True)
        log = recipes.root / "unmounts"
        for command, body in {
            "sync": ":",
            "umount": 'echo "$1" >> "$MOUNT_LOG"',
        }.items():
            executable = recipes.bin / command
            executable.write_text("#!/bin/sh\n" + body + "\n")
            executable.chmod(0o755)
        result = subprocess.run(
            ["sh", "-eu", "-c", script],
            env={
                **os.environ,
                "PATH": f"{recipes.bin}:{os.environ['PATH']}",
                "IMAGE_ROOT": str(root),
                "CORVUS_BUILD_NAME": "test",
                "MOUNT_LOG": str(log),
            },
            capture_output=True,
            text=True,
            check=False,
        )
        assert result.returncode == 0, result.stderr
        assert log.read_text().splitlines() == [str(root)]
        bootstrap = recipes.root / "yaml/gentoo/scripts/bootstrap-system.sh"
        assert "mount --bind" not in bootstrap.read_text()

    @pytest.mark.parametrize("fail_install", [False, True])
    def test_cloud_init_configuration(
        self, recipes: RecipeRunner, fail_install: bool
    ) -> None:
        """Exercise the shared installer without package installation or a VM."""
        root = recipes.root / "target"
        log = recipes.root / "package-call"
        emerge = recipes.bin / "emerge"
        emerge.write_text(
            '#!/bin/sh\n[ -z "${SYSROOT+x}" ] || exit 99\n'
            'printf "%s\\n" "$@" > "$PACKAGE_LOG"\n'
            f"exit {7 if fail_install else 0}\n"
        )
        emerge.chmod(0o755)
        systemctl = recipes.bin / "systemctl"
        systemctl.write_text('#!/bin/sh\n[ "$1" = "--root=$IMAGE_ROOT" ]\n')
        systemctl.chmod(0o755)
        result = subprocess.run(
            [
                "sh",
                "-eu",
                str(recipes.root / "yaml/gentoo/scripts/install-cloud-init.sh"),
            ],
            env={
                **os.environ,
                "PATH": f"{recipes.bin}:{os.environ['PATH']}",
                "IMAGE_ROOT": str(root),
                "SYSROOT": "/invalid",
                "PACKAGE_LOG": str(log),
            },
            capture_output=True,
            text=True,
            check=False,
        )
        config = root / "etc/cloud/cloud.cfg.d/90_corvus_nocloud.cfg"
        if fail_install:
            assert result.returncode == 7
            assert not config.exists()
        else:
            assert result.returncode == 0, result.stderr
            assert yaml.safe_load(config.read_text()) == {
                "datasource_list": ["NoCloud", "None"],
                "locale": False,
            }
        assert f"--root={root}" in log.read_text().splitlines()
        assert "--buildpkg" in log.read_text().splitlines()

    @pytest.mark.parametrize(
        "recipe,target,variables,pipelines,policy",
        [
            ("gentoo", "build-cloud", (), ["gentoo-cloud"], "update"),
            (
                "gentoo",
                "build",
                (),
                ["gentoo-cloud", "gentoo-headless", "gentoo-test"],
                "update",
            ),
            (
                "gentoo",
                "build-headless",
                (),
                ["gentoo-cloud", "gentoo-headless"],
                "update",
            ),
            (
                "gentoo",
                "build-test",
                (),
                ["gentoo-cloud", "gentoo-headless", "gentoo-test"],
                "update",
            ),
            (
                "gentoo",
                "rebuild",
                ("HEADLESS_ONLY=1",),
                ["gentoo-cloud", "gentoo-headless"],
                "overwrite",
            ),
            (
                "gentoo",
                "rebuild",
                ("IMAGE_POLICY=skip",),
                ["gentoo-cloud", "gentoo-headless", "gentoo-test"],
                "overwrite",
            ),
            (
                "",
                "image",
                ("IMAGE=gentoo-headless",),
                ["gentoo-cloud", "gentoo-headless"],
                "update",
            ),
            (
                "",
                "image",
                ("IMAGE=gentoo",),
                ["gentoo-cloud", "gentoo-headless", "gentoo-test"],
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
        assert "gentoo_cloud_tag=20261004T164559Z" in builds[0].args
        assert f"gentoo_cloud_sha256={SHA256}" in builds[0].args
        assert any(arg.endswith(f"/{FILENAME}") for arg in builds[0].args)
        for build in builds[1:]:
            assert not any(arg.startswith("gentoo_cloud_") for arg in build.args)
        for build in builds:
            assert (build.cwd / build.args[1]).is_file()

    @pytest.mark.parametrize("recipe", ["gentoo"])
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
                "gentoo",
                "ensure-cloud",
                "disk:gentoo-cloud",
                "gentoo-cloud.yml",
                2,
            ),
            (
                "gentoo",
                "ensure-headless",
                "disk:gentoo-headless",
                "gentoo-headless.yml",
                0,
            ),
            ("gentoo", "ensure", "disk:gentoo-corvus-test", "gentoo-test.yml", 0),
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
            ({"FAIL_BUILD": "gentoo-cloud.yml"}, ["gentoo-cloud.yml"]),
            (
                {"FAIL_BUILD": "gentoo-headless.yml"},
                ["gentoo-cloud.yml", "gentoo-headless.yml"],
            ),
        ],
    )
    def test_failure_stops_downstream_stages(
        self, recipes: RecipeRunner, env: dict[str, str], completed: list[str]
    ) -> None:
        result = recipes.run("gentoo", "build", env=env)
        assert result.returncode != 0
        assert [
            call.args[1] for call in recipes.calls() if call.args[0] == "build"
        ] == completed

    def test_ensure_lookup_error_stops_publication(self, recipes: RecipeRunner) -> None:
        result = recipes.run("gentoo", "ensure", env={"LOOKUP_ERROR": "1"})
        assert result.returncode != 0
        assert all(
            call.command == "crv" and call.args[0] == "-o" for call in recipes.calls()
        )

    @pytest.mark.parametrize(
        "recipe",
        [
            "gentoo/gentoo-cloud.yml",
            "gentoo/gentoo-headless.yml",
            "gentoo/gentoo-test.yml",
        ],
    )
    @pytest.mark.parametrize("policy", ["update", "skip", "overwrite"])
    def test_yaml_policies_and_kernel_input(
        self, recipes: RecipeRunner, recipe: str, policy: str
    ) -> None:
        source = recipes.root / "yaml" / recipe
        doc = yaml.safe_load(source.read_text())
        assert doc["vars"]["image_if_exists"] == "update"
        import_variables = {
            "gentoo_cloud_url",
            "gentoo_cloud_tag",
            "gentoo_cloud_sha256",
        }
        imports = [
            disk
            for step in doc["pipeline"]
            for disk in step.get("apply", {}).get("disks", [])
            if "import" in disk
        ]
        if source.name == "gentoo-cloud.yml":
            assert import_variables <= doc["vars"].keys()
            assert [disk["name"] for disk in imports] == [
                "gentoo-cloud:{{ gentoo_cloud_tag }}"
            ]
        else:
            assert import_variables.isdisjoint(doc["vars"])
            assert not imports
        doc["vars"]["image_if_exists"] = policy
        for key in ("gentoo_cloud_url", "gentoo_cloud_tag", "gentoo_cloud_sha256"):
            if key in doc["vars"]:
                doc["vars"][key] = (
                    "20261004T164559Z"
                    if key.endswith("tag")
                    else SHA256
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
                    if disk.get("import"):
                        assert disk["name"] == "gentoo-cloud:20261004T164559Z"
            if "build" in step:
                build = step["build"]
                assert build["target"]["ifExists"] == policy
                for provisioner in build.get("provisioners", []):
                    if "file" in provisioner:
                        kernel_inputs.append(
                            base64.b64decode(provisioner["file"]["content"])
                        )
        if source.name == "gentoo-headless.yml":
            assert kernel_inputs == [(source.parent / "gentoo-kernel").read_bytes()]
