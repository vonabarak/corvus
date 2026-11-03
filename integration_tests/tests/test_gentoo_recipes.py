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

from .fixtures.gentoo_scripts import library_text
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
        source = directory / "gentoo-glibc-systemd-headless.yml"
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
            "gentoo-glibc-systemd-headless",
            "gentoo-glibc-systemd-headless-cloudinit",
        }
        scripts = [
            Path(item["shell"]["script"]).stem
            for item in builds["gentoo-glibc-systemd-headless"]["provisioners"]
            if "shell" in item and "script" in item["shell"]
        ]
        assert scripts == []
        assert any(
            item.get("shell", {}).get("inline")
            == "/bin/bash /usr/local/lib/corvus-build/images/gentoo-glibc-systemd-headless/build.sh"
            for item in builds["gentoo-glibc-systemd-headless"]["provisioners"]
        )

    @pytest.mark.parametrize(
        "name",
        ["gentoo-glibc-systemd-headless", "gentoo-glibc-systemd-headless-cloudinit"],
    )
    def test_uefi_templates_share_os_images(
        self, recipes: RecipeRunner, name: str
    ) -> None:
        source = recipes.root / "yaml/gentoo/gentoo-glibc-systemd-headless.yml"
        doc = yaml.safe_load(preprocess_build_yaml(str(source)))
        templates = {
            template["name"]: template
            for step in doc["pipeline"]
            for template in step.get("apply", {}).get("templates", [])
        }
        bios = templates[name]
        uefi = templates[name + "-uefi"]
        assert uefi["drives"][:1] == bios["drives"]
        assert uefi["drives"][1:] == [
            {
                "diskImage": "ovmf-code",
                "interface": "pflash",
                "readOnly": True,
                "strategy": "direct",
            },
            {"diskImage": "ovmf-vars", "interface": "pflash", "strategy": "clone"},
        ]
        ignored = {"name", "description", "drives"}
        assert {k: v for k, v in uefi.items() if k not in ignored} == {
            k: v for k, v in bios.items() if k not in ignored
        }

    @pytest.mark.parametrize("recipe", ["gentoo", "corvus-test-node"])
    @pytest.mark.parametrize("cache_dir", ["/data/corvus-cache", "/srv/build cache"])
    def test_cache_directory_forwarded(
        self, recipes: RecipeRunner, recipe: str, cache_dir: str
    ) -> None:
        result = recipes.run(
            "gentoo",
            "build",
            (
                f"GENTOO_CACHE_DIR={cache_dir}",
                *(["IMAGE=corvus-test-node"] if recipe == "corvus-test-node" else []),
            ),
        )
        assert result.returncode == 0, result.stdout + result.stderr
        calls = [call for call in recipes.calls() if call.command == "crv"]
        assert len(calls) == 4
        for call in calls[1:]:
            assert f"cache_dir={cache_dir}" in call.args

    @pytest.mark.parametrize("fail_mount", [False, True])
    def test_cache_mounts(self, recipes: RecipeRunner, fail_mount: bool) -> None:
        script = (
            recipes.root
            / "yaml/gentoo/scripts/images/gentoo-glibc-systemd-builder-cloudinit/build.sh"
        ).read_text()
        script = script.rsplit('main "$@"', 1)[0] + "\nmount_build_caches\n"
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
            ["bash", "-ec", script],
            env={
                **os.environ,
                "PATH": f"{recipes.bin}:{os.environ['PATH']}",
                "CORVUS_BUILD_SCRIPTS": str(recipes.root / "yaml/gentoo/scripts"),
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
            for cache in ("distfiles", "packages/x86-64", "binhost"):
                assert (recipes.root / "cache" / cache).is_dir()
                target = "binpkgs" if cache == "packages/x86-64" else cache
                assert (recipes.root / "guest" / target).is_dir()
                assert (
                    f"--bind {recipes.root}/cache/{cache} {recipes.root}/guest/{target}"
                    in log.read_text()
                )

    @pytest.mark.parametrize("failure", [None, "directory", "file"])
    def test_node_cache_permissions(
        self, recipes: RecipeRunner, failure: str | None
    ) -> None:
        script = library_text(recipes.root / "yaml/gentoo/scripts")
        cache = recipes.root / "cache"
        cache.mkdir()
        script += '\nenvironment_check_cache_writable "$1"\n'
        commands = {}
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
            ["bash", "-ec", script, "bash", str(cache)],
            env={**os.environ, "PATH": f"{recipes.bin}:{os.environ['PATH']}"},
            capture_output=True,
            text=True,
            check=False,
        )
        if failure is None:
            assert result.returncode == 0, result.stderr
            assert list(cache.iterdir()) == []
        else:
            assert result.returncode != 0
            assert "check host cache permissions" in result.stderr

    def test_node_build_cache_is_temporary(self, recipes: RecipeRunner) -> None:
        source = recipes.root / "yaml/gentoo/corvus-test-node.yml"
        doc = yaml.safe_load(preprocess_build_yaml(str(source)))
        build = doc["pipeline"][0]["build"]
        runtime = doc["pipeline"][-1]["apply"]["templates"][0]
        assert not runtime.get("sharedDirs")
        assert build["template"] == "gentoo-glibc-systemd-builder-cloudinit-bake"
        assert build["strategy"] == "offline-overlay"
        assert build["sourceImage"] == "gentoo-glibc-systemd-headless"
        script = (
            source.parent / "scripts/images/corvus-test-node/build.sh"
        ).read_text()
        assert "configure_target_caches" not in script
        assert "image_clean_temporary_files" in script
        assert script.index("install_node_packages\n") < script.index(
            "clean_node_build_files\n"
        )
        assert "--reinstall-atoms=dev-python/pycapnp" in script

    def test_repository_storage_policy(self, recipes: RecipeRunner) -> None:
        for recipe in (
            "gentoo-glibc-systemd-builder-cloudinit",
            "gentoo-glibc-systemd-headless",
            "gentoo-corvus-test",
        ):
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
                        "gentoo-glibc-systemd-headless",
                        "gentoo-glibc-systemd-headless-cloudinit",
                        "gentoo-glibc-systemd-headless-uefi",
                        "gentoo-glibc-systemd-headless-cloudinit-uefi",
                    }:
                        assert not shares
                if "build" not in step:
                    continue
                build = step["build"]
                if build["name"] == "gentoo-corvus-test":
                    helper = library_text(source.parent / "scripts")
                    assert "mounts_mount /var/db/repos -t tmpfs tmpfs" in helper
                    assert 'mounts_bind /var/db/repos "$root/var/db/repos"' in helper
                    assert (
                        'mounts_bind "/mnt/corvus-cache/$source" "$root/var/cache/$cache"'
                        in helper
                    )
                    assert "-userfetch -userpriv" in library_text(
                        source.parent / "scripts"
                    )
                    assert 'ROOT="$root" portage_emerge --root-deps' in helper
                    continue
                env = build["shellDefaults"]["env"]
                if build["name"] == "gentoo-glibc-systemd-builder-cloudinit":
                    assert env["DISTDIR"] == "/mnt/corvus-cache/distfiles"
                    assert env["PKGDIR"] == "/mnt/corvus-cache/packages/x86-64"
                else:
                    assert env["IMAGE_LIBC"] == "glibc"
                    assert env["IMAGE_ROOT"] == "/mnt/sysroot"
                assert {"-userfetch", "-userpriv"} <= set(env["FEATURES"].split())
                shell = "\n".join(
                    item["shell"]["inline"]
                    for item in build["provisioners"]
                    if "shell" in item
                )
                shell += "\n" + library_text(source.parent / "scripts")
                shell += "\n" + "\n".join(
                    base64.b64decode(item["file"]["content"]).decode()
                    for item in build["provisioners"]
                    if "file" in item and item["file"]["to"].endswith(".sh")
                )
                if build["name"] == "gentoo-corvus-test":
                    assert shell.index(
                        "mount -t tmpfs tmpfs /var/db/repos"
                    ) < shell.index("emerge-webrsync -k")
                else:
                    assert "mount -t tmpfs tmpfs /var/db/repos" not in shell
                assert "emerge-webrsync -k" in shell
                if build["name"] in {
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                }:
                    # Native BDEPENDs read the builder's configuration separately;
                    # shell environment overrides alone do not cover that root.
                    assert 'DISTDIR="/mnt/corvus-cache/distfiles"' in shell
                    assert 'PKGDIR="/mnt/corvus-cache/packages/x86-64"' in shell
                    assert (
                        'export FEATURES="${FEATURES:-} -userfetch -userpriv' in shell
                    )
                for line in shell.splitlines():
                    if (
                        not line.lstrip().startswith("#")
                        and " emerge " in " " + line
                        and not any(
                            flag in line
                            for flag in (
                                "--sync",
                                "--config",
                                'emerge --root-deps "$@"',
                            )
                        )
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

    @pytest.mark.parametrize("fail_install", [False, True])
    @pytest.mark.parametrize("libc", ["glibc", "musl"])
    def test_cloud_init_configuration(
        self, recipes: RecipeRunner, fail_install: bool, libc: str
    ) -> None:
        """Exercise the shared installer without package installation or a VM."""
        root = recipes.root / "target"
        (root / "etc/cloud").mkdir(parents=True)
        (root / "etc/cloud/cloud.cfg").write_text("cloud_config_modules:\n  - locale\n")
        backend = root / "usr/lib/python/cloudinit/distros/gentoo.py"
        backend.parent.mkdir(parents=True)
        backend.write_text("class Distro(distros.Distro):\n    pass\n")
        chroot = recipes.bin / "chroot"
        chroot.write_text("#!/bin/sh\nexit 0\n")
        chroot.chmod(0o755)
        log = recipes.root / "package-call"
        emerge = recipes.bin / "emerge"
        emerge.write_text(
            '#!/bin/sh\n[ -z "${SYSROOT+x}" ] || exit 99\n'
            '[ "$ROOT" = "$IMAGE_ROOT" ] || exit 98\n'
            'printf "%s\\n" "$@" > "$PACKAGE_LOG"\n'
            f"exit {7 if fail_install else 0}\n"
        )
        emerge.chmod(0o755)
        systemctl = recipes.bin / "systemctl"
        systemctl.write_text('#!/bin/sh\n[ "$1" = "--root=$IMAGE_ROOT" ]\n')
        systemctl.chmod(0o755)
        result = subprocess.run(
            [
                "bash",
                "-eu",
                str(recipes.root / "yaml/gentoo/scripts/stages/install-cloud-init.sh"),
            ],
            env={
                **os.environ,
                "PATH": f"{recipes.bin}:{os.environ['PATH']}",
                "IMAGE_ROOT": str(root),
                "IMAGE_LIBC": libc,
                "SYSROOT": "/invalid",
                "PACKAGE_LOG": str(log),
                "CORVUS_BUILD_SCRIPTS": str(recipes.root / "yaml/gentoo/scripts"),
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
            settings = yaml.safe_load(config.read_text())
            assert settings["datasource_list"] == ["NoCloud"]
            assert settings["system_info"]["default_user"]["name"] == "gentoo"
            assert settings["growpart"]["devices"] == ["/"]
            assert settings["resize_rootfs"] is True
            locale = config.with_name("91_corvus_locale.cfg")
            assert yaml.safe_load(locale.read_text())["locale"] == (
                "C.UTF-8" if libc == "musl" else False
            )
            assert "  - locale" not in (root / "etc/cloud/cloud.cfg").read_text()
            if libc == "musl":
                assert 'init_cmd = ["rc-service"]' in backend.read_text()
                assert (
                    yaml.safe_load(
                        config.with_name("92_corvus_network.cfg").read_text()
                    )["network"]["config"]
                    == "disabled"
                )
        assert not any(
            arg.startswith(("--root", "--sysroot"))
            for arg in log.read_text().splitlines()
        )
        assert "--buildpkg" in log.read_text().splitlines()

    @pytest.mark.parametrize(
        "image",
        [
            "",
            "unknown",
            "gentoo",
            "gentoo-glibc-systemd-upstream-cloudinit gentoo-musl-openrc-headless",
        ],
    )
    def test_invalid_image_stops_before_external_commands(
        self, recipes: RecipeRunner, image: str
    ) -> None:
        result = recipes.run("gentoo", "build", (f"IMAGE={image}",))
        assert result.returncode != 0
        assert "IMAGE" in result.stderr
        assert "gentoo-corvus-test" in result.stderr
        assert not recipes.calls()

    @pytest.mark.parametrize(
        "image,pipeline",
        [
            (
                "gentoo-glibc-systemd-upstream-cloudinit",
                "gentoo-glibc-systemd-upstream-cloudinit",
            ),
            (
                "gentoo-glibc-systemd-builder-cloudinit",
                "gentoo-glibc-systemd-builder-cloudinit",
            ),
            ("gentoo-musl-openrc-builder", "gentoo-musl-openrc-builder"),
            ("gentoo-glibc-systemd-headless", "gentoo-glibc-systemd-headless"),
            ("gentoo-musl-openrc-headless", "gentoo-musl-openrc-headless"),
            ("gentoo-corvus-test", "gentoo-corvus-test"),
        ],
    )
    def test_check_inspects_exactly_the_recipe_outputs(
        self, recipes: RecipeRunner, image: str, pipeline: str
    ) -> None:
        document = yaml.safe_load(
            (recipes.root / "yaml/gentoo" / f"{pipeline}.yml").read_text()
        )
        expected = set()
        for step in document["pipeline"]:
            if "build" in step:
                expected.add(("disk", step["build"]["name"]))
            for kind, key in (("disk", "disks"), ("template", "templates")):
                for output in step.get("apply", {}).get(key, []):
                    expected.add((kind, output["name"].split(":")[0]))
        result = recipes.run("gentoo", "check", (f"IMAGE={image}",))
        assert result.returncode == 0, result.stdout + result.stderr
        calls = recipes.calls()
        assert all(call.command == "crv" and call.args[0] == "-o" for call in calls)
        assert {(call.args[2], call.args[4]) for call in calls} == expected
        assert len(calls) == len(expected)

    def test_build_can_reuse_dependencies(self, recipes: RecipeRunner) -> None:
        result = recipes.run(
            "gentoo",
            "build",
            (
                "IMAGE=gentoo-glibc-systemd-headless",
                "BUILD_DEPENDENCIES=dependencies",
                "IMAGE_POLICY=overwrite",
                "GENTOO_CACHE_DIR=/srv/build cache",
            ),
        )
        assert result.returncode == 0, result.stdout + result.stderr
        calls = recipes.calls()
        assert not any(call.command == "curl" for call in calls)
        builds = [call for call in calls if call.args[0] == "build"]
        assert len(builds) == 1
        assert builds[0].args[1] == "gentoo-glibc-systemd-headless.yml"
        assert "image_if_exists=overwrite" in builds[0].args
        assert "cache_dir=/srv/build cache" in builds[0].args

    @pytest.mark.parametrize(
        "recipe,target,variables,pipelines,policy",
        [
            (
                "gentoo",
                "build",
                ("IMAGE=gentoo-glibc-systemd-upstream-cloudinit",),
                ["gentoo-glibc-systemd-upstream-cloudinit"],
                "update",
            ),
            (
                "gentoo",
                "build",
                ("IMAGE=gentoo-glibc-systemd-builder-cloudinit",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                ],
                "update",
            ),
            (
                "gentoo",
                "build",
                (),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                    "gentoo-corvus-test",
                ],
                "update",
            ),
            (
                "gentoo",
                "build",
                ("IMAGE=gentoo-glibc-systemd-headless",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                ],
                "update",
            ),
            (
                "gentoo",
                "build",
                ("IMAGE=gentoo-corvus-test",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                    "gentoo-corvus-test",
                ],
                "update",
            ),
            (
                "gentoo",
                "rebuild",
                ("IMAGE=gentoo-glibc-systemd-headless",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                ],
                "overwrite",
            ),
            (
                "gentoo",
                "rebuild",
                ("IMAGE_POLICY=skip",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                    "gentoo-corvus-test",
                ],
                "overwrite",
            ),
            (
                "",
                "image",
                ("IMAGE=gentoo-glibc-systemd-headless",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                ],
                "update",
            ),
            (
                "",
                "image",
                ("IMAGE=gentoo-corvus-test",),
                [
                    "gentoo-glibc-systemd-upstream-cloudinit",
                    "gentoo-glibc-systemd-builder-cloudinit",
                    "gentoo-glibc-systemd-headless",
                    "gentoo-corvus-test",
                ],
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

    @pytest.mark.parametrize(
        "image",
        [
            "gentoo-glibc-systemd-upstream-cloudinit",
            "gentoo-glibc-systemd-builder-cloudinit",
            "gentoo-musl-openrc-builder",
            "gentoo-glibc-systemd-headless",
            "gentoo-musl-openrc-headless",
            "gentoo-corvus-test",
        ],
    )
    @pytest.mark.parametrize("target", ["ensure", "check"])
    def test_existing_outputs_are_reused(
        self, recipes: RecipeRunner, image: str, target: str
    ) -> None:
        result = recipes.run("gentoo", target, (f"IMAGE={image}",))
        assert result.returncode == 0, result.stdout + result.stderr
        assert recipes.calls()
        assert all(
            call.command == "crv" and call.args[0] == "-o" for call in recipes.calls()
        )

    @pytest.mark.parametrize(
        "image,missing,pipeline,downloads",
        [
            (
                "gentoo-glibc-systemd-upstream-cloudinit",
                "disk:gentoo-glibc-systemd-upstream-cloudinit",
                "gentoo-glibc-systemd-upstream-cloudinit.yml",
                2,
            ),
            (
                "gentoo-glibc-systemd-headless",
                "disk:gentoo-glibc-systemd-headless",
                "gentoo-glibc-systemd-headless.yml",
                0,
            ),
            (
                "gentoo-glibc-systemd-builder-cloudinit",
                "disk:gentoo-glibc-systemd-builder-cloudinit",
                "gentoo-glibc-systemd-builder-cloudinit.yml",
                0,
            ),
            (
                "gentoo-glibc-systemd-builder-cloudinit",
                "template:gentoo-glibc-systemd-builder-cloudinit-bake",
                "gentoo-glibc-systemd-builder-cloudinit.yml",
                0,
            ),
            (
                "gentoo-musl-openrc-builder",
                "disk:gentoo-musl-openrc-builder",
                "gentoo-musl-openrc-builder.yml",
                2,
            ),
            (
                "gentoo-musl-openrc-builder",
                "template:gentoo-musl-openrc-builder-bake",
                "gentoo-musl-openrc-builder.yml",
                2,
            ),
            (
                "gentoo-corvus-test",
                "disk:gentoo-corvus-test",
                "gentoo-corvus-test.yml",
                0,
            ),
            (
                "gentoo-musl-openrc-headless",
                "disk:gentoo-musl-openrc-headless",
                "gentoo-musl-openrc-headless.yml",
                0,
            ),
            (
                "gentoo-musl-openrc-headless",
                "template:gentoo-musl-openrc-headless-cloudinit-uefi",
                "gentoo-musl-openrc-headless.yml",
                0,
            ),
        ],
    )
    def test_ensure_missing_outputs(
        self,
        recipes: RecipeRunner,
        image: str,
        missing: str,
        pipeline: str,
        downloads: int,
    ) -> None:
        result = recipes.run(
            "gentoo", "ensure", (f"IMAGE={image}",), env={"MISSING_OUTPUTS": missing}
        )
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
            (
                {"FAIL_BUILD": "gentoo-glibc-systemd-upstream-cloudinit.yml"},
                ["gentoo-glibc-systemd-upstream-cloudinit.yml"],
            ),
            (
                {"FAIL_BUILD": "gentoo-glibc-systemd-builder-cloudinit.yml"},
                [
                    "gentoo-glibc-systemd-upstream-cloudinit.yml",
                    "gentoo-glibc-systemd-builder-cloudinit.yml",
                ],
            ),
            (
                {"FAIL_BUILD": "gentoo-glibc-systemd-headless.yml"},
                [
                    "gentoo-glibc-systemd-upstream-cloudinit.yml",
                    "gentoo-glibc-systemd-builder-cloudinit.yml",
                    "gentoo-glibc-systemd-headless.yml",
                ],
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
            "gentoo/gentoo-glibc-systemd-upstream-cloudinit.yml",
            "gentoo/gentoo-glibc-systemd-builder-cloudinit.yml",
            "gentoo/gentoo-glibc-systemd-headless.yml",
            "gentoo/gentoo-corvus-test.yml",
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
        if source.name == "gentoo-glibc-systemd-upstream-cloudinit.yml":
            assert import_variables <= doc["vars"].keys()
            assert [disk["name"] for disk in imports] == [
                "gentoo-glibc-systemd-upstream-cloudinit:{{ gentoo_cloud_tag }}"
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
                        assert (
                            disk["name"]
                            == "gentoo-glibc-systemd-upstream-cloudinit:20261004T164559Z"
                        )
            if "build" in step:
                build = step["build"]
                assert build["target"]["ifExists"] == policy
                for provisioner in build.get("provisioners", []):
                    if (
                        provisioner.get("file", {}).get("to")
                        == "/tmp/gentoo-kernel-config"
                    ):
                        kernel_inputs.append(
                            base64.b64decode(provisioner["file"]["content"])
                        )
        if source.name == "gentoo-glibc-systemd-headless.yml":
            assert kernel_inputs == [
                (source.parent / "scripts/stages/kernel/gentoo-kernel").read_bytes()
            ]


@pytest.mark.parametrize("usepkg", ["true", "false"])
@pytest.mark.parametrize("getbinpkg", ["true", "false"])
def test_binary_controls_forwarded(
    recipes: RecipeRunner, usepkg: str, getbinpkg: str
) -> None:
    result = recipes.run(
        "gentoo",
        "build",
        ("IMAGE=corvus-test-vm", f"USEPKG={usepkg}", f"GETBINPKG={getbinpkg}"),
    )
    assert result.returncode == 0, result.stdout + result.stderr
    builds = [
        call
        for call in recipes.calls()
        if call.command == "crv" and call.args[0] == "build"
    ]
    assert len(builds) == 4
    for call in builds:
        if "gentoo-glibc-systemd-upstream-cloudinit.yml" in call.args:
            continue
        assert f"USEPKG={usepkg}" in call.args
        assert f"GETBINPKG={getbinpkg}" in call.args


def test_binary_controls_declared_in_all_installing_recipes(
    recipes: RecipeRunner,
) -> None:
    directory = recipes.root / "yaml/gentoo"
    for recipe in directory.glob("*.yml"):
        if recipe.stem == "gentoo-glibc-systemd-upstream-cloudinit":
            continue
        original = yaml.safe_load(recipe.read_text())
        for name in ("USEPKG", "GETBINPKG"):
            assert original["vars"][name] == "true"
        expanded = yaml.safe_load(preprocess_build_yaml(str(recipe)))
        for step in expanded["pipeline"]:
            if "build" in step:
                env = step["build"]["shellDefaults"]["env"]
                assert env["USEPKG"] == env["GETBINPKG"] == "true"
