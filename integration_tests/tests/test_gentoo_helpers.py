"""Run shared shell helpers against temporary roots, without mounting host disks."""

import os
import subprocess
from pathlib import Path

import pytest

import yaml

ROOT = Path(__file__).resolve().parents[2] / "yaml/gentoo"


def test_library_sourcing_has_no_side_effects() -> None:
    """Loading helpers must not initialize state, change the shell, or run commands."""
    result = subprocess.run(
        [
            "bash",
            "-euc",
            r"""
before_variables= after_variables= before_options= before_traps= before_directory=
before_variables=$(compgen -v)
before_options=$(set +o)
trap ':' USR1
before_traps=$(trap -p)
before_directory=$PWD
# An empty PATH catches external commands executed while loading the library.
PATH= . "$1"
PATH= . "$1"
after_variables=$(compgen -v)
[[ "$before_variables" == "$after_variables" ]]
[[ "$before_options" == "$(set +o)" ]]
[[ "$before_traps" == "$(trap -p)" ]]
[[ "$before_directory" == "$PWD" ]]
initialize_mount_tracking
[[ ${#mounts[@]} == 0 ]]
mounts=(parent child)
PATH= . "$1"
[[ ${mounts[*]} == 'parent child' ]]
initialize_mount_tracking
[[ ${#mounts[@]} == 0 ]]
""",
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
        ],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == result.stderr == ""


def test_recipe_script_and_library_provisioning() -> None:
    """Every file reference resolves and each consumer receives just one library."""
    for recipe in ROOT.glob("*.yml"):
        document = yaml.safe_load(recipe.read_text())
        for step in document["pipeline"]:
            if "build" not in step:
                continue
            files = [
                item["file"]
                for item in step["build"].get("provisioners", [])
                if "file" in item
            ]
            libraries = []
            for provisioner in files:
                if "from" not in provisioner:
                    continue
                source = ROOT / provisioner["from"]
                assert source.is_file(), source
                if "scripts/lib/" in provisioner["from"]:
                    libraries.append(provisioner)
            # The cloud import has no shell library consumer.
            if recipe.stem != "gentoo-cloud":
                assert libraries == [
                    {
                        "from": "./scripts/lib/build.sh",
                        "to": "/usr/local/lib/corvus-build/build.sh",
                        "mode": "0644",
                    }
                ]
    document = yaml.safe_load((ROOT / "gentoo-builder.yml").read_text())
    build = next(step["build"] for step in document["pipeline"] if "build" in step)
    shells = [item["shell"] for item in build["provisioners"] if "shell" in item]
    assert shells == [
        {
            "inline": "/bin/bash /tmp/prepare-bake-host.sh",
            "timeoutSec": 7680,
            "env": {"IMAGE_ROOT": "/"},
        }
    ]


@pytest.mark.parametrize(
    "failure",
    ["none", "network", "caches", "repository", "configuration", "tools", "update"],
)
def test_builder_preparation_order_and_failure(tmp_path: Path, failure: str) -> None:
    """Publish only after successful preparation, keeping shared cache contents."""
    for directory in ("etc", "tmp", "var/tmp", "var/cache"):
        (tmp_path / directory).mkdir(parents=True, exist_ok=True)
    for directory in ("tmp", "var/tmp", "var/cache"):
        (tmp_path / directory / "sentinel").touch()
    script = (ROOT / "scripts/prepare-bake-host.sh").read_text()
    definitions = script.rsplit('main "$@"', 1)[0]
    result = subprocess.run(
        [
            "bash",
            "-euc",
            definitions
            + r"""
record_step() {
    printf '%s\n' "$1"
    if [[ "$1" == "$FAIL_STEP" ]]; then return 7; fi
}
wait_for_network() { record_step network; }
mount_build_caches() { record_step caches; }
prepare_repository() {
    [[ ! -v ROOT && ! -v SYSROOT ]]
    record_step repository
}
configure_builder() { record_step configuration; }
install_builder_tools() { record_step tools; }
update_builder() { record_step update; }
sync() { record_step sync; }
mountpoint() { return 0; }
fstrim() { record_step "trim:$1"; }
main
""",
        ],
        env={
            **os.environ,
            "CORVUS_BUILD_LIB": str(ROOT / "scripts/lib"),
            "IMAGE_ROOT": str(tmp_path),
            "CORVUS_BUILD_NAME": "test-builder",
            "ROOT": "/invalid",
            "SYSROOT": "/invalid",
            "FAIL_STEP": failure,
        },
        capture_output=True,
        text=True,
    )
    steps = ["network", "caches", "repository", "configuration", "tools", "update"]
    if failure != "none":
        assert result.returncode == 7, result.stderr
        assert result.stdout.splitlines() == steps[: steps.index(failure) + 1]
        assert not (tmp_path / "etc/corvus-image-info").exists()
        assert (tmp_path / "tmp/sentinel").exists()
    else:
        assert result.returncode == 0, result.stderr
        assert result.stdout.splitlines() == steps + [
            "sync",
            f"trim:{tmp_path}",
            f"trim:{tmp_path}/boot/efi",
        ]
        assert (
            "image: test-builder\n" in (tmp_path / "etc/corvus-image-info").read_text()
        )
        assert not (tmp_path / "tmp/sentinel").exists()
        assert not (tmp_path / "var/tmp/sentinel").exists()
    assert (tmp_path / "var/cache/sentinel").exists()


def test_shell_syntax() -> None:
    for script_path in (ROOT / "scripts").rglob("*.sh"):
        subprocess.run(
            [
                "bash" if script_path.read_text().startswith("#!/bin/bash") else "sh",
                "-n",
                str(script_path),
            ],
            check=True,
            capture_output=True,
        )
    for recipe in ROOT.glob("*.yml"):
        document = yaml.safe_load(recipe.read_text())
        for step in document["pipeline"]:
            for template in step.get("apply", {}).get("templates", []):
                user_data = template.get("cloudInitConfig", {}).get("userData", {})
                for command in user_data.get("bootcmd", []) + user_data.get(
                    "runcmd", []
                ):
                    script = command if isinstance(command, str) else command[-1]
                    subprocess.run(
                        ["sh", "-n", "-c", script], check=True, capture_output=True
                    )


def test_boot_configuration(tmp_path: Path) -> None:
    (tmp_path / "boot").mkdir()
    (tmp_path / "etc").mkdir()
    for version in ("6.9", "6.10", "6.11.old"):
        (tmp_path / "boot" / f"vmlinuz-{version}").touch()
    subprocess.run(
        [
            "bash",
            "-ec",
            '. "$1"; write_target_fstab "$2"; '
            'write_target_grub_config "$2" "Gentoo guest"',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
            str(tmp_path),
        ],
        check=True,
    )
    assert (tmp_path / "boot/vmlinuz").readlink() == Path("vmlinuz-6.10")
    menu = (tmp_path / "boot/grub/grub.cfg").read_text()
    assert "search --set=root --label corvus-root" in menu
    assert "linux /boot/vmlinuz-6.10 root=/dev/vda3" in menu
    assert "LABEL=corvus-root / ext4" in (tmp_path / "etc/fstab").read_text()
    assert "LABEL=CORVUS-EFI /boot/efi vfat" in (tmp_path / "etc/fstab").read_text()


def test_missing_kernel_fails(tmp_path: Path) -> None:
    (tmp_path / "boot").mkdir()
    result = subprocess.run(
        [
            "bash",
            "-ec",
            '. "$1"; write_target_grub_config "$2" Gentoo',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
            str(tmp_path),
        ],
        check=False,
    )
    assert result.returncode != 0
    assert not (tmp_path / "boot/grub/grub.cfg").exists()


def test_grub_failure_stops_second_install(tmp_path: Path) -> None:
    command = tmp_path / "grub-install"
    log = tmp_path / "calls"
    command.write_text('#!/bin/sh\necho "$*" >> "$CALL_LOG"\nexit 7\n')
    command.chmod(0o755)
    result = subprocess.run(
        [
            "bash",
            "-ec",
            '. "$1"; install_target_grub /target /dev/example',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
        ],
        env={
            **os.environ,
            "PATH": f"{tmp_path}:{os.environ['PATH']}",
            "CALL_LOG": str(log),
        },
        check=False,
    )
    assert result.returncode == 7
    assert len(log.read_text().splitlines()) == 1


def test_kernel_archive_reused_and_config_invalidates(tmp_path: Path) -> None:
    source = tmp_path / "sources/linux-6.18-gentoo"
    (source / "arch/x86/boot").mkdir(parents=True)
    (source / "arch/x86/boot/bzImage").write_text("kernel")
    (source / "System.map").write_text("symbols")
    config = tmp_path / "kernel-config"
    config.write_text("CONFIG_TEST=y\n")
    cache = tmp_path / "cache"
    target = tmp_path / "target"
    binary = tmp_path / "bin"
    binary.mkdir()
    log = tmp_path / "compiles"
    version = tmp_path / "version"
    version.write_text("6.18")
    commands = {
        "portageq": 'printf sys-kernel/gentoo-sources-; cat "$VERSION_FILE"\n',
        "emerge": 'echo install >> "$INSTALL_LOG"\n',
        "mountpoint": "exit 0\n",
        "make": """case " $* " in
  *" kernelrelease "*) echo 6.18-gentoo ;;
  *" bzImage "*) echo compile >> "$COMPILE_LOG" ;;
  *" modules_install "*)
    mkdir -p "$SYSROOT/lib/modules/6.18-gentoo"
    echo module > "$SYSROOT/lib/modules/6.18-gentoo/example.ko" ;;
esac
""",
    }
    for name, body in commands.items():
        command = binary / name
        command.write_text("#!/bin/sh\n" + body)
        command.chmod(0o755)
    script = (ROOT / "scripts/build-kernel.sh").read_text()
    script = script.replace("/usr/src", str(tmp_path / "sources"))
    script = script.replace("/tmp/gentoo-kernel-config", str(config))
    script = script.replace("/mnt/corvus-cache", str(cache))
    env = {
        **os.environ,
        "PATH": f"{binary}:{os.environ['PATH']}",
        "SYSROOT": str(target),
        "COMPILE_LOG": str(log),
        "VERSION_FILE": str(version),
        "INSTALL_LOG": str(tmp_path / "installs"),
    }
    for attempt in range(4):
        if attempt == 1:
            (target / "boot/vmlinuz-6.18-gentoo").unlink()
            (target / "lib/modules/6.18-gentoo/example.ko").unlink()
        if attempt == 2:
            config.write_text("CONFIG_TEST=n\n")
        if attempt == 3:
            version.write_text("6.18-r1")
            source.rename(source.with_name("linux-6.18-gentoo-r1"))
        subprocess.run(["sh", "-ec", script], env=env, check=True, capture_output=True)
        assert (target / "boot/vmlinuz-6.18-gentoo").read_text() == "kernel"
        assert (target / "lib/modules/6.18-gentoo/example.ko").read_text() == "module\n"
        assert len(log.read_text().splitlines()) == max(1, attempt)
        assert len((tmp_path / "installs").read_text().splitlines()) == max(1, attempt)
    assert len(list((cache / "kernels").glob("kernel-*.tar.gz"))) == 3


def test_finalization_trims_only_image_filesystems(tmp_path: Path) -> None:
    """Unsupported discard must not prevent publication or touch shared caches."""
    (tmp_path / "etc").mkdir()
    log = tmp_path / "calls"
    result = subprocess.run(
        [
            "bash",
            "-ec",
            '. "$1"; sync() { echo sync >> "$TRIM_LOG"; }; '
            "mountpoint() { return 0; }; "
            'fstrim() { test -s "$TRIM_ROOT/etc/corvus-image-info"; '
            'echo "$1" >> "$TRIM_LOG"; return 1; }; '
            'finalize_image "$2" test-image',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
            str(tmp_path),
            str(log),
        ],
        env={**os.environ, "TRIM_LOG": str(log), "TRIM_ROOT": str(tmp_path)},
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stderr
    assert log.read_text().splitlines() == [
        "sync",
        str(tmp_path),
        str(tmp_path / "boot/efi"),
    ]
    assert "Discard unavailable" in result.stderr


@pytest.mark.parametrize("filesystem", ["tmpfs", "ext4"])
def test_scratch_cleanup_skips_only_tmpfs(tmp_path: Path, filesystem: str) -> None:
    """Leave live tmpfs alone, but still reject disk-backed scratch mounts."""
    for directory in ("tmp", "var/tmp"):
        (tmp_path / directory).mkdir(parents=True)
        (tmp_path / directory / "sentinel").touch()
    result = subprocess.run(
        [
            "bash",
            "-ec",
            r"""
. "$1"
findmnt() {
    if [[ "$*" == *FSTYPE* ]]; then
        [[ "$3" == "$IMAGE_ROOT/tmp" ]] || return 1
        printf '%s\n' "$FILESYSTEM"
    else
        printf '%s\n' "$IMAGE_ROOT/tmp"
    fi
}
clean_image_scratch_files "$IMAGE_ROOT"
""",
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
        ],
        env={**os.environ, "IMAGE_ROOT": str(tmp_path), "FILESYSTEM": filesystem},
        capture_output=True,
        text=True,
    )
    assert (result.returncode == 0) == (filesystem == "tmpfs"), result.stderr
    assert (tmp_path / "tmp/sentinel").exists()
    assert not (tmp_path / "var/tmp/sentinel").exists()
    if filesystem != "tmpfs":
        assert "Refusing to clean" in result.stderr


def test_cleanup_protects_mounted_cache(tmp_path: Path) -> None:
    """A nested cache mount prevents all deletion in its containing directory."""
    cache = tmp_path / "cache"
    cache.mkdir()
    package = cache / "package"
    package.write_text("keep")
    result = subprocess.run(
        [
            "bash",
            "-ec",
            '. "$1"; findmnt() { printf "%s\n" "$CACHE/nested"; }; '
            'clear_image_directory "$CACHE"',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
        ],
        env={**os.environ, "CACHE": str(cache)},
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode != 0
    assert package.read_text() == "keep"
    assert "Refusing to clean" in result.stderr


@pytest.mark.parametrize(
    "build_status,unmount_status,expected_status",
    [(0, 0, 0), (7, 0, 7), (0, 9, 1), (7, 9, 7)],
)
def test_mount_cleanup_preserves_failure_and_reverses_order(
    tmp_path: Path, build_status: int, unmount_status: int, expected_status: int
) -> None:
    """Preserve failed builds; successful builds attempt every unmount in reverse."""
    log = tmp_path / "unmounts"
    result = subprocess.run(
        [
            "bash",
            "-ec",
            'source "$1"; mount() { :; }; '
            'umount() { echo "$1" >> "$MOUNT_LOG"; return "$UNMOUNT_STATUS"; }; '
            'initialize_mount_tracking; trap cleanup EXIT; mount_at "$2/parent" device; '
            'mount_at "$2/parent/child" device; exit "$BUILD_STATUS"',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
            str(tmp_path),
        ],
        env={
            **os.environ,
            "MOUNT_LOG": str(log),
            "BUILD_STATUS": str(build_status),
            "UNMOUNT_STATUS": str(unmount_status),
        },
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == expected_status
    if build_status:
        assert not log.exists()
    else:
        assert log.read_text().splitlines() == [
            str(tmp_path / "parent/child"),
            str(tmp_path / "parent"),
        ]


def test_runtime_recipe_alignment() -> None:
    """Keep shared mechanics equal while allowing explicit image policy differences."""
    documents = {
        flavor: yaml.safe_load((ROOT / recipe).read_text())
        for flavor, recipe in (
            ("glibc", "gentoo-headless.yml"),
            ("musl", "gentoo-musl.yml"),
        )
    }
    user_data = []
    for flavor, document in documents.items():
        builds = [step["build"] for step in document["pipeline"] if "build" in step]
        base, cloud = builds
        assert base["strategy"] == "from-scratch"
        assert cloud["strategy"] == "offline-overlay"
        assert cloud["sourceImage"] == base["name"]
        assert (
            cloud["template"]
            == base["template"]
            == (
                "gentoo-builder-bake"
                if flavor == "glibc"
                else "gentoo-musl-builder-bake"
            )
        )
        for build, entry in (
            (base, "build-runtime.sh"),
            (cloud, "build-cloud-init.sh"),
        ):
            env = build["shellDefaults"]["env"]
            assert env["IMAGE_LIBC"] == flavor
            assert env["IMAGE_INIT"] == ("systemd" if flavor == "glibc" else "openrc")
            assert env["IMAGE_ROOT"] == "/mnt/sysroot"
            assert env["TARGET_DISK"] == "/dev/vdb"
            assert build["provisioners"][-1]["shell"] == {
                "inline": f"/bin/bash /tmp/{entry}",
                "timeoutSec": 28800,
            }
        for step in document["pipeline"]:
            for template in step.get("apply", {}).get("templates", []):
                if template["cloudInit"]:
                    assert template["sshKeys"] == [{"name": "corvus"}]
                    user_data.append(template["cloudInitConfig"]["userData"])
    assert user_data == [{"runcmd": [["passwd", "-d", "root"]]}] * 4
    for recipe in ROOT.glob("*.yml"):
        document = yaml.safe_load(recipe.read_text())
        for step in document["pipeline"]:
            for template in step.get("apply", {}).get("templates", []):
                assert template["name"] != "gentoo-headless-bake"
                for drive in template.get("drives", []):
                    if drive.get("interface") != "pflash":
                        assert drive["cacheType"] == "writeback"


def test_native_workspace_cache_isolation(tmp_path: Path) -> None:
    """A native builder must clear cross-build variables and select only its cache."""
    for flavor, cache in (
        ("glibc", "packages/x86-64"),
        ("musl", "packages/x86-64_musl"),
    ):
        result = subprocess.run(
            [
                "bash",
                "-euc",
                '. "$CORVUS_BUILD_LIB/build.sh"\n'
                "wait_for_network() { :; }\n"
                "mkdir() { :; }\n"
                "mountpoint() { return 0; }\n"
                'check_cache_writable() { printf "%s\\n" "$1"; }\n'
                'refresh_build_repository() { test -z "${ROOT+x}"; test -z "${SYSROOT+x}"; }\n'
                "prepare_workspace\n"
                'printf "%s\\n" "$DISTDIR" "$PKGDIR"',
            ],
            env={
                **os.environ,
                "ROOT": "/invalid",
                "SYSROOT": "/invalid",
                "IMAGE_LIBC": flavor,
                "CORVUS_BUILD_LIB": str(ROOT / "scripts/lib"),
            },
            capture_output=True,
            text=True,
            check=True,
        )
        assert result.stdout.splitlines() == [
            "/mnt/corvus-cache/distfiles",
            f"/mnt/corvus-cache/{cache}",
            f"/mnt/corvus-cache/binhost/{cache.rsplit('/', 1)[-1]}",
            "/mnt/corvus-cache/distfiles",
            f"/mnt/corvus-cache/{cache}",
        ]


def test_offline_entry_failure_cleanup(tmp_path: Path) -> None:
    """Both entry points stop on installer failure and preserve every tracked mount."""
    library = tmp_path / "lib"
    library.mkdir()
    (library / "build.sh").write_text(
        (ROOT / "scripts/lib/build.sh").read_text() + "\n"
        "prepare_workspace() { :; }\n"
        "partition_target_disk() { :; }\n"
        "mount_target_disk() { mounts+=(root efi proc); }\n"
        'umount() { echo "$1" >> "$LOG"; }\n'
        'finish_runtime() { echo published >> "$LOG"; }\n'
    )
    for entry, installer in (
        ("build-runtime.sh", "install-glibc-root.sh"),
        ("build-cloud-init.sh", "install-cloud-init.sh"),
    ):
        log = tmp_path / entry
        (tmp_path / installer).write_text("#!/bin/sh\nexit 7\n")
        script = (ROOT / "scripts" / entry).read_text().replace("/tmp/", f"{tmp_path}/")
        result = subprocess.run(
            ["bash", "-c", script],
            env={
                **os.environ,
                "CORVUS_BUILD_LIB": str(library),
                "IMAGE_LIBC": "glibc",
                "IMAGE_INIT": "systemd",
                "IMAGE_ROOT": "/unused",
                "TARGET_DISK": "/unused",
                "LOG": str(log),
            },
            capture_output=True,
            text=True,
        )
        assert result.returncode == 7, result.stderr
        assert not log.exists()


def test_binhost_configuration_is_libc_specific(tmp_path: Path) -> None:
    """Configuration written into an offline root never selects the other libc."""
    for libc, variant in (("glibc", "x86-64"), ("musl", "x86-64_musl")):
        root = tmp_path / libc
        subprocess.run(
            [
                "bash",
                "-ec",
                '. "$1"; configure_binhost "$2" "$3"',
                "bash",
                str(ROOT / "scripts/lib/build.sh"),
                libc,
                str(root),
            ],
            check=True,
        )
        config = (root / "etc/portage/binrepos.conf/zz-corvus-cache.conf").read_text()
        assert f"location = /mnt/corvus-cache/binhost/{variant}\n" in config
        assert (
            f"sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/{variant}\n"
            in config
        )
        assert "verify-signature = false" in config
    invalid = tmp_path / "invalid"
    result = subprocess.run(
        [
            "bash",
            "-ec",
            '. "$1"; configure_binhost invalid "$2"',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
            str(invalid),
        ],
        capture_output=True,
        check=False,
    )
    assert result.returncode != 0
    assert not invalid.exists()


def test_musl_bootstrap_binhost_and_minimal_policy(tmp_path: Path) -> None:
    """Bootstrap replaces stale binhosts while retaining minimal USE matching."""
    config = tmp_path / "etc/portage"
    (config / "binrepos.conf").mkdir(parents=True)
    (config / "binrepos.conf/glibc.conf").write_text("incorrect glibc host")
    (config / "make.conf").touch()
    script = (ROOT / "scripts/build-musl-builder.sh").read_text()
    definitions = script.rsplit('main "$@"', 1)[0]
    subprocess.run(
        [
            "bash",
            "-ec",
            definitions + '\nwork="$1"\nconfigure_builder\n',
            "bash",
            str(tmp_path),
        ],
        env={**os.environ, "CORVUS_BUILD_LIB": str(ROOT / "scripts/lib")},
        check=True,
        capture_output=True,
    )
    make_conf = (config / "make.conf").read_text()
    assert 'PKGDIR="/mnt/corvus-cache/packages/x86-64_musl"' in make_conf
    assert 'DISTDIR="/mnt/corvus-cache/distfiles"' in make_conf
    assert 'USE="-systemd -X -nls -doc -test"' in make_conf
    assert "--getbinpkg --usepkg --binpkg-respect-use=y" in make_conf
    assert " -getbinpkg" not in make_conf
    assert "--usepkgonly" not in make_conf
    assert not (config / "binrepos.conf/glibc.conf").exists()
    assert (
        "sys-apps/openrc -netifrc" in (config / "package.use/corvus-musl").read_text()
    )
    binhost = (config / "binrepos.conf/zz-corvus-cache.conf").read_text()
    assert "binhost/x86-64_musl\n" in binhost
    # The same namespace works before and after boot; detach it before cleaning.
    assert 'bind /mnt/corvus-cache "$work/mnt/corvus-cache"' in script
    assert script.index('umount "$work/mnt/corvus-cache"') < script.index(
        'clean_image_temporary_files "$IMAGE_ROOT"'
    )


def run_test_builder(code: str, *args: str) -> subprocess.CompletedProcess[str]:
    """Exercise entrypoint functions without invoking its main or privileged tools."""
    definitions = (
        (ROOT / "scripts/build-test-offline.sh").read_text().rsplit('main "$@"', 1)[0]
    )
    return subprocess.run(
        ["bash", "-euc", definitions + "\nset +x\n" + code, "bash", *args],
        env={**os.environ, "CORVUS_BUILD_LIB": str(ROOT / "scripts/lib")},
        capture_output=True,
        text=True,
    )


def test_development_install_uses_native_portage(tmp_path: Path) -> None:
    """Install targets through ROOT; synchronization and policy remain on the helper."""
    executable = tmp_path / "emerge"
    executable.write_text('#!/bin/bash\nset -eu\nemerge "$@"\n')
    executable.chmod(0o755)
    result = run_test_builder(
        r"""
root=$1
export PATH="$root:$PATH"
unset ROOT SYSROOT
write_package_policy() { printf 'policy %s\n' "$1"; }
emerge() {
    [[ ! ${SYSROOT+x} ]]
    printf 'ROOT=%s emerge %s\n' "${ROOT-unset}" "$*"
}
export -f emerge
install_development_packages
""",
        str(tmp_path),
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout.splitlines() == [
        "policy /",
        f"policy {tmp_path}",
        "ROOT=/ emerge --noreplace --usepkg --buildpkg --quiet-build dev-vcs/git",
        f"ROOT={tmp_path} emerge --root-deps --noreplace --usepkg --buildpkg --quiet-build dev-vcs/git",
        "ROOT=unset emerge --sync",
        f"ROOT={tmp_path} emerge --root-deps --usepkg --buildpkg --quiet-build --onlydeps app-emulation/corvus",
        f"ROOT={tmp_path} emerge --root-deps --verbose app-emulation/corvus",
    ]


def test_development_target_configuration(tmp_path: Path) -> None:
    """Offline configuration keeps cache mappings, target services, and resolver intact."""
    root = tmp_path / "root"
    (root / "etc/portage").mkdir(parents=True)
    (root / "etc/resolv.conf").write_text("unchanged\n")
    result = run_test_builder(
        r"""
root=$1
systemctl() { printf '%s\n' "$*"; }
configure_target_caches
write_package_policy "$root"
configure_vde_service
configure_vde_network
configure_nat
configure_dhcp
configure_login_banner
""",
        str(root),
    )
    assert result.returncode == 0, result.stderr
    fstab = (root / "etc/fstab").read_text()
    for cache, source in (
        ("distfiles", "distfiles"),
        ("binpkgs", "packages/x86-64"),
        ("binhost", "binhost"),
    ):
        assert f"/mnt/corvus-cache/{source} /var/cache/{cache} none bind," in fstab
    config = (root / "etc/portage/binrepos.conf/zz-corvus-cache.conf").read_text()
    assert "location = /var/cache/binhost/x86-64\n" in config
    assert (root / "etc/resolv.conf").read_text() == "unchanged\n"
    assert os.readlink(root / "etc/motd") == "/etc/issue"
    service_calls = [
        line for line in result.stdout.splitlines() if line.startswith("--root=")
    ]
    assert len(service_calls) == 2
    assert all(line.startswith(f"--root={root} enable ") for line in service_calls)
    assert "vde-switch.service" in result.stdout
    assert (
        "ip saddr 192.168.92.0/24" in (root / "etc/nftables/rules/main.nft").read_text()
    )
    assert not (root / "etc/nftables.conf").exists()
    assert "nftables" in result.stdout
    assert "dnsmasq" in result.stdout


@pytest.mark.parametrize("failure", ["none", "install", "postgres", "stop", "unmount"])
def test_development_build_cleanup(tmp_path: Path, failure: str) -> None:
    """Failures preserve resources; successful builds stop PostgreSQL before unmounting."""
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    log = tmp_path / "events"
    result = run_test_builder(
        r"""
export LOG=$1 FAILURE=$2 IMAGE_ROOT=$3
export ROOT=/incorrect SYSROOT=/incorrect PORTAGE_CONFIGROOT=/incorrect
check_build_prerequisites() {
    [[ ! ${ROOT+x} && ! ${SYSROOT+x} && ! ${PORTAGE_CONFIGROOT+x} ]]
}
prepare_build_mounts() { workspace=$IMAGE_ROOT/workspace; mounts=(root run); }
configure_target_caches() { :; }
install_development_packages() { [[ $FAILURE != install ]] || return 17; }
initialize_postgres() {
    postgres_active=true
    [[ $FAILURE != postgres ]] || return 19
    stop_postgres
}
chroot() { echo stop >> "$LOG"; [[ $FAILURE != stop ]] || return 23; }
unmount_tracked() {
    echo "unmount ${mounts[*]}" >> "$LOG"
    [[ $FAILURE != unmount ]] || return 29
}
configure_vde_service() { :; }
configure_vde_network() { :; }
configure_nat() { :; }
configure_dhcp() { :; }
configure_login_banner() { :; }
write_test_metadata() { :; }
finalize_image() { echo finalize >> "$LOG"; }
main
""",
        str(log),
        failure,
        str(tmp_path),
    )
    assert (
        result.returncode
        == {"none": 0, "install": 17, "postgres": 19, "stop": 23, "unmount": 1}[failure]
    ), result.stderr
    events = log.read_text().splitlines() if log.exists() else []
    assert ("unmount root run" in events) == (failure in {"none", "unmount"})
    if failure in {"none", "unmount"}:
        assert events == ["stop", "finalize", "unmount root run"]
    elif failure == "stop":
        assert events == ["stop"]
    else:
        assert events == []
    assert ("finalize" in events) == (failure in {"none", "unmount"})
    assert ("stop" in events) == (failure in {"none", "stop", "unmount"})
    assert workspace.exists() == (failure != "none")


def test_development_postgres_uses_individual_target_commands(tmp_path: Path) -> None:
    """Target configuration drops helper cache paths and uses only a private socket."""
    executable = tmp_path / "chroot"
    executable.write_text(
        "#!/bin/bash\n[[ ! ${ROOT+x} && ! ${SYSROOT+x} && ! ${PORTAGE_TMPDIR+x} "
        '&& ! ${DISTDIR+x} && ! ${PKGDIR+x} ]]\nchroot "$@"\n'
    )
    executable.chmod(0o755)
    result = run_test_builder(
        r"""
root=$1/root
export PATH="$1:$PATH"
pgbin=/usr/lib64/postgresql-18/bin
pgdata=/var/lib/postgresql/18/data
postgres_active=false
export ROOT=/helper SYSROOT=/helper PORTAGE_TMPDIR=/helper DISTDIR=/helper PKGDIR=/helper
chroot() {
    printf 'target %s\n' "$*"
    if [[ $* == *psql* ]]; then cat; fi
}
export -f chroot
systemctl() { printf 'service %s\n' "$*"; }
initialize_postgres
[[ $postgres_active == false ]]
""",
        str(tmp_path),
    )
    assert result.returncode == 0, result.stderr
    lines = result.stdout.splitlines()
    assert len([line for line in lines if line.startswith("target ")]) == 6
    assert f"target {tmp_path}/root emerge --config dev-db/postgresql:18" in lines
    assert f"service --root={tmp_path}/root enable postgresql-18.service" in lines
    assert (
        "listen_addresses='' -c unix_socket_directories=/run/postgresql"
        in result.stdout
    )
    assert "CREATE USER corvus SUPERUSER;" in lines
    assert "CREATE DATABASE corvus OWNER corvus;" in lines
    assert lines[-1].endswith("-m fast -w stop")


def test_development_workspace_allows_unprivileged_sync(tmp_path: Path) -> None:
    """Portage's sync user must traverse the helper workspace created by mktemp."""
    workspace = tmp_path / "workspace"
    workspace.mkdir(mode=0o700)
    result = run_test_builder(
        r"""
root=$1/root
expected_workspace=$1/workspace
mktemp() { printf '%s\n' "$expected_workspace"; }
mount_at() { :; }
prepare_workspace() { :; }
mount_target_disk() { :; }
bind() { :; }
configure_glibc_caches() { :; }
prepare_build_mounts
[[ $PORTAGE_TMPDIR == "$expected_workspace" ]]
""",
        str(tmp_path),
    )
    assert result.returncode == 0, result.stderr
    assert workspace.stat().st_mode & 0o777 == 0o755


def test_node_kernel_checks_artifact_not_helper(tmp_path: Path) -> None:
    """Offline validation follows the artifact symlink, regardless of helper uname."""
    definitions = (
        (ROOT / "scripts/build-test-node.sh").read_text().rsplit('main "$@"', 1)[0]
    )
    (tmp_path / "boot").mkdir()
    (tmp_path / "boot/vmlinuz").symlink_to("vmlinuz-6.12.1-gentoo")
    (tmp_path / "boot/vmlinuz-6.12.1-gentoo").touch()
    (tmp_path / "lib/modules/6.12.1-gentoo").mkdir(parents=True)
    config = tmp_path / "boot/config-6.12.1-gentoo"
    for contents, expected in [
        ("CONFIG_KVM_INTEL=m\n", 0),
        ("# CONFIG_KVM_INTEL is not set\n", 1),
    ]:
        config.write_text(contents)
        result = subprocess.run(
            [
                "bash",
                "-euc",
                definitions + "\nroot=$1\ncheck_node_kernel\n",
                "bash",
                str(tmp_path),
            ],
            env={**os.environ, "CORVUS_BUILD_LIB": str(ROOT / "scripts/lib")},
            capture_output=True,
            text=True,
        )
        assert result.returncode == expected, result.stderr


def test_node_configuration_preserves_runtime_policy(tmp_path: Path) -> None:
    """Only harness-independent services start; debug files retain runtime expansion."""
    definitions = (
        (ROOT / "scripts/build-test-node.sh").read_text().rsplit('main "$@"', 1)[0]
    )
    # Redirect only upload staging; all generated target paths stay beneath the fixture.
    staging = tmp_path / "staging/systemd"
    staging.mkdir(parents=True)
    (staging / "corvus.service").write_text("[Service]\nExecStart=/bin/true\n")
    definitions = definitions.replace("/tmp/corvus-test-node", str(staging.parent))
    root = tmp_path / "root"
    result = subprocess.run(
        [
            "bash",
            "-euc",
            definitions
            + """
root=$1
systemctl() { printf '%s\\n' "$*"; }
write_node_package_policy "$root"
install_node_services
configure_node_debugging
""",
            "bash",
            str(root),
        ],
        env={**os.environ, "CORVUS_BUILD_LIB": str(ROOT / "scripts/lib")},
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    assert (
        "disable corvus.service corvus-netd.service corvus-nodeagent.service"
        in result.stdout
    )
    assert "corvus-tcp-relay.service sshd.service" in result.stdout
    assert (
        "pulseaudio sound-server"
        in (root / "etc/portage/package.use/corvus").read_text()
    )
    assert not (root / "etc/portage/binrepos.conf/zz-corvus-cache.conf").exists()
    profile = root / "etc/profile.d/50-corvus.sh"
    assert "$(/opt/corvus/bin/crv completion bash" in profile.read_text()
    assert subprocess.run(["bash", "-n", str(profile)], check=False).returncode == 0


@pytest.mark.parametrize("profile", ["", "test-vm"])
def test_runtime_cleanup_is_profile_scoped(tmp_path: Path, profile: str) -> None:
    """Only the immutable test guest loses package records and non-English catalogs."""
    for directory in (
        "var/db/pkg/sys-libs/musl-1.2.5",
        "var/lib/portage",
        "usr/share/locale/en",
        "usr/share/locale/en_GB",
        "usr/share/locale/en@shaw",
        "usr/share/locale/fr",
        "usr/share/locale/de",
        "usr/lib/locale/C.UTF-8",
    ):
        path = tmp_path / directory
        path.mkdir(parents=True)
        (path / "sentinel").touch()
    result = subprocess.run(
        [
            "bash",
            "-euc",
            '. "$1"; clean_image_temporary_files() { :; }; '
            "reset_image_identity() { :; }; finalize_image() { :; }; finish_runtime",
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
        ],
        env={
            **os.environ,
            "IMAGE_LIBC": "musl",
            "IMAGE_ROOT": str(tmp_path),
            "IMAGE_PROFILE": profile,
            "CORVUS_BUILD_NAME": "test",
        },
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    keep_metadata = profile != "test-vm"
    assert (tmp_path / "var/db/pkg").exists() == keep_metadata
    assert (tmp_path / "var/lib/portage").exists() == keep_metadata
    for language in ("en", "en_GB", "en@shaw"):
        assert (tmp_path / "usr/share/locale" / language / "sentinel").exists()
    for language in ("fr", "de"):
        assert (tmp_path / "usr/share/locale" / language).exists() == keep_metadata
    assert (tmp_path / "usr/lib/locale/C.UTF-8/sentinel").exists()


@pytest.mark.parametrize(
    "package", ["sys-libs/glibc", "app-admin/sudo", "dev-lang/python"]
)
def test_test_vm_verifies_packages_before_removing_records(
    tmp_path: Path, package: str
) -> None:
    """Forbidden dependencies fail the bake while their package records still exist."""
    record = tmp_path / "var/db/pkg" / f"{package}-1.0"
    record.mkdir(parents=True)
    result = subprocess.run(
        [
            "bash",
            "-euc",
            '. "$1"; finish_runtime',
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
        ],
        env={
            **os.environ,
            "IMAGE_LIBC": "musl",
            "IMAGE_ROOT": str(tmp_path),
            "IMAGE_PROFILE": "test-vm",
            "CORVUS_BUILD_NAME": "test",
        },
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0
    assert f"Unexpected runtime package: {package}" in result.stderr
    assert record.is_dir()


@pytest.mark.parametrize(
    "entry",
    [
        "build-runtime.sh",
        "build-cloud-init.sh",
        "build-musl-builder.sh",
        "build-test-offline.sh",
        "build-test-node.sh",
    ],
)
@pytest.mark.parametrize("signal,status", [("HUP", 129), ("INT", 130), ("TERM", 143)])
def test_build_interrupt_preserves_resources(
    tmp_path: Path, entry: str, signal: str, status: int
) -> None:
    """Real entrypoint traps preserve mounts, active PostgreSQL, and scratch data."""
    library = tmp_path / "lib"
    library.mkdir()
    workspace = tmp_path / "workspace"
    workspace.mkdir()
    (workspace / "sentinel").touch()
    log = tmp_path / "cleanup"
    (library / "build.sh").write_text(
        (ROOT / "scripts/lib/build.sh").read_text()
        + r"""
initialize_mount_tracking() { mounts=(root run); }
interrupt_build() {
    workspace=$WORKSPACE
    postgres_active=true
    kill -s "$SIGNAL" "$$"
    exit 99
}
prepare_workspace() { interrupt_build; }
check_build_prerequisites() { interrupt_build; }
stop_postgres() { echo stop >> "$LOG"; }
umount() { echo unmount >> "$LOG"; }
cloud-init() { :; }
export -f cloud-init
"""
    )
    result = subprocess.run(
        ["bash", str(ROOT / "scripts" / entry)],
        env={
            **os.environ,
            "CORVUS_BUILD_LIB": str(library),
            "IMAGE_ROOT": "/unused",
            "TARGET_DISK": "/unused",
            "IMAGE_INIT": "systemd",
            "SIGNAL": signal,
            "WORKSPACE": str(workspace),
            "LOG": str(log),
        },
        capture_output=True,
        text=True,
    )
    assert result.returncode == status, result.stderr
    assert not log.exists()
    assert (workspace / "sentinel").exists()


@pytest.mark.parametrize("propagation_status", [0, 9])
def test_bind_isolates_mount_events_and_tracks_before_propagation(
    tmp_path: Path, propagation_status: int
) -> None:
    """Even a bind whose propagation setup fails remains tracked for inspection."""
    result = subprocess.run(
        [
            "bash",
            "-euc",
            r"""
. "$1"
initialize_mount_tracking
mount() {
    printf '%s\n' "$*"
    if [[ $1 == --make-rprivate ]]; then
        [[ ${mounts[*]} == "$2" ]]
        return "$PROPAGATION_STATUS"
    fi
}
bind /source "$2/target"
""",
            "bash",
            str(ROOT / "scripts/lib/build.sh"),
            str(tmp_path),
        ],
        env={**os.environ, "PROPAGATION_STATUS": str(propagation_status)},
        capture_output=True,
        text=True,
    )
    assert result.returncode == propagation_status, result.stderr
    assert result.stdout.splitlines() == [
        f"--bind /source {tmp_path}/target",
        f"--make-rprivate {tmp_path}/target",
    ]


def test_all_gentoo_builds_preserve_helper_and_use_visible_mounts() -> None:
    """Every build keeps its failed helper and runs without a private namespace."""
    for recipe in ROOT.glob("*.yml"):
        for step in yaml.safe_load(recipe.read_text())["pipeline"]:
            if "build" not in step:
                continue
            build = step["build"]
            assert build["cleanup"] == "onSuccess", recipe
            for provisioner in build.get("provisioners", []):
                if "shell" in provisioner:
                    assert "unshare" not in provisioner["shell"]["inline"], recipe
