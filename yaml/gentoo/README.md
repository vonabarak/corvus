# Gentoo image development

## Image and recipe names

Ordinary images use `gentoo-<libc>-<init>-<role>[-cloudinit]`.
Use `glibc` or `musl`, `systemd` or `openrc`, and the role `upstream`,
`builder`, or `headless`. The optional `cloudinit` suffix means cloud-init
is installed. Headless describes the guest; builder describes a reusable
compilation host. Use the primary output's full name for the YAML filename.
Keep a runtime and its cloud-init variant in the same recipe.

The purpose-specific names `gentoo-corvus-test`, `corvus-test-node`, and
`corvus-test-vm` are exceptions. Their recipe filenames match their names.
Templates use the image name, adding `-bootstrap`, `-bake`, or `-uefi` for
that template's purpose. Do not add compatibility aliases for renamed images.
Existing published resources are not renamed or removed by these recipes.

## Script ownership

Every locally built image has exactly one public entry point:
`scripts/images/<published-image>/build.sh`. Keep exclusive stages and helper
libraries beside that entry point. Shared orchestration belongs in
`scripts/workflows/`; shared executable steps belong in `scripts/stages/`,
with libc-specific steps below `glibc/` or `musl/`. Shared functions belong
in small responsibility-based modules in `scripts/lib/`.
Keep related stage assets together: kernel compilation and its configuration live
in `scripts/stages/kernel/`. Image-specific assets belong beside their entry
point, such as `scripts/images/corvus-test-node/systemd/`.

| Image | Entry point below `scripts/images/` | Shared workflow |
| --- | --- | --- |
| gentoo-glibc-systemd-builder-cloudinit | gentoo-glibc-systemd-builder-cloudinit/build.sh | none |
| gentoo-musl-openrc-builder | gentoo-musl-openrc-builder/build.sh | none |
| gentoo-glibc-systemd-headless | gentoo-glibc-systemd-headless/build.sh | build-runtime.sh |
| gentoo-glibc-systemd-headless-cloudinit | gentoo-glibc-systemd-headless-cloudinit/build.sh | build-cloud-init.sh |
| gentoo-musl-openrc-headless | gentoo-musl-openrc-headless/build.sh | build-runtime.sh |
| gentoo-musl-openrc-headless-cloudinit | gentoo-musl-openrc-headless-cloudinit/build.sh | build-cloud-init.sh |
| gentoo-corvus-test | gentoo-corvus-test/build.sh | none |
| corvus-test-node | corvus-test-node/build.sh | none |
| corvus-test-vm | corvus-test-vm/build.sh | none |

The upstream image is imported and has no shell entry point. Pre-guest-agent
bootstrap commands remain inline in the builder recipe.

Executable scripts define `main` and private descriptive functions. Thin entry
points may `exec` a shared workflow. Shared functions use their module prefix
(`portage_`, `mounts_`, `image_`, etc.); the nested guest's private library uses
`test_vm_`. Libraries only declare functions and source dependencies: sourcing
must not initialize variables, change options, install traps, or run commands.
Call initializers explicitly before using state. Keep imports explicit.

`CORVUS_BUILD_SCRIPTS` overrides the script root, defaulting to
`/usr/local/lib/corvus-build`. Preserve the source hierarchy there. YAML uploads
individual files, including all transitive library and subprocess dependencies.
Use mode `0755` for executable stages and `0644` for libraries. Invoke the
image's entry point from YAML. Bootstrap copies the needed hierarchy into the
musl chroot and clears the root override there.

## Dependencies and package ownership

The import recipe alone owns the upstream cloud image. The glibc builder
consumes it; the glibc runtime and musl builder consume the glibc builder.
The musl runtime and nested test guest consume the musl builder.
The musl builder is internal: keep it out of the top-level image list and
aggregate image lists; consumer dependencies prepare it automatically.
Development and node artifacts consume the glibc runtime while building on the glibc
builder. Development uses its cloud-init runtime variant as the artifact source.
Make prepares these registered dependencies and propagates image policy;
`ensure` only fills missing images. Keep import declarations out of consumers.

Install reusable native tools in builders: disk/filesystem/GRUB tools,
curl/tar/xz, and Python build backends `gpep517` and `setuptools`. Git is an
explicit glibc builder dependency used by development and node builds. GLib's
schema compiler belongs in the musl builder. Consumers check these prerequisites;
they do not reinstall them. Keep target Git and other target dependencies where
required. Let Portage resolve incidental build dependencies such as `installer`.
Kernel sources remain selected on kernel-cache misses. PostgreSQL, node services,
and other image-specific dependencies belong to their target recipes.

The helper owns native compilation. Use target `ROOT` installs without moving
whole workflows into chroots; only bootstrap and individual target commands
enter a chroot. Keep glibc and musl package caches separate. Preserve the shared
distfile/binhost caches and kernel-cache inputs. Refresh repositories before
selection. Configure the build host's Portage cache policy before target `ROOT`
installs: native build dependencies ignore environment `FEATURES` overrides.
Keep this policy out of the resulting guest's configuration.
Initialize mount tracking before traps; clean up in reverse order
on success, and preserve resources on failure or interruption for inspection.

Ordinary glibc and musl images install `@system` and the shared extra package set.
The nested test guest owns its minimal package list, stripping, and forbidden-package
checks. Finalization cleans and verifies persisted build residue through a private,
non-recursive bind view; transient mounts and their shared contents stay untouched.
Only the two builders pass `--keep-repositories` to finalization. See
[image cleanup policy](../../doc/test-images.md#gentoo-image-cleanup) for checked paths.

## Changing a build

1. Choose the owning image, stage, or library; keep exclusive behavior local.
2. Update entry points, uploaded dependency closures, Make dependencies, and
   all image/template consumers together when names or ownership change.
3. Update this inventory and the focused Gentoo recipe tests.
4. Check shell syntax recursively, run the Gentoo/image-recipe tests and
   `make yaml-schema-check`. Follow the root agent guide for Python formatting,
   checks, and repository tests. Validate changed images with real builds when
   the daemon, cache, and VM capacity are available.

See [image build workflows](../../doc/test-images.md) for operational details.
