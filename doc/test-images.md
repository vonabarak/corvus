# Image builds

Every image recipe owns a Makefile in its `yaml/` directory. The root Makefile
only forwards image commands to `yaml/Makefile`. Build every managed image
with `make images`, remove local intermediates with `make images-clean`, or publish new versions with
`make images-rebuild`. The commands below select one image:

```sh
make image-list
make image IMAGE=corvus-test-node
make image-ensure IMAGE=corvus-test-node
make image-check IMAGE=corvus-test-node
make image-clean IMAGE=corvus-test-node
make image-rebuild IMAGE=corvus-test-node
make image-cache-clean IMAGE=installer
```

Normal builds of managed images use conditional `update`. Imports reuse
verified source identities when upstream checksums match. Derived images skip
when the recipe and resolved source versions match their stored metadata;
changed imports or rebuilt dependencies cause their consumers to rebuild.
`image-rebuild` always uses `overwrite`.
Existing versions, backing files, overlays, VMs, and templates remain usable.
Unqualified image selectors resolve the current `latest`; templates are updated
in place so subsequent VM creation uses it. Already-created disks keep their
original backing version.

Converted YAML recipes default `image_if_exists` to `update`; Make passes
`skip` for ensure operations. Uploaded installation and answer media reuse
verified SHA-256 identities when the file bytes and disk format match. The Python build
API expands declared defaults before submission too. For recipes with required
variables, set their values in the YAML `vars` mapping before calling the Python
API.

`image-ensure` checks the declared disks and templates, creates missing outputs
with `skip`, and reuses existing versions. Ensure operations use this target
for dependencies too. Normal builds refresh the upstream chain with `update`;
forced builds propagate `overwrite`. Aggregate builds prepare shared dependencies
once before evaluating their consumers.
`images-ensure` ensures all recipes.
Operational errors stop the build; only a missing-resource response triggers
publication. `image-check` checks every declared output without publishing.

`image-clean` removes only local `build/` intermediates. `image-cache-clean`
removes reusable download caches where declared. Published resources, runtime
VMs, writable disks, SSH keys, and caches survive ordinary cleanup.

All Gentoo recipes share `yaml/gentoo/` and its Makefile, kernel configuration,
and `scripts/` provisioners. The Corvus development image builds from
`gentoo-glibc-systemd-headless`. The default `build` target runs the full chain. Manage the stages
with `IMAGE` (default: `gentoo-corvus-test`):

```sh
make -C yaml/gentoo build IMAGE=gentoo-glibc-systemd-upstream-cloudinit
make -C yaml/gentoo build IMAGE=gentoo-glibc-systemd-builder-cloudinit
make -C yaml/gentoo build IMAGE=gentoo-musl-openrc-builder
make -C yaml/gentoo build IMAGE=gentoo-glibc-systemd-headless
make -C yaml/gentoo build IMAGE=gentoo-corvus-test
make image IMAGE=gentoo-glibc-systemd-headless
make image IMAGE=gentoo-corvus-test
```

Use the same selector with `ensure`, `publish`, `rebuild`, `check`, `clean`,
and `cache-clean`. Each selector covers the complete recipe and its templates;
headless and musl recipes include both base and cloud-init variants.
`publish` submits only the selected recipe; `build` refreshes dependencies first.

Ordinary Gentoo builds use `ifExists: update`. `build IMAGE=gentoo-glibc-systemd-upstream-cloudinit`
fetches the current upstream manifest and the SHA256 for its selected filename.
An unchanged checksum reuses the imported image without downloading it again.
`build IMAGE=gentoo-glibc-systemd-headless` checks that import first, then evaluates the
builder, headless, and headless cloud-init builds in order.
`build IMAGE=gentoo-corvus-test` evaluates that whole chain before the
development image. Each build skips when its stored metadata matches the
current recipe and resolved source versions; a changed source or rebuilt
intermediate causes dependent builds to rebuild. See
[build metadata comparison](image-builds.md#how-update-compares-metadata).

The Gentoo pipelines use official Gentoo mirrors and a writable
virtiofs cache; the build host does not need Gentoo or a Portage tree. The
`gentoo-glibc-systemd-upstream-cloudinit.yml` recipe owns the upstream import, tagged with the timestamp
in Gentoo's filename. `gentoo-glibc-systemd-builder-cloudinit.yml` prepares `gentoo-glibc-systemd-builder-cloudinit` and the
common `gentoo-glibc-systemd-builder-cloudinit-bake` template, then `gentoo-glibc-systemd-headless.yml` builds
`gentoo-glibc-systemd-headless` from scratch, then adds cloud-init with `offline-overlay`
using that same external builder.
The base runtime is never booted as a cloud-init build host.
Both headless image flavors support BIOS and UEFI boot. The
`gentoo-glibc-systemd-headless-uefi` and `gentoo-glibc-systemd-headless-cloudinit-uefi` templates share the
same OS images as their respective BIOS templates and use independent clones
of the registered `ovmf-vars` firmware image alongside read-only `ovmf-code`.
Each OS disk uses GPT: partition 1 is a 1 MiB BIOS boot partition, partition 2
is a 512 MiB FAT32 EFI System Partition mounted at `/boot/efi`, and partition 3
is the ext4 root filesystem occupying the remaining space. Cloud-init grows
partition 3 and its filesystem when the virtual disk is enlarged. GRUB is
installed for both firmware types, with an EFI fallback executable that does
not require preconfigured firmware boot entries.

Rebuild previously published headless images and their development/test-node
derivatives before using this layout; existing images with root on partition 2
are not converted. Use `make image-rebuild IMAGE=gentoo-corvus-test` for the headless and
development chain, then `make image-rebuild IMAGE=corvus-test-node` for the test node.

### Gentoo image cleanup

Every locally built Gentoo image cleans and verifies build residue before
publication. The check inspects the root filesystem through a private,
non-recursive bind mount, exposing stored files beneath transient cache,
repository, and tmpfs mounts without touching their shared contents.
Upstream imports are unchanged. Both reusable builders retain `/var/db/repos`;
all other built images must leave it empty.

The checked directories are `/var/cache/{binpkgs,binhost,distfiles}`, `/var/db/repos`,
`/tmp`, `/var/tmp` (including Portage build scratch), `/var/log/portage`,
`/var/lib/portage/{elog,failed,logs}`, and `/usr/local/lib/corvus-build`.
They must be absent or contain only empty directory trees. `/var/log/emerge*`
entries must be absent, including zero-byte log files. Cleanup does not follow
symlinks; unsafe ancestor symlinks fail finalization. Remaining files are listed
and prevent publication. Installed package records, Portage configuration and
world files, application data, and unrelated runtime logs are preserved.

Runtime builds also reset clone identity and write image metadata. All builds
flush writes and trim the root and mounted EFI filesystems before publication.
Unsupported discard emits a warning and does not fail the build; image
compaction remains enabled separately. Offline glibc and musl builds
run in the helper VM's main mount namespace. Successful builds release tracked
mounts in reverse order; failed builds and HUP/INT/TERM interruptions preserve
existing mounts, workspaces, and any active build PostgreSQL process for debugging.
Bind destinations use private propagation so target mount events do not propagate
back to their source mounts. All Gentoo build recipes use `cleanup: onSuccess`,
retaining the helper VM and artifact disk on failure.

Connect to the retained helper with `ssh corvus@vsock%<CID>` (find its CID with
`crv vm show <ID>`). Inspect the artifact with `sudo findmnt -R /mnt/sysroot`,
all build mounts with `sudo findmnt`, and attached partitions with `lsblk -f`.
Mounts remain visible after the provisioning shell exits, until manual unmount
or VM shutdown. Mounts already detached by a completed build phase are not
recreated after a later failure. Cleanup failures can leave partially released
resources; inspect the remaining mounts before removing any workspace. Use a
fresh helper for another build rather than rerunning over preserved mounts.

Each image has a dedicated entry point under `scripts/images/<image>/`.
Shared workflows, stages, and responsibility-based libraries live under
`scripts/workflows/`, `scripts/stages/`, and `scripts/lib/` respectively.
See the [Gentoo development rules](../yaml/gentoo/README.md) for naming,
script ownership, package ownership, and the entry-point inventory.

Both runtimes refresh repository metadata before package selection and use UTC,
localhost defaults, locked root passwords, and fresh machine/SSH identities.
Cloud-init variants share NoCloud, the default user, and root growth settings.
All four end-user cloud-init templates inject the `corvus` SSH key and run
`passwd -d root` on first boot, enabling passwordless root console login. This
does not change sshd authentication settings; baked images still lock root.
Builder and standalone development user-data retain their separate policies.
Locale handling, service setup, and package sets remain specific to each image.
Documentation/development-file stripping and forbidden-package checks belong only
to the immutable `corvus-test-vm` guest.

The independent `gentoo-musl-openrc-headless.yml` pipeline builds a complete musl/OpenRC system:

```sh
make image IMAGE=gentoo-musl-openrc-headless
make image-ensure IMAGE=gentoo-musl-openrc-headless
make image-rebuild IMAGE=gentoo-musl-openrc-headless
# Equivalent entry point:
make -C yaml/gentoo build IMAGE=gentoo-musl-openrc-headless
```

It consumes the native `gentoo-musl-openrc-builder-bake` template. Its dedicated
`gentoo-musl-openrc-builder.yml` recipe bootstraps a persistent musl/OpenRC image from
the official stage3 using the glibc builder. This one-time bootstrap enters
the new disk with chroot to install Portage, GCC, binutils,
GRUB, disk tools, networking, and QGA. The resulting builder boots independently
with 8 CPUs, 8 GiB RAM, and a 40 GiB disk.
The dependency chain is `gentoo-glibc-systemd-upstream-cloudinit → gentoo-glibc-systemd-builder-cloudinit → gentoo-musl-openrc-builder →
gentoo-musl-openrc-headless`; the headless and development images are not required.
`ensure IMAGE=gentoo-musl-openrc-builder` checks presence only;
`build IMAGE=gentoo-musl-openrc-builder` propagates the selected image policy.

Shared shell libraries in `scripts/lib/` implement cache mounts, glibc cache
configuration, disk layout, GRUB, and image metadata. Entry scripts own their
mount lifetimes and flavor-specific package and service choices. Bootstrap
cloud-init contains only commands needed before QGA can upload scripts; no
cloud-init `write_files` support is required. The Makefile resolves
an official amd64 musl/OpenRC stage3 and its SHA256 only when publishing the
builder recipe. Both runtime steps boot the reusable musl builder and compile
natively with `ROOT=/mnt/sysroot` and `SYSROOT` unset: build dependencies stay
on the builder. Both ordinary runtime flavors install `@system`, including
Portage and the toolchain, and retain documentation, headers, static libraries,
and sources. Their extra package set matches glibc: QGA, msmtp, sudo, growpart,
dosfstools, efibootmgr, nfs-utils, thin-provisioning-tools, Vim, and bash-completion.
The target retains its musl/OpenRC profile, binhost, and guest-local cache paths.
Run `emerge --sync` after boot to populate its empty repository directory.
Distfiles share the existing cache; musl binary packages live separately under
`packages/x86-64_musl`, and never use the glibc binary repository.
Bootstrap and native musl builds use the official `23.0/x86-64_musl` binhost,
with downloads cached in `binhost/x86-64_musl`. Portage uses compatible binary
packages and compiles from source when none match. The minimal USE flags remain
unchanged, so packages such as OpenRC with `-netifrc` may still require compilation.
Rebuild `gentoo-musl-openrc-builder` before building derivatives to apply this policy.

`gentoo-musl-openrc-headless` contains OpenRC/sysvinit, DHCP networking, DNS and CA support,
OpenSSH, a system logger, and QEMU guest agent. Root starts locked and there is
no preset SSH credential; provision users and keys through the guest agent or
use the cloud-init flavor. `gentoo-musl-openrc-headless-cloudinit` adds NoCloud
cloud-init in an offline overlay. Its distribution default user is
`gentoo`; Corvus-generated user-data can override this. Inject SSH keys through
NoCloud or the template. Networking stays under dhcpcd
(DHCP); cloud-init network rendering is disabled because it has no dhcpcd
renderer. Both flavors retain package records, licenses, documentation, and
development files, clean build residue, and use the same kernel configuration
and dual BIOS/UEFI GPT layout as the glibc image. Their BIOS templates use the
image names; UEFI templates add
`-uefi`. Runtime defaults are 1 vCPU, 512 MiB RAM, and a 4 GiB virtual disk.
The cloud-init flavor grows root partition 3 when the disk is enlarged. musl
provides UTF-8 without glibc's `locale-gen`; the locale-generation module is
disabled. The Gentoo cloud-init backend calls OpenRC's `rc-service` directly.

To compare footprint, build current `gentoo-glibc-systemd-headless` and `gentoo-musl-openrc-headless` with the
same kernel, boot both at 512 MiB with identical disks and no user workload,
and wait for startup services to settle. Record allocated image bytes on the
storage node (`du -B1`), root filesystem usage (`df -B1 /`), and
`MemTotal - MemAvailable` from `/proc/meminfo`; repeat for both cloud-init
flavors after `cloud-init status --wait`. Virtual disk capacity alone does not
measure image footprint. Check BIOS and UEFI boot, guest-agent access, DHCP/DNS,
SSH key provisioning, and cloud-init disk growth before using the images.
Build-time RAM remains 8 GiB, separate from the runtime target.

A local smoke test on 2026-10-30, before musl installed `@system` and the shared
extras, recorded these settled base-image measurements using 1 vCPU, 512 MiB RAM,
and kernel `6.18.57-gentoo-corvus`:

| Image | Allocated qcow2 | Root filesystem used | RAM used |
|---|---:|---:|---:|
| `gentoo-glibc-systemd-headless` | 1453.0 MiB | 1303.2 MiB | 58.6 MiB |
| `gentoo-musl-openrc-headless` | 358.0 MiB | 236.7 MiB | 48.1 MiB |

These historical observations do not describe the current musl package policy
and are not size limits. The existing regular image had a
20 GiB virtual disk and musl had 4 GiB, so filesystem metadata also differs.

Small provisioning steps live inline; larger steps remain in `scripts/`.
Expanded script contents participate in build metadata.

Provision `/data/corvus-cache` on the execution node with permissions allowing
VM root to write through virtiofs. When the node agent runs as an ordinary user,
that host user must be able to create directories and files in each cache
subdirectory too. A root-owned `packages/x86-64` directory with mode `0755` prevents
binary package writes in this setup. Repair its ownership on the execution node,
for example `sudo chown -R <node-agent-user>:<node-agent-group> /data/corvus-cache/packages/x86-64`.
The recipes check directory creation and file writes before starting Portage.
Binary package directories follow the variant names in Gentoo's
[amd64 23.0 binpackage layout](https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/).
Portage manages the package category directories and indexes inside each variant:

```text
/mnt/corvus-cache/
├── packages/
│   ├── x86-64/      # Locally built glibc binary packages
│   └── x86-64_musl/ # Locally built musl binary packages
├── binhost/
│   ├── x86-64/      # Downloaded official glibc binary packages
│   └── x86-64_musl/ # Downloaded official musl binary packages
├── distfiles/       # Shared source downloads and bootstrap stage archives
└── kernels/         # Compiled kernel archives
```

Directories are created as needed; there is no automatic migration. Move existing
`x86-64/` and `x86-64_musl/` directories into `packages/` to reuse locally compiled
packages. For older layouts, move `binpkgs` to `packages/x86-64`, `musl/binpkgs`
to `packages/x86-64_musl`, and `binhost/binhost` to `binhost/x86-64`.
Existing `binhost/x86-64`, `distfiles`, and `kernels` caches remain valid.
Override the host path throughout the dependency chain with:

```sh
make image IMAGE=gentoo-corvus-test GENTOO_CACHE_DIR=/srv/corvus-cache
```

For direct CLI builds, pass `--var cache_dir=/srv/corvus-cache` to each consuming
recipe. Distfiles, downloaded official binaries, and locally compiled packages
are cached separately inside this directory. Every package installation uses
`--buildpkg`; `emerge-webrsync -k` also keeps repository snapshots and their
verification files in the distfiles cache. Subsequent builds reuse cached files
while checking for current snapshots. Built kernels and modules are stored under
`kernels/` as `kernel-<ebuild-version>-<config-sha256>.tar.gz`. The shared kernel
script restores matching archives into the target instead of compiling again;
the latest stable amd64 ebuild in the local repository selects the source version.
Sources are installed only on a cache miss. Ebuild revisions and the supplied
kernel configuration are part of the cache key; changes produce a new archive.
Neither reusable builder preinstalls kernel sources. Archives also include the
matching `System.map` and kernel config and are published atomically after a successful build. Delete an archive to force
recompilation with the same sources and configuration.
The first build can take substantially longer than subsequent builds. Bake
commands disable Portage’s `userfetch` and `userpriv` features: downloads and
ebuild phases run as guest root, including live-package Git fetches during
`src_unpack`. Root can write through the share even when the host cannot map
the guest `portage` UID. These features are applied to each package installation, including native build
dependencies. Portage discards environment `FEATURES` overrides for native build
dependencies when `ROOT` points at another root, so the build host's `make.conf`
also disables `userfetch`, `userpriv`, and implicit `getbinpkg`, and sets
`-binpkg-request-signature binpkg-ignore-signature` so signature failures do not
reject native dependencies. The installation wrapper applies the same flags to
target packages; `verify-signature = false` in binhost configuration alone does
not cover every package in the local cache. Portage still invokes GnuPG to decode
clear-signed Manifests and can print unknown-key errors even though these failures
are ignored and installation succeeds. Both builders install `app-portage/getuto`
when needed and run `getuto` during their build to initialize the Gentoo keyring.
The keyring persists in `/etc/portage/gnupg` for derivative builds, avoiding these
messages without enabling signature enforcement. Existing musl builders must be
rebuilt to acquire the keyring. This policy is
not written into the resulting guest's configuration. Managed configurations remove
inherited `FEATURES` and `EMERGE_DEFAULT_OPTS` assignments before applying the
build host policy. Build environments explicitly set `DISTDIR` and `PKGDIR` to
`/mnt/corvus-cache/distfiles` and `/mnt/corvus-cache/packages/x86-64`, so selecting another
ROOT or SYSROOT does not redirect the caches. The from-scratch target's
distfiles and package cache directories stay empty during the build; those
shared caches are not mounted inside SYSROOT.

Gentoo recipes accept `USEPKG` and `GETBINPKG`, both defaulting to `true`.
Pass them as Make variables or YAML build variables (`--var USEPKG=false`).
For a build that installs packages from source, run:

```sh
make -C yaml/gentoo build IMAGE=gentoo-corvus-test USEPKG=false GETBINPKG=false
```

The controls propagate through dependency builds and musl bootstrap installations.
`USEPKG` enables cached binary packages; `GETBINPKG` enables binhost downloads
and, following Portage semantics, also enables binary package use. Disable both
to select sources. Values must be `true` or `false`. Every installation still
uses `--buildpkg` to cache compiled packages. Already installed packages and
cached kernel archives are reused; these controls do not force reinstallation.
The live Corvus ebuild and pycapnp keep their explicit source-build exceptions.
The builder's cloud-init first-start guest-agent installation always passes
`--usepkg --getbinpkg` explicitly, regardless of these controls; its later build
provisioning honors them.

The intermediate builder retains `/var/db/repos` on its filesystem so the
from-scratch build can use it. The new headless root has an empty repository
directory. Cloud-init builds mount that directory as tmpfs; development builds
sync repositories and overlays on helper tmpfs and bind it into the artifact, so their
published disks contain no Portage tree. The bake host exposes caches at
`/mnt/corvus-cache` and temporary `/var/cache` bind mounts. The development VM also receives the cache share, with an optional
systemd automount at `/mnt/corvus-cache`. Its Portage configuration points directly
to `distfiles`, `packages/x86-64`, and `binhost/x86-64` inside that share.
The development guest disables `userfetch` so downloads run as root and avoid
ownership changes for the guest `portage` user on the host cache.
Other runtime templates do not attach caches.
The external host cache survives both `clean` and `cache-clean`.

The development recipe uses `offline-overlay` with `gentoo-glibc-systemd-headless-cloudinit`
as its artifact source. A separate 40 GB helper boots the shared glibc builder and waits
for Cloud-init to grow its filesystem. Package installation runs natively on the helper with `ROOT=/mnt/sysroot` and
`--root-deps`, retaining development/build dependencies in the artifact as well
as on the helper. Compilation uses a temporary workspace on the helper's
`/var/tmp`; the artifact keeps the base image's size (currently 20 GB).
The artifact never boots during provisioning, preserving Cloud-init first-boot
state. One helper-side script orchestrates the build; only individual target
commands (PostgreSQL initialization and installed-version queries) use chroot.
PostgreSQL runs temporarily with a local-only socket on the artifact’s private
`/run` and stops before mounts are released. Runtime services are enabled with
`systemctl --root` for the first real boot.

Development images contain no Portage repositories. After boot, run
`sudo emerge --sync` once before testing package reinstalls or changing USE flags;
the synchronized repositories then persist on the VM's writable disk. Distfiles,
local binary packages, and downloaded binhost packages reuse the writable host
cache. For example, `sudo emerge --ask --oneshot app-emulation/corvus` reinstalls
Corvus using the configured caches. Changing USE flags can require recompilation.
The final apply keeps `ifExists: skip`: an existing `gentoo-corvus-test` VM is
preserved. Remove and recreate it explicitly to adopt a rebuilt artifact and
its shared-directory configuration.

Published systems retain the systemd no-multilib profile, official binhost,
runtime NFS tools, and UTF-8 locales. Cloud-init uses `locale: false` because
localedef generates the locales during the bake.

`make -C yaml/gentoo ensure IMAGE=<name>` fills missing outputs and dependencies
with `skip`. When all outputs exist, it performs no upstream downloads or
publication. Node-image ensure operations use this presence-only path; ordinary
node builds evaluate the headless chain first.

To force the chain, use `make image-rebuild IMAGE=gentoo-glibc-systemd-headless`; use
`IMAGE=gentoo-corvus-test` to force the development image and its upstream chain too.
Forced builds publish fresh versions and retain old ones. Build metadata does
not detect changes inside external Portage repositories, mirrors, or shared
directories; force a rebuild to pick up those
changes when the declared inputs and upstream image checksum are unchanged.

## Import ownership

Managed image builds use dedicated import recipes, with one owning declaration
for each source image or installation medium. Keep its download URL, checksum,
storage settings, and import policy there. Derivative build YAML consumes the
registered image; its Makefile prepares dependencies before invoking the build
and propagates the selected image policy. Presence-only `ensure` fills missing
outputs, while update and forced builds evaluate or refresh upstream images.

This gives shared inputs one source of truth and avoids drift between consumer
recipes. Importing inside each consumer would allow one CLI invocation and one
daemon task hierarchy for the full operation, but would repeat source settings
across consumers. With dedicated recipes, Make orders separate daemon tasks;
successful upstream publications remain available if a downstream command fails.

The multi-OS library owns all five cloud-image imports and their templates.
VM, nginx, and monitor pipelines consume those registered images through their
existing templates. Their Make build targets resolve current upstream checksums
before submitting the derivative. Existing source versions are retained when
checksums match; the import URL alone does not trigger an update.

Conditional builds do not detect changes in external package repositories or
host-mounted directories. Use `image-rebuild` when those undeclared inputs
must be refreshed. Direct CLI invocation requires preparing dependencies first.

For direct Gentoo CLI use, run `make -C yaml/gentoo build IMAGE=gentoo-glibc-systemd-builder-cloudinit` before
invoking the headless YAML. Its Make build targets prepare the source
automatically, including with parallel Make.

The shared VirtIO-Win medium belongs to `yaml/virtio-win/`; the Windows Server
installation ISO belongs to `yaml/windows-server-media/`. Both Windows recipes
prepare those dependencies through Make. Windows 11 installation media remains
a manual prerequisite. For direct CLI use, prepare the required import owners
and local answer ISO first.

The AlmaLinux DVD belongs to `yaml/almalinux-10-media/`. Its desktop consumer
uses a separate `OEMDRV` answer ISO containing the predefined Kickstart file.

Windows and AlmaLinux answer media and the synthetic installer ISO are persistent
uploads with `update`. Make regenerates each local ISO only when its declared files,
Makefile, generation script, or selected packages change. This preserves the
exact ISO bytes across unchanged runs, including their embedded timestamps;
it does not promise reproducible output after regeneration. The synthetic
installer script only creates a local ISO; its Makefile owns publication.
Unchanged uploaded media retain their image IDs, so dependent builds skip when
all their other metadata matches. Changed media publish new IDs and rebuild
consumers. `image-rebuild` uses `overwrite` for imports, uploads, and builds;
`image-ensure` checks presence without regenerating media that already exists.

## Recipe catalogue


| Image | Directory | Artifact and use |
| --- | --- | --- |
| `corvus-test-node` | `yaml/gentoo/` | `corvus-test-node`, the Gentoo outer node used by every topology. It builds or ensures `gentoo-glibc-systemd-headless` from `yaml/gentoo/` when needed. |
| `corvus-test-vm` | `yaml/gentoo/` (`IMAGE=corvus-test-vm`) | `BaseImages/Gentoo/<id>-corvus-test-vm.qcow2`, the inner musl/OpenRC VM used by Linux lifecycle, storage, network, and virtiofs tests. It builds or ensures the native musl builder first. |
| `freebsd` | `yaml/freebsd/` | FreeBSD 15.1 NoCloud source and `freebsd15` template. |
| `freebsd-headless` | `yaml/freebsd/` | [Serial-console FreeBSD with preinstalled QGA](../yaml/freebsd/README.md); prepares `freebsd` first. |
| `multi-os` | `yaml/multi-os/` | Debian 12, Ubuntu 26.04, AlmaLinux 10, and Alpine 3.21 base disks used by cloud-init tests and as the Alpine recipe-build fixture’s bake host. |
| `virtio-win` | `yaml/virtio-win/` | Shared, pinned `virtio-win-iso` installation medium. |
| `almalinux-10-media` | `yaml/almalinux-10-media/` | Official AlmaLinux 10 `x86_64_v2` DVD, verified against its current SHA256; shared with nested tests under `BaseImages/AlmaLinux10/Media/`. |
| `almalinux-10-desktop` | [yaml/almalinux-10-desktop/](../yaml/almalinux-10-desktop/README.md) | Offline Kickstart installation of GNOME with automatic login, VirtIO graphics/sound, and QEMU guest agent; no cloud-init. Publishes `almalinux-10-desktop-base` and its runtime template. |
| `windows-server-media` | `yaml/windows-server-media/` | Pinned `windows-server-2025-iso` evaluation installation medium. |
| `windows` | `yaml/windows-server-2025/` | `BaseImages/WindowsServer2025/<id>-windows-server-2025-eval.qcow2`, used by Windows and cloudbase-init integration coverage. (Image ID `windows` is a Make selector; the registered disk is `windows-server-2025-eval`.) |
| `installer` | `yaml/corvus-test-installer/` | `BaseImages/SyntheticInstaller/<id>-corvus-test-installer-iso.raw`, a small ISO used by `test_build_installer.py` to exercise the installer strategy. Its download cache is `yaml/corvus-test-installer/cache/`. |
| `key` | `integration_tests/keys/` | The SSH keypair embedded in the node and inner-VM images, used by the harness tunnel. |
| `gentoo-glibc-systemd-headless` | `yaml/gentoo/` | `gentoo-glibc-systemd-upstream-cloudinit`, `gentoo-glibc-systemd-builder-cloudinit`, `gentoo-glibc-systemd-headless`, and `gentoo-glibc-systemd-headless-cloudinit`. The `gentoo-glibc-systemd-headless` image is the base for both the Corvus development and node images. |
| `gentoo-musl-openrc-headless` | `yaml/gentoo/` | `gentoo-musl-openrc-headless` and `gentoo-musl-openrc-headless-cloudinit`, musl/OpenRC runtimes with BIOS/UEFI templates. Consumes the native musl builder, bootstrapped through the shared glibc builder and cloud import. |
| `gentoo` | `yaml/gentoo/` | `gentoo-corvus-test`, the Corvus development image. Builds or ensures the regular headless chain first. |
| `windows-11` | [yaml/windows-11/](../yaml/windows-11/README.md) | `BaseImages/Windows11/<id>-windows-11-pro-base.qcow2`, a generalized, decrypted Windows 11 Pro overlay base. The runtime template gives each VM its own TPM. Requires registered `windows-11-iso` media; missing VirtIO-Win media is downloaded automatically. |
| `windows-11-desktop` | [yaml/windows-11-desktop/](../yaml/windows-11-desktop/README.md) | Generalized Windows 11 desktop with Total Commander, Firefox, PDF-XChange Editor and Process Explorer; consumes the Windows 11 base and publishes `windows-11-desktop`. |
| `debian-nginx` | `yaml/debian-nginx/` | `debian-12-nginx`, an example Debian 12 nginx derivative built from the multi-OS Debian base. |
| `ubuntu-nginx` | `yaml/ubuntu-nginx/` | `ubuntu26-nginx`, an example Ubuntu 26.04 nginx derivative built from the multi-OS Ubuntu base. |
| `monitor` | `yaml/corvus-monitor/` | `corvus-monitor`, a Debian 12 Prometheus and Grafana image. Set `CORVUS_WEB_TARGET=host:port` when building to bake a Corvus metrics endpoint into its dashboard configuration. |

The test-node recipe lives in `yaml/gentoo/corvus-test-node.yml`. Build it with
`make image IMAGE=corvus-test-node` or `make -C yaml/gentoo build IMAGE=corvus-test-node`;
both prepare the registered dependencies and integration SSH key.
It uses the same Gentoo cache as the headless and development images. Run
`make image IMAGE=corvus-test-node GENTOO_CACHE_DIR=/srv/corvus-cache` to override the default `/data/corvus-cache` throughout its dependency chain.
The shared `gentoo-glibc-systemd-builder-cloudinit-bake` template builds an offline overlay of
`gentoo-glibc-systemd-headless` and attaches this directory through
virtiofs; downloads, built packages, and official binaries use its `distfiles`,
`packages/x86-64`, and `binhost/x86-64` subdirectories. The recipe checks directory creation and
file writes, disables `userfetch` and `userpriv` only during provisioning, and
keeps saved `emerge-webrsync -k` snapshots in the distfiles cache. Gentoo and
additional Git repositories are populated on tmpfs without using host repositories.
Published nodes contain neither repository trees nor cache mounts; their normal
Portage configuration remains suitable for local package installation. The runtime
template and integration harness do not attach the build cache.

The test-node exposes the host `~/VMs/BaseImages` tree to the inner daemon.
The harness snapshots the outer daemon's `latest` catalogue once per test
class and registers its exact backing paths with the inner daemon. Secondary
nodes use the same snapshot and inner image IDs. Retained versions and
unregistered files are ignored; IDs or filenames never select the latest.
Missing published files or ambiguous local placements fail registration.

The integration suite requires `node`, `vm`, `multi-os`, `freebsd`, `windows`,
`installer`, `almalinux-10-media`, and `key`. Run `make image-ensure IMAGE=<name>`
before running the suite; dependencies are ensured automatically. Use `make image IMAGE=<name>`
when deliberately publishing a replacement fixture.

### Default integration-test guest

`make image IMAGE=corvus-test-vm` builds a 1 GiB Gentoo musl/OpenRC guest using the
shared Gentoo kernel and native musl builder. It includes SSH over TCP/VSOCK,
QGA, ACPI shutdown, TPM tools, hardware inspection tools, and passwordless doas
for `corvus`. The test-only credentials are `corvus:corvus`; its SSH key is the
shared integration-test key. Host keys are generated on first boot. An initial
512 MiB BIOS/TPM smoke test measured 233 MiB used on the root filesystem and 53 MiB RAM usage after boot;
actual usage varies with attached devices and workloads.

The test guest owns its explicit minimal package list; it does not install
`@system` or the ordinary images’ extra package set. It rejects compilers, Portage,
glibc, systemd, Python, and sudo before removing documentation, headers, sources,
static libraries, installed-package metadata, Portage configuration, and
non-English locales. Other Gentoo images retain their installed development
files and package records. All built images follow the shared residue policy.
The smaller 64 MiB ESP leaves room for the root filesystem; all Gentoo
images share the expanded virtual-hardware kernel configuration and kernel cache.

The full recipe-build integration test deliberately builds the separate Alpine
fixture under `yaml/corvus-test-vm-alpine/`, not a nested Gentoo toolchain. It
uses unique disposable output names and does not replace the default guest.
To publish that fixture manually, run `make -C yaml/corvus-test-vm-alpine build`.
