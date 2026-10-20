# Image builds

Every image recipe owns a Makefile in its `yaml/` directory. The root Makefile
only forwards image commands to `yaml/Makefile`. Build every managed image
with `make images`, remove local intermediates with `make images-clean`, or publish new versions with
`make images-rebuild`. The commands below select one image:

```sh
make image-list
make image IMAGE=node
make image-ensure IMAGE=node
make image-check IMAGE=node
make image-clean IMAGE=node
make image-rebuild IMAGE=node
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
`gentoo-headless`. The default `build` target runs the full chain. Manage the stages
with:

```sh
make -C yaml/gentoo build-cloud
make -C yaml/gentoo build-headless
make -C yaml/gentoo build-test
make image IMAGE=gentoo-headless
make image IMAGE=gentoo
```

Ordinary Gentoo builds use `ifExists: update`. `build-cloud` fetches the current
upstream manifest and the SHA256 for its selected filename. An unchanged
checksum reuses the imported image without downloading it again. `build-headless`
checks that import first, then evaluates the builder, headless, and headless
cloud-init builds in order. `build-test` evaluates that whole chain before the
development image. Each build skips when its stored metadata matches the
current recipe and resolved source versions; a changed source or rebuilt
intermediate causes dependent builds to rebuild. See
[build metadata comparison](image-builds.md#how-update-compares-metadata).

The single headless pipeline uses official Gentoo mirrors and a writable
virtiofs cache; the build host does not need Gentoo or a Portage tree. The
`gentoo-cloud.yml` recipe owns the upstream import, tagged with the timestamp
in Gentoo's filename. The pipeline prepares `gentoo-builder`, builds
`gentoo-headless` from scratch, then adds Cloud-init in a separate overlay.
Small provisioning steps live inline; larger steps remain in `scripts/`.
Expanded script contents participate in build metadata.

Provision `/data/corvus-cache` on the execution node with permissions allowing
VM root to write through virtiofs. When the node agent runs as an ordinary user,
that host user must be able to create directories and files in each cache
subdirectory too. A root-owned `binpkgs` directory with mode `0755` prevents
binary package writes in this setup. Repair its ownership on the execution node,
for example `sudo chown -R <node-agent-user>:<node-agent-group> /data/corvus-cache/binpkgs`.
The recipes check directory creation and file writes before starting Portage.
The guest creates `distfiles`, `binpkgs`, and
`binhost` subdirectories after mounting the share. Override the path throughout
the dependency chain with:

```sh
make image IMAGE=gentoo GENTOO_CACHE_DIR=/srv/corvus-cache
```

For direct CLI builds, pass `--var cache_dir=/srv/corvus-cache` to each consuming
recipe. Distfiles, downloaded official binaries, and locally compiled packages
are cached separately inside this directory. Every package installation uses
`--buildpkg`; `emerge-webrsync -k` also keeps repository snapshots and their
verification files in the distfiles cache. Subsequent builds reuse cached files
while checking for current snapshots. Kernel compilation still runs normally.
The first build can take substantially longer than subsequent builds. Bake
commands disable Portage’s `userfetch` and `userpriv` features: downloads and
ebuild phases run as guest root, including live-package Git fetches during
`src_unpack`. Root can write through the share even when the host cannot map
the guest `portage` UID. The development image also retains these overrides so
operators can write to the same shared caches; other installed images retain
their normal Portage privilege settings.
Native build dependencies also receive these settings in the bake host's
`make.conf`; Portage resolves them separately from packages installed into the
target root. Build environments explicitly set `DISTDIR` and `PKGDIR` to
`/mnt/corvus-cache/distfiles` and `/mnt/corvus-cache/binpkgs`, so selecting another
ROOT or SYSROOT does not redirect the caches. The from-scratch target's
distfiles and package cache directories stay empty during the build; those
shared caches are not mounted inside SYSROOT.

The intermediate builder retains `/var/db/repos` on its filesystem so the
from-scratch build can use it. The new headless root has an empty repository
directory. Cloud-init and development-image builds mount that directory
as tmpfs before syncing, including additional development overlays, so their
published disks contain no Portage tree. The bake host exposes caches at
`/mnt/corvus-cache` and temporary `/var/cache` bind mounts. The development VM also receives the cache share, with optional
systemd automounts at `/mnt/corvus-cache` and dependent bind automounts at
`/var/cache/distfiles`, `/var/cache/binpkgs`, and `/var/cache/binhost`.
Other runtime templates do not attach caches.
The external host cache survives both `clean` and `cache-clean`.

The development recipe uses `offline-overlay` with `gentoo-headless-cloudinit`
as its artifact source. A separate 40 GB helper boots the same base and waits
for Cloud-init to grow its filesystem. Package compilation uses the helper's
`/var/tmp/portage`; the artifact keeps the base image's size (currently 20 GB).
The artifact never boots during provisioning, preserving Cloud-init first-boot
state. PostgreSQL is initialized with a temporary local-only server inside the
chroot; runtime services are enabled for the first real boot.

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

`ensure-cloud`, `ensure-headless`, and `ensure` in `yaml/gentoo/` fill missing outputs with `skip`. When all outputs exist,
they perform no upstream downloads or publication. Node-image ensure operations
use this presence-only path; ordinary node builds evaluate the headless chain
first.

To force the chain, use `make image-rebuild IMAGE=gentoo-headless`; use
`IMAGE=gentoo` to force the development image and its upstream chain too.
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

For direct Gentoo CLI use, run `make -C yaml/gentoo build-cloud` before
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

`yaml/test-images/`, `yaml/example-apply/`, and `yaml/template-example/` are
apply or template examples that consume images; they do not create a managed
image artifact. `yaml/test-images/test-images.yml` uses the `vm` and `windows`
artifacts above.

| Image | Directory | Artifact and use |
| --- | --- | --- |
| `node` | `yaml/corvus-test-node/` | `corvus-test-node`, the Gentoo outer node used by every topology. It builds or ensures `gentoo-headless` from `yaml/gentoo/` when needed. |
| `vm` | `yaml/corvus-test-vm/` | `BaseImages/Alpine/<id>-corvus-test-vm.qcow2`, the inner Alpine VM used by Linux lifecycle, storage, network, and virtiofs tests. It builds or ensures `multi-os` first. |
| `freebsd` | `yaml/freebsd/` | FreeBSD 15.1 NoCloud source and `freebsd15` template. |
| `freebsd-headless` | `yaml/freebsd/` | [Serial-console FreeBSD with preinstalled QGA](../yaml/freebsd/README.md); prepares `freebsd` first. |
| `multi-os` | `yaml/multi-os/` | Debian 12, Ubuntu 26.04, AlmaLinux 10, and Alpine 3.21 base disks used by cloud-init tests and as the VM-image build base. |
| `virtio-win` | `yaml/virtio-win/` | Shared, pinned `virtio-win-iso` installation medium. |
| `almalinux-10-media` | `yaml/almalinux-10-media/` | Official AlmaLinux 10 `x86_64_v2` DVD, verified against its current SHA256; shared with nested tests under `BaseImages/AlmaLinux10/Media/`. |
| `almalinux-10-desktop` | [yaml/almalinux-10-desktop/](../yaml/almalinux-10-desktop/README.md) | Offline Kickstart installation of GNOME with automatic login, VirtIO graphics/sound, and QEMU guest agent; no cloud-init. Publishes `almalinux-10-desktop-base` and its runtime template. |
| `windows-server-media` | `yaml/windows-server-media/` | Pinned `windows-server-2025-iso` evaluation installation medium. |
| `windows` | `yaml/windows-server-2025/` | `BaseImages/WindowsServer2025/<id>-windows-server-2025-eval.qcow2`, used by Windows and cloudbase-init integration coverage. (Image ID `windows` is a Make selector; the registered disk is `windows-server-2025-eval`.) |
| `installer` | `yaml/corvus-test-installer/` | `BaseImages/SyntheticInstaller/<id>-corvus-test-installer-iso.raw`, a small ISO used by `test_build_installer.py` to exercise the installer strategy. Its download cache is `yaml/corvus-test-installer/cache/`. |
| `key` | `integration_tests/keys/` | The SSH keypair embedded in the node and inner-VM images, used by the harness tunnel. |
| `gentoo-headless` | `yaml/gentoo/` | `gentoo-cloud`, `gentoo-builder`, `gentoo-headless`, and `gentoo-headless-cloudinit`. The `gentoo-headless` image is the base for both the Corvus development and node images. |
| `gentoo` | `yaml/gentoo/` | `gentoo-corvus-test`, the Corvus development image. Builds or ensures the regular headless chain first. |
| `windows-11` | [yaml/windows-11/](../yaml/windows-11/README.md) | `BaseImages/Windows11/<id>-windows-11-pro-base.qcow2`, a generalized, decrypted Windows 11 Pro overlay base. The runtime template gives each VM its own TPM. Requires registered `windows-11-iso` media; missing VirtIO-Win media is downloaded automatically. |
| `windows-11-desktop` | [yaml/windows-11-desktop/](../yaml/windows-11-desktop/README.md) | Generalized Windows 11 desktop with Total Commander, Firefox, PDF-XChange Editor and Process Explorer; consumes the Windows 11 base and publishes `windows-11-desktop`. |
| `debian-nginx` | `yaml/debian-nginx/` | `debian-12-nginx`, an example Debian 12 nginx derivative built from the multi-OS Debian base. |
| `ubuntu-nginx` | `yaml/ubuntu-nginx/` | `ubuntu26-nginx`, an example Ubuntu 26.04 nginx derivative built from the multi-OS Ubuntu base. |
| `monitor` | `yaml/corvus-monitor/` | `corvus-monitor`, a Debian 12 Prometheus and Grafana image. Set `CORVUS_WEB_TARGET=host:port` when building to bake a Corvus metrics endpoint into its dashboard configuration. |

The test-node image build uses the same Gentoo cache as the headless and
development images. Run `make image IMAGE=node GENTOO_CACHE_DIR=/srv/corvus-cache`
to override the default `/data/corvus-cache` throughout its dependency chain.
The dedicated `corvus-test-node-bake` template attaches this directory through
virtiofs; downloads, built packages, and official binaries use its `distfiles`,
`binpkgs`, and `binhost` subdirectories. The recipe checks directory creation and
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
