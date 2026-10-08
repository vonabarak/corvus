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

An explicit `image` publishes new versions with `overwrite`, except for the
Gentoo recipes, which use conditional updates as described below.
`image-rebuild` always uses `overwrite`.
Existing versions, backing files, overlays, VMs, and templates remain usable.
Unqualified image selectors resolve the current `latest`; templates are updated
in place so subsequent VM creation uses it. Already-created disks keep their
original backing version.

Managed YAML recipes default `image_if_exists` to `overwrite`, or `update` for
Gentoo; Make passes `skip` for ensure operations. The Python build API expands
declared defaults before submission too. For recipes with required variables,
set their values in the YAML `vars` mapping before calling the Python API.

`image-ensure` checks the declared disks and templates, creates missing outputs
with `skip`, and reuses existing versions. Dependencies use this target; rebuild
an upstream recipe explicitly to refresh it. `images-ensure` ensures all recipes.
Operational errors stop the build; only a missing-resource response triggers
publication. `image-check` checks every declared output without publishing.

`image-clean` removes only local `build/` intermediates. `image-cache-clean`
removes reusable download caches where declared. Published resources, runtime
VMs, writable disks, SSH keys, and caches survive ordinary cleanup.

The Gentoo base recipes live in `yaml/gentoo-headless/`, together with their
shared kernel configuration. `yaml/gentoo-test/` owns the Corvus development
image built from the `gentoo-headless-cloudinit` template. Manage the stages
with:

```sh
make -C yaml/gentoo-headless build-cloud
make -C yaml/gentoo-headless build-headless
make -C yaml/gentoo-headless build-headless-standalone
make -C yaml/gentoo-test build-test
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

The standalone target runs the same `build-cloud` prerequisite as the regular
headless target, then builds its derivative using official mirrors without the
regular recipe's host-side mounts. The download URL and checksum belong only
to `gentoo-base-cloud.yml`; both headless pipelines consume the registered
`gentoo-base-cloud` image. Standalone is opt-in and is not required by the
default Gentoo build or its presence checks. Aggregate image builds run the
development-image chain, which includes the regular headless images, once.

`ensure-cloud` and `ensure-headless` in `yaml/gentoo-headless/`, and `ensure` in
`yaml/gentoo-test/`, fill missing outputs with `skip`. When all outputs exist,
they perform no upstream downloads or publication. Node-image dependencies use
this presence-only path.

To force the regular chain, use `make image-rebuild IMAGE=gentoo-headless`; use
`IMAGE=gentoo` to force the development image and its upstream chain too. For
standalone, run `make -C yaml/gentoo-headless build-headless-standalone
IMAGE_POLICY=overwrite`. Forced builds publish fresh versions and retain old
ones. Build metadata does not detect changes inside external Portage
repositories, mirrors, or shared directories; force a rebuild to pick up those
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

For direct Gentoo CLI use, run `make -C yaml/gentoo-headless build-cloud` before
invoking either headless YAML. Their Make build targets prepare the source
automatically, including with parallel Make. "Standalone" refers to the absence
of host-side mounts during the bake, not to importing dependencies in its YAML.

Follow-up: split the embedded imports out of
[`windows-11.yml`](../yaml/windows-11/windows-11.yml) and
[`windows-server-2025.yml`](../yaml/windows-server-2025/windows-server-2025.yml).
Give their shared VirtIO-Win installation medium one dedicated import owner,
and move the Windows Server evaluation ISO import into a dedicated recipe.
Wire those dependencies through their Makefiles. These existing recipes have
not yet been converted to the import ownership rule.

## Recipe catalogue

`yaml/test-images/`, `yaml/example-apply/`, and `yaml/template-example/` are
apply or template examples that consume images; they do not create a managed
image artifact. `yaml/test-images/test-images.yml` uses the `vm` and `windows`
artifacts above.

| Image | Directory | Artifact and use |
| --- | --- | --- |
| `node` | `yaml/corvus-test-node/` | `corvus-test-node`, the Gentoo outer node used by every topology. It ensures `gentoo-headless` from `yaml/gentoo-headless/` when needed. |
| `vm` | `yaml/corvus-test-vm/` | `BaseImages/Alpine/<id>-corvus-test-vm.qcow2`, the inner Alpine VM used by Linux lifecycle, storage, network, and virtiofs tests. It ensures `multi-os` first. |
| `multi-os` | `yaml/multi-os/` | Debian 12, Ubuntu 26.04, AlmaLinux 10, FreeBSD 14, and Alpine 3.21 base disks used by cloud-init tests and as the VM-image build base. |
| `windows` | `yaml/windows-server-2025/` | `BaseImages/WindowsServer2025/<id>-windows-server-2025-eval.qcow2`, used by Windows and cloudbase-init integration coverage. (Image ID `windows` is a Make selector; the registered disk is `windows-server-2025-eval`.) |
| `installer` | `yaml/corvus-test-installer/` | `BaseImages/SyntheticInstaller/<id>-corvus-test-installer-iso.raw`, a small ISO used by `test_build_installer.py` to exercise the installer strategy. Its download cache is `yaml/corvus-test-installer/cache/`. |
| `key` | `integration_tests/keys/` | The SSH keypair embedded in the node and inner-VM images, used by the harness tunnel. |
| `gentoo-headless` | `yaml/gentoo-headless/` | `gentoo-base-cloud`, `gentoo-builder`, `gentoo-base-headless`, and `gentoo-base-headless-cloudinit`. The headless-cloudinit image is the base for the Corvus development image; `gentoo-headless` is the node-image base. An opt-in target builds `gentoo-base-headless-standalone`. |
| `gentoo` | `yaml/gentoo-test/` | `gentoo-corvus-test`, the Corvus development image. Builds or ensures the regular headless chain first. |
| `windows-11` | [yaml/windows-11/](../yaml/windows-11/README.md) | `BaseImages/Windows11/<id>-windows-11-pro-base.qcow2`, a generalized, decrypted Windows 11 Pro overlay base. The runtime template gives each VM its own TPM. Requires registered `windows-11-iso` media; missing VirtIO-Win media is downloaded automatically. |
| `debian-nginx` | `yaml/debian-nginx/` | `debian-12-nginx`, an example Debian 12 nginx derivative built from the multi-OS Debian base. |
| `ubuntu-nginx` | `yaml/ubuntu-nginx/` | `ubuntu26-nginx`, an example Ubuntu 26.04 nginx derivative built from the multi-OS Ubuntu base. |
| `monitor` | `yaml/corvus-monitor/` | `corvus-monitor`, a Debian 12 Prometheus and Grafana image. Set `CORVUS_WEB_TARGET=host:port` when building to bake a Corvus metrics endpoint into its dashboard configuration. |

The test-node exposes the host `~/VMs/BaseImages` tree to the inner daemon.
The harness snapshots the outer daemon's `latest` catalogue once per test
class and registers its exact backing paths with the inner daemon. Secondary
nodes use the same snapshot and inner image IDs. Retained versions and
unregistered files are ignored; IDs or filenames never select the latest.
Missing published files or ambiguous local placements fail registration.

The integration suite requires `node`, `vm`, `multi-os`, `windows`,
`installer`, and `key`. Run `make image-ensure IMAGE=<name>` before running the
suite; dependencies are ensured automatically. Use `make image IMAGE=<name>`
when deliberately publishing a replacement fixture.
