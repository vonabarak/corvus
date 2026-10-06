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

An explicit `image` or `image-rebuild` publishes new versions with `overwrite`.
Existing versions, backing files, overlays, VMs, and templates remain usable.
Unqualified image selectors resolve the current `latest`; templates are updated
in place so subsequent VM creation uses it. Already-created disks keep their
original backing version.

Managed YAML recipes default `image_if_exists` to `overwrite`; Make passes
`skip` for ensure operations. The Python build API expands declared defaults
before submission too. For recipes with required variables, set their values
in the YAML `vars` mapping before calling the Python API.

`image-ensure` checks the declared disks and templates, creates missing outputs
with `skip`, and reuses existing versions. Dependencies use this target; rebuild
an upstream recipe explicitly to refresh it. `images-ensure` ensures all recipes.
Operational errors stop the build; only a missing-resource response triggers
publication. `image-check` checks every declared output without publishing.

`image-clean` removes only local `build/` intermediates. `image-cache-clean`
removes reusable download caches where declared. Published resources, runtime
VMs, writable disks, SSH keys, and caches survive ordinary cleanup.

Gentoo stages can be managed separately with `make -C yaml/gentoo-test
build-cloud`, `build-headless`, or `build-test`; `ensure-cloud` and
`ensure-headless` reuse their existing outputs. The ordinary Gentoo build
reuses the upstream cloud import and publishes the headless and test stages.

`yaml/test-images/`, `yaml/example-apply/`, and `yaml/template-example/` are
apply or template examples that consume images; they do not create a managed
image artifact. `yaml/test-images/test-images.yml` uses the `vm` and `windows`
artifacts above.

| Image | Directory | Artifact and use |
| --- | --- | --- |
| `node` | `yaml/corvus-test-node/` | `corvus-test-node`, the Gentoo outer node used by every topology. It builds `gentoo-headless` from `yaml/gentoo-test/` when needed. |
| `vm` | `yaml/corvus-test-vm/` | `BaseImages/Alpine/<id>-corvus-test-vm.qcow2`, the inner Alpine VM used by Linux lifecycle, storage, network, and virtiofs tests. It ensures `multi-os` first. |
| `multi-os` | `yaml/multi-os/` | Debian 12, Ubuntu 26.04, AlmaLinux 10, FreeBSD 14, and Alpine 3.21 base disks used by cloud-init tests and as the VM-image build base. |
| `windows` | `yaml/windows-server-2025/` | `BaseImages/WindowsServer2025/<id>-windows-server-2025-eval.qcow2`, used by Windows and cloudbase-init integration coverage. (Image ID `windows` is a Make selector; the registered disk is `windows-server-2025-eval`.) |
| `installer` | `yaml/corvus-test-installer/` | `BaseImages/SyntheticInstaller/<id>-corvus-test-installer-iso.raw`, a small ISO used by `test_build_installer.py` to exercise the installer strategy. Its download cache is `yaml/corvus-test-installer/cache/`. |
| `key` | `integration_tests/keys/` | The SSH keypair embedded in the node and inner-VM images, used by the harness tunnel. |
| `gentoo` | `yaml/gentoo-test/` | `gentoo-base-cloud`, `gentoo-builder`, `gentoo-base-headless`, `gentoo-base-headless-cloudinit`, and `gentoo-corvus-test`. The headless-cloudinit image is the base for the Corvus development image; `gentoo-headless` is the node-image base. |
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
