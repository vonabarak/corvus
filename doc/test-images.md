# Image builds

Every image recipe owns a Makefile in its `yaml/` directory. The root Makefile
only forwards image commands to `yaml/Makefile`. Build every managed image
with `make images`, clean them with `make images-clean`, or recreate them with
`make images-rebuild`. The commands below select one image:

```sh
make image-list
make image IMAGE=node
make image-check IMAGE=node
make image-clean IMAGE=node
make image-rebuild IMAGE=node
make image-cache-clean IMAGE=installer
```

`image-clean` removes the registered artifact and its image-specific local
intermediates. `image-rebuild` runs its clean operation and then builds it.
`image-cache-clean` only removes reusable download caches where an image has
one. The SSH key is retained by cleanup because rotating it would invalidate
access to existing test nodes.

`yaml/test-images/`, `yaml/example-apply/`, and `yaml/template-example/` are
apply or template examples that consume images; they do not create a managed
image artifact. `yaml/test-images/test-images.yml` uses the `vm` and `windows`
artifacts above.

| Image | Directory | Artifact and use |
| --- | --- | --- |
| `node` | `yaml/corvus-test-node/` | `corvus-test-node`, the Gentoo outer node used by every topology. It builds `gentoo-headless` from `yaml/gentoo-test/` when needed. |
| `vm` | `yaml/corvus-test-vm/` | `BaseImages/Alpine/corvus-test-vm.qcow2`, the inner Alpine VM used by Linux lifecycle, storage, network, and virtiofs tests. It ensures `multi-os` first. |
| `multi-os` | `yaml/multi-os/` | Debian 12, Ubuntu 24.04, AlmaLinux 10, FreeBSD 14, and Alpine 3.21 base disks used by cloud-init tests and as the VM-image build base. |
| `windows` | `yaml/windows-server-2025/` | `BaseImages/WindowsServer2025/windows-server-2025-eval.qcow2`, used by Windows and cloudbase-init integration coverage. (Image ID `windows` is a Make selector; the registered disk is `windows-server-2025-eval`.) |
| `installer` | `yaml/corvus-test-installer/` | `BaseImages/SyntheticInstaller/corvus-test-installer-iso.raw`, a small ISO used by `test_build_installer.py` to exercise the installer strategy. Its download cache is `yaml/corvus-test-installer/cache/`. |
| `key` | `integration_tests/keys/` | The SSH keypair embedded in the node and inner-VM images, used by the harness tunnel. |
| `gentoo` | `yaml/gentoo-test/` | `gentoo-base-cloud`, `gentoo-builder`, `gentoo-base-headless`, `gentoo-base-headless-cloudinit`, and `gentoo-corvus-test`. The headless-cloudinit image is the base for the Corvus development image; `gentoo-headless` is the node-image base. |
| `windows-11` | `yaml/windows-11/` | `BaseImages/Windows11/windows-11-pro-base.qcow2`, a TPM-backed Windows 11 Pro image for manual and compatibility testing. It requires registered `windows-11-iso` and `virtio-win-iso` media. |
| `debian-nginx` | `yaml/debian-nginx/` | `debian-12-nginx`, an example Debian 12 nginx derivative built from the multi-OS Debian base. |
| `ubuntu-nginx` | `yaml/ubuntu-nginx/` | `ubuntu24-nginx`, an example Ubuntu 24.04 nginx derivative built from the multi-OS Ubuntu base. |
| `monitor` | `yaml/corvus-monitor/` | `corvus-monitor`, a Debian 12 Prometheus and Grafana image. Set `CORVUS_WEB_TARGET=host:port` when building to bake a Corvus metrics endpoint into its dashboard configuration. |

The test-node exposes the host `~/VMs/BaseImages` tree to the inner daemon.
The harness registers the published files from that tree, so do not change the
artifact paths or names without updating `integration_tests/corvus_test_harness/base_images.py`
and its callers.

The integration suite requires `node`, `vm`, `multi-os`, `windows`,
`installer`, and `key`. Build those explicitly with `make image IMAGE=<name>`
before running the suite; their dependencies are built automatically where
needed.
