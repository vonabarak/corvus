# Gentoo headless with Catalyst

This standalone pipeline builds `gentoo-glibc-systemd-headless-catalyst`: a
20 GiB qcow2 image with glibc, systemd, a custom kernel, GPT/ext4, BIOS and UEFI
boot, SSH and the QEMU guest agent. It has no cloud-init. Its image and template
names differ from the existing shell-based builds in [../gentoo](../gentoo/README.md).
It is deliberately absent from the top-level and aggregate image Makefiles.

## Build

Run from this directory, with a configured `crv` client and local `curl` and `git`:

```sh
make build
make ensure
make check
make test
```

Prerequisites are the registered `ovmf-code` and `ovmf-vars` images, `corvus`
network and SSH key, KVM on the selected node, and a writable virtiofs cache at
`/data/corvus-cache`. Override `GENTOO_CACHE_DIR` and `GENTOO_KEYSERVER` as needed.
The builder uses 8 CPUs, 16 GiB RAM and an 80 GiB disk; allow several hours for a
cold kernel/package build. All cache paths refer to the node running the VM.

`make build` prepares dependencies with `IMAGE_POLICY` (default `update`), then
publishes the runtime image. `make ensure` checks presence and only creates
missing outputs, using `skip`; it does not refresh existing images. `make rebuild`
forces publication using `overwrite`. `make clean` removes only local build
output, never registered images or the shared cache.

Build only the new helper image with:

```sh
make build IMAGE=gentoo-glibc-systemd-catalyst-builder
```

The dependency chain is:

1. The existing upstream import recipe in `../gentoo/Makefile` owns
   `gentoo-glibc-systemd-upstream-cloudinit`. This pipeline consumes that registered
   image and does not duplicate its import declaration.
2. `gentoo-glibc-systemd-catalyst-builder.yml` boots that upstream image directly,
   bootstraps QGA through cloud-init, and installs Catalyst 4.2.0 and disk tools.
   It publishes its own `-bootstrap` and `-bake` templates. It does not depend on
   either existing locally built Gentoo builder.
   The bake template explicitly starts QGA through cloud-init because Gentoo's
   static QGA unit does not gain a boot dependency from `systemctl enable`.
3. `gentoo-glibc-systemd-headless-catalyst.yml` runs Catalyst inside that helper
   and assembles the resulting stage4 into the new disk. It publishes BIOS and
   `-uefi` runtime templates.

The first guest-agent installation retains the existing upstream-bootstrap
signature exception. Subsequent builder package operations use the shared
verified Portage helpers. Runtime seed and repository verification fail closed.

## Catalyst owns the system build

`catalyst/catalyst.toml` is native Catalyst 4.2 TOML configuration.
`catalyst/stage4.spec` declares the profile, packages, kernel sources, genkernel
arguments, overlay, finish hook and cleanup. The custom kernel configuration is
uploaded from `../gentoo/scripts/stages/kernel/gentoo-kernel`; the old kernel
build script is not used. Catalyst installs packages and invokes genkernel in
its own chroot, then captures a stage4 archive.

The static root overlay supplies host, locale, network and SSH host-key setup.
`finish.sh` enables systemd units, adds QGA to `multi-user.target`, and saves the
effective kernel configuration.
The small `scripts/build.sh` adapter verifies inputs, runs Catalyst, checks the
archive, and reuses the existing disk and image helpers to partition, extract,
install both GRUB targets, reset identity and finalize. Package selection and
kernel compilation belong in the spec, rather than additional shell workflows.
This is an explicit Catalyst alternative to the ROOT-native implementation
rules of the existing `yaml/gentoo` pipeline.

The spec appends genkernel's `kernel` action after Catalyst's default `all`
action, selecting kernel and modules without an initramfs. The reused kernel
has built-in virtio block, ext4 and device support, and the reused GRUB
configuration boots directly from `/dev/vda3`.

## Inputs, verification and caches

Before Corvus fingerprints a runtime build, `scripts/publish.sh` resolves a
specific no-multilib systemd stage3 URL and SHA256, plus the Gentoo sync
repository's stable commit. These are passed as YAML variables into the guest
shell environment, so changes invalidate build metadata. To reproduce selected
inputs, provide all three explicitly:

```sh
make build STAGE3_URL=https://distfiles.gentoo.org/releases/amd64/autobuilds/current-stage3-amd64-nomultilib-systemd/stage3-amd64-nomultilib-systemd-TIMESTAMP.tar.xz \
  STAGE3_SHA256=64_HEX_DIGITS REPO_COMMIT=40_HEX_DIGITS
```

Replace the placeholders with real values. Historical releases may require an
archive mirror. Direct `crv build` also requires `stage3_url`, `stage3_sha256`
and `repo_commit`; local Make resolves them automatically.

The builder verifies the stage3's signed `.sha256` document against Gentoo's
installed release keys, compares it to the declared digest, then hashes the
archive. It fetches and verifies the repository commit signature before Catalyst
creates its snapshot. HTTPS alone is not the trust check. Binary packages retain
signature verification; unsigned locally compiled packages remain usable.

Distfiles are shared under `distfiles/`; Catalyst's compiled packages live under
`catalyst/packages/amd64-systemd/`. Kernel caches stay on the bake VM's native
filesystem: Catalyst moves kernel sources with ownership preservation, which an
unprivileged virtiofs share cannot support. Warm builds reuse packages and
distfiles, but rebuild the kernel.
A shared lock serializes access to Catalyst caches. Kernel caches, chroots,
downloaded seeds, repository snapshots and logs are local to each bake VM under
`/var/tmp/corvus-catalyst.*`. There is no autoresume across new builds.
The runtime contains input provenance in `/etc/corvus-image-info`.
Pinned source inputs do not promise byte-for-byte reproducibility: official
binary packages and other upstream downloads can change availability.

## Validation and debugging

`make test` runs offline input, checksum, artifact and publication-gate checks,
then checks shell syntax. From the repository root, `make yaml-schema-check`
validates both recipes. Full acceptance requires a cold build, a warm-cache
rebuild and boots from both runtime templates. Check DHCP, DNS, SSH, guest-agent
access, effective kernel configuration, empty machine identity before first
boot, distinct generated SSH keys across clones and absence of build residue.

Builds use `cleanup: onSuccess`; a failure preserves the helper and its private
Catalyst work directory for inspection. Do not publish a manually repaired disk
as validation of the recipe. Correct the configuration and rerun the build.
Use Corvus build/task logs and `/var/log/cloud-init-output.log` for bootstrap
failures. A failed build must not replace the previously published image.
