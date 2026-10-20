# FreeBSD images

Run these commands from the repository root:

```sh
make image IMAGE=freebsd
make image IMAGE=freebsd-headless
```

`freebsd.yml` exclusively owns the official FreeBSD 15.1-RELEASE amd64
BASIC-CLOUDINIT UFS source import, registered as `freebsd:15`, and its
`freebsd15` template. The checksum covers the compressed download.
`multi-os` now contains only Linux sources.

`freebsd-headless.yml` consumes that registered source and publishes a flattened,
compacted `freebsd-headless` image and runtime template. The bake uses NoCloud
(nuageinit) to install QEMU guest agent, configures the primary serial console at
115200 baud, reboots, verifies the active console and agent, then removes bake
identity and re-arms first-boot initialization. The runtime VM needs no package
repository access to start QGA. The QGA startup hook sets its terminal port
to raw mode with a larger buffer so long provisioning commands work on FreeBSD. NoCloud remains enabled for hostname, networking,
users, and SSH keys on each new VM.

The build requires registered `ovmf-code` and `ovmf-vars` firmware images,
a `corvus` network with Internet access, and the `corvus` SSH key used by the
published templates. The dedicated bake template injects no SSH keys and suppresses
the upstream factory user. The runtime template creates the key-only `corvus` account;
provide your own NoCloud user configuration if password login on the serial
console is desired. Root is locked. Use `crv vm view` for serial access.

```sh
make image-ensure IMAGE=freebsd-headless
make image-rebuild IMAGE=freebsd-headless
```

Normal builds refresh the source first and propagate `IMAGE_POLICY` (`update`,
`skip`, or `overwrite`). `ensure` checks presence and publishes missing outputs
with `skip`; `rebuild` forces publication with `overwrite`. Aggregate image builds
prepare the source once before the derivative. The raw source template bootstraps
QGA through NoCloud on first boot; the headless image has it preinstalled.
