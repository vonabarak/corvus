# AlmaLinux 10 GNOME desktop from an official DVD

This example uses `strategy: installer` with the official AlmaLinux 10
`x86_64_v2` DVD and the checked-in [Kickstart file](ks.cfg). The DVD supplies
all packages: installation needs no external repository. Anaconda automatically
loads `ks.cfg` from a second ISO labeled `OEMDRV`, installs, and powers off.
Corvus captures the first disk and registers the `almalinux-10-desktop` template.
The build sends Up and Enter at the DVD boot menu to select "Start AlmaLinux"
immediately, skipping the 60-second countdown and redundant media check.

Kickstart reads BaseOS and AppStream directly from the DVD mounted at
`/run/install/repo`. This avoids the [upstream x86_64_v2 CD-ROM detection
bug](https://bugs.almalinux.org/view.php?id=575), without modifying the official ISO.

The desktop uses GNOME with automatic login, VirtIO graphics and sound, SPICE
integration, and the QEMU guest agent. It does not install cloud-init. BIOS
firmware, 4 vCPUs, 4 GiB RAM, and a 40 GiB disk are the example defaults.
Kickstart enables the guest agent's `guest-exec` RPCs and makes only its
`virt_qemu_ga_t` SELinux domain permissive, so Corvus can run desktop checks
through the agent while other SELinux domains remain enforcing. Adapt that
policy to your security requirements before deploying the image more broadly.

## Build and run

Use a working Corvus node with KVM, the managed `corvus` network, and `mkisofs`
(or `genisoimage`, selected with `ISO_TOOL=genisoimage`). Allow disk space for
the full DVD, installed image, and temporary bake disk.

From the repository root:

```sh
make image IMAGE=almalinux-10-desktop
crv template instantiate almalinux-10-desktop alma-desktop
crv vm start alma-desktop --wait
crv vm view alma-desktop
```

The login and password are **`corvus` / `corvus`**; this user belongs to `wheel`.
Root login is locked. Edit the example password hash in `ks.cfg` before building
an image with different credentials. GDM logs `corvus` in automatically, including
after a restart. No SSH keys, cloud-init user data, or host identity are injected.

The dedicated `almalinux-10-media` recipe owns the official DVD import. It
resolves `AlmaLinux-10-latest-x86_64_v2-dvd.iso` and the matching SHA256 from
the [official ISO directory](https://repo.almalinux.org/almalinux/10/isos/x86_64_v2/).
Explicit builds refresh that dependency using the selected `IMAGE_POLICY`.
The imported DVD is stored under `BaseImages/AlmaLinux10/Media/`, and the
standalone qcow2 artifact under `BaseImages/AlmaLinux10/`.

```sh
make image-ensure IMAGE=almalinux-10-desktop  # Only fill missing outputs
make image-check IMAGE=almalinux-10-desktop
make image-rebuild IMAGE=almalinux-10-desktop # Force new media and image versions
make image-clean IMAGE=almalinux-10-desktop   # Remove local intermediates only
```

Edit `ks.cfg` to customize packages or installation settings. Make regenerates
`build/kickstart.iso` when the Kickstart file or Makefile changes; unchanged
builds retain identical answer-media bytes. Updated media invalidate dependent
builds. For direct `crv build` use, first prepare the vendor media and run
`make -C yaml/almalinux-10-desktop answer-media`.

## Verify and troubleshoot

The real installation test requires only the node image and vendor DVD, not a
prebuilt desktop:

```sh
make image-ensure IMAGE=node
make image-ensure IMAGE=almalinux-10-media
make integration-tests MATCH=TestBuildAlmaLinuxInstaller
```

It creates answer media in a temporary directory on the test host, uploads it,
and runs the installation on an isolated nested node using its normal writable
`BaseImages` directory; the host base images are mounted read-only at `BaseImagesRO`.
It verifies the
resulting desktop without installer media through the guest agent, then repeats
the checks after a restart.
Checks include the OS version, guest agent, absence of cloud-init, active GNOME
login, VirtIO drivers, and playback through the user's PipeWire sink.
For a manual check, open the desktop with `crv vm view`, confirm automatic login,
and play `/usr/share/sounds/alsa/Front_Center.wav` with `pw-play` in a terminal.

The installer has a one-hour shutdown timeout and `cleanup: onSuccess`.
A failed bake stays available for SPICE inspection. Anaconda logs are under
`/tmp` during installation and `/var/log/anaconda/` in the installed system;
Kickstart post-install output is in `/root/corvus-kickstart.log`. Successful
configuration creates `/var/lib/corvus/installer-complete`. The Kickstart must
finish with `poweroff`: rebooting would return to the installation DVD.

See the [installer strategy](../../doc/image-builds.md#installer),
[image lifecycle](../../doc/test-images.md), and
[RHEL 10 Kickstart guide](https://docs.redhat.com/en/documentation/red_hat_enterprise_linux/10/html-single/automatically_installing_rhel/index)
for the underlying conventions and unattended-install syntax.
