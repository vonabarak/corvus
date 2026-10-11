#!/bin/sh
# Build natively against the bake host, as in the @system bootstrap. Treating
# this same-architecture root as a cross-compilation sysroot makes Rust bindgen
# look for build-host libclang inside the unfinished image.
# Keep thin-provisioning-tools explicit: it was shipped by the regular flavor,
# but Portage can treat it as a bake-host dependency when installing LVM.
env -u SYSROOT ROOT="$SYSROOT" emerge --quiet-build \
  app-emulation/qemu-guest-agent \
  mail-mta/msmtp \
  app-admin/sudo \
  net-fs/nfs-utils \
  sys-block/thin-provisioning-tools \
  app-editors/vim \
  app-shells/bash-completion
