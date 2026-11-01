#!/bin/bash
# Configure the integration-test node offline; only individual commands use chroot.
set -euxo pipefail
. "${CORVUS_BUILD_LIB:-/usr/local/lib/corvus-build}/build.sh"

# Keep the node's runtime package policy separate from the development image.
write_node_package_policy() {
    local config_root=${1%/}
    install -d "$config_root/etc/portage"/{package.use,package.accept_keywords}
    write_corvus_repositories "$1"
    cat > "$config_root/etc/portage/package.use/corvus" <<'EOF'
app-emulation/qemu spice pipewire pulseaudio usb usbredir virtfs passt vde
net-libs/gnutls pkcs11 tools
dev-db/postgresql python uuid xml
media-libs/libpulse -X -gtk -glib
media-video/pipewire -X -zeroconf -v4l -bluetooth pulseaudio sound-server
EOF
    cat > "$config_root/etc/portage/package.accept_keywords/corvus" <<'EOF'
app-emulation/corvus **
app-emulation/corvus-bin **
dev-python/pycapnp **
dev-python/fastapi **
dev-haskell/supervisors **
dev-haskell/data-default-instances-vector **
dev-haskell/monad-stm **
dev-haskell/zenhack-prelude **
EOF
}

# Sync overlays natively and install runtime tools into the mounted artifact.
install_node_packages() {
    write_node_package_policy /
    write_node_package_policy "$root"
    env -u SYSROOT ROOT=/ emerge --noreplace --usepkg --buildpkg --quiet-build dev-vcs/git
    emerge --sync
    install_target_packages --noreplace --usepkg --buildpkg --quiet-build \
      net-misc/socat \
      app-emulation/qemu \
      media-video/pipewire \
      app-crypt/swtpm \
      net-misc/vde \
      net-dns/dnsmasq \
      net-firewall/nftables \
      app-emulation/virtiofsd \
      app-cdr/cdrtools \
      sys-fs/dosfstools \
      sys-fs/mtools \
      sys-firmware/edk2-bin \
      dev-db/postgresql:18 \
      dev-vcs/git \
      net-misc/curl \
      app-arch/zstd \
      sys-process/htop \
      app-misc/jq \
      dev-python/pip \
      dev-python/click \
      dev-python/cryptography \
      dev-python/jinja2 \
      dev-python/pyyaml \
      dev-python/pycapnp \
      dev-python/uvicorn \
      dev-python/fastapi \
      dev-python/prometheus-client \
      dev-python/httptools \
      dev-python/uvloop \
      dev-python/watchfiles \
      dev-python/websockets

    # The local pycapnp ebuild has a plain capnproto dependency
    # rather than a subslot operator. If emerge upgraded
    # capnproto, its existing C extension can still be linked to
    # the removed ABI. Rebuild just pycapnp from source against
    # the installed library; all of its build dependencies may
    # still come from the shared binary package cache.
    install_target_packages --usepkg --quiet-build \
      --usepkg-exclude=dev-python/pycapnp \
      --buildpkg \
      --reinstall-atoms=dev-python/pycapnp \
      dev-python/pycapnp
}

# Create target accounts before installing user-owned fixture and certificate paths.
configure_node_user() {
    if ! chroot "$root" id corvus >/dev/null 2>&1; then
        chroot "$root" useradd -m -s /bin/bash -G wheel,kvm corvus
    else
        chroot "$root" usermod -aG wheel,kvm corvus
    fi
    # Resolve ownership using target accounts, never the helper's numeric IDs.
    chroot "$root" chown corvus:corvus /home/corvus
    chroot "$root" install -d -m 0700 -o corvus -g corvus /home/corvus/.ssh
    chroot "$root" install -d -m 0755 -o corvus -g corvus \
        /home/corvus/VMs /home/corvus/VMs/BaseImages /home/corvus/VMs/BaseImagesRO /etc/corvus
    install -m 0600 /tmp/corvus-test-node/authorized_keys "$root/home/corvus/.ssh/authorized_keys"
    chroot "$root" chown corvus:corvus /home/corvus/.ssh/authorized_keys
    install -d -m 0750 "$root/etc/sudoers.d"
    echo '%wheel ALL=(ALL) NOPASSWD: ALL' > "$root/etc/sudoers.d/wheel-nopasswd"
    chmod 0440 "$root/etc/sudoers.d/wheel-nopasswd"
}

# Enable only services that can start before harness certificate deployment.
install_node_services() {
    install -d -m 0755 "$root/opt/corvus/bin" "$root/mnt/corvus" "$root/etc/systemd/system"
    install -m 0644 /tmp/corvus-test-node/systemd/* "$root/etc/systemd/system/"
    systemctl --root="$root" enable \
        opt-corvus-bin.mount mnt-corvus.mount home-corvus-VMs-BaseImagesRO.mount \
        corvus-tcp-relay.service sshd.service
    # The harness deploys mTLS certificates before enabling these three services.
    systemctl --root="$root" disable corvus.service corvus-netd.service corvus-nodeagent.service
}

# Make host-shared commands and diagnostics available during interactive inspection.
configure_node_debugging() {
    install -d -m 0755 "$root/etc/profile.d"
    cat > "$root/etc/profile.d/50-corvus.sh" <<'EOF'
# Test-node debug aliases — see yaml/gentoo/

# Put the host-shared crv on PATH so tab-completion can
# invoke it bare (alias expansion is off inside the
# completion function). The daemon listens on TCP via
# the vsock-to-TCP relay; CORVUS_HOST / CORVUS_PORT let
# crv (and the entity-name completer) target it.
if [ -d /opt/corvus/bin ]; then
  case ":$PATH:" in
    *:/opt/corvus/bin:*) ;;
    *) PATH="/opt/corvus/bin:$PATH" ;;
  esac
  export PATH
  export CORVUS_HOST=127.0.0.1
  export CORVUS_PORT=9876
fi

alias crv='/opt/corvus/bin/crv'
alias crv-log='journalctl -u corvus -f --since "5 min ago"'
alias crv-status='systemctl status --no-pager corvus'
alias crv-psql='psql -U corvus corvus'
# Show every corvus-spawned process (qemu, virtiofsd, dnsmasq).
alias crv-procs='pgrep -af "[c]orvus|[q]emu-system|[v]irtiofsd|[d]nsmasq" | sort'

# Tab-completion for `crv`. Regenerated each login so
# it tracks whichever binary is mounted at the time
# (the virtiofs share is the developer's stack build).
if [ -x /opt/corvus/bin/crv ]; then
  eval "$(/opt/corvus/bin/crv completion bash 2>/dev/null)"
fi
EOF
    chmod 0644 "$root/etc/profile.d/50-corvus.sh"

    cat > "$root/etc/motd" <<'EOF'
── Corvus integration-test outer node ───────────────────────
  crv               — host's crv pointed at the inner daemon
  crv-log           — tail the inner daemon's journal
  crv-status        — service state
  crv-psql          — connect to the inner Postgres
  crv-procs         — list daemon-spawned helpers
  sudo journalctl -u virtiofsd@\* -f   — virtiofs daemon logs
EOF
}

# Validate the artifact's boot kernel, which need not match the helper kernel.
check_node_kernel() {
    local kernel
    kernel=$(readlink "$root/boot/vmlinuz")
    case "$kernel" in
        vmlinuz-*) kernel_version=${kernel#vmlinuz-} ;;
        *) echo "Unexpected artifact kernel link: $kernel" >&2; return 1 ;;
    esac
    test -f "$root/boot/$kernel"
    test -d "$root/lib/modules/$kernel_version"
    grep -qE '^CONFIG_KVM_INTEL=[ym]|^CONFIG_KVM_AMD=[ym]' "$root/boot/config-$kernel_version" || {
        echo "Artifact kernel lacks KVM_INTEL/KVM_AMD" >&2
        return 1
    }
}

# Reset clone identity and record the installed target tool versions.
finalize_node_image() {
    reset_image_identity "$root"
    echo uninitialized > "$root/etc/machine-id"
    install -d -m 0755 "$root/var/lib/dbus"
    ln -sf /etc/machine-id "$root/var/lib/dbus/machine-id"
    write_image_metadata "$root" corvus-test-node
    {
        echo "kernel: $kernel_version"
        chroot "$root" qemu-system-x86_64 --version | sed -n '1p'
        chroot "$root" psql --version
        # Consume the entire version output: head can produce SIGPIPE under pipefail.
        chroot "$root" socat -V 2>&1 | sed -n '1p'
    } >> "$root/etc/corvus-image-info"
    cat "$root/etc/corvus-image-info"
}

# Retire transient target binds before removing scratch files, never host cache data.
clean_node_build_files() {
    # Target runtime policy was never changed to point at the helper's caches.
    unmount_tracked
    initialize_mount_tracking
    mount_at "$root" "${TARGET_DISK:-/dev/vdb}3"
    mount_at "$root/boot/efi" "${TARGET_DISK:-/dev/vdb}2"
    clean_image_temporary_files "$root"
}

# Build the node on a disposable helper without booting the artifact's init system.
main() {
    : "${IMAGE_ROOT:=/mnt/sysroot}"
    root=$IMAGE_ROOT
    IMAGE_LIBC=glibc
    workspace=
    postgres_active=false
    pgdata=/var/lib/postgresql/18/data
    pgbin=/usr/lib64/postgresql-18/bin
    unset ROOT SYSROOT PORTAGE_CONFIGROOT
    initialize_mount_tracking
    trap cleanup_test_build EXIT
    trap 'exit 130' INT
    trap 'exit 143' TERM
    trap 'exit 129' HUP
    check_build_prerequisites
    prepare_build_mounts
    install_node_packages
    initialize_postgres
    configure_vde_service
    configure_vde_network
    configure_nat
    configure_dhcp
    configure_node_user
    install_node_services
    configure_node_debugging
    check_node_kernel
    # Identity reset needs target passwd while its device interfaces are available.
    finalize_node_image
    clean_node_build_files
    trim_image_filesystems "$root"
}

main "$@"
