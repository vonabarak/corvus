#!/bin/bash
# Install cloud-init for systemd/glibc and OpenRC/musl installations.
set -eu

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

# Install cloud-init into the target; native build tools belong to the builder.
install_packages() {
    if [ "$IMAGE_LIBC" = musl ]; then
        # Build dependencies stay on the builder; ROOT receives runtime packages.
        ROOT="$IMAGE_ROOT" portage_emerge --buildpkg --quiet-build app-emulation/cloud-init sys-fs/growpart app-admin/sudo app-alternatives/awk
        return
    fi
    ROOT="$IMAGE_ROOT" portage_emerge --buildpkg \
      --noreplace --binpkg-respect-use=y --quiet-build \
      app-emulation/cloud-init
}

# Schedule cloud-init boot stages using the target's init system.
enable_services() {
    if [ "$IMAGE_INIT" = openrc ]; then
        enable_cloudinit_services
        return
    fi
    systemctl --root="$IMAGE_ROOT" enable \
      cloud-init.target \
      cloud-init-main.service \
      cloud-init-local.service \
      cloud-config.service \
      cloud-final.service \
      cloud-init-hotplugd.socket
}

# Share NoCloud, default-user, and root-filesystem growth settings across both builds.
configure_datasource() {
    mkdir -p "$root/etc/cloud/cloud.cfg.d"
    cat > "$root/etc/cloud/cloud.cfg.d/90_corvus_nocloud.cfg" <<'CONF'
datasource_list: [NoCloud]
system_info:
  default_user:
    name: gentoo
    primary_group: users
    groups: [wheel]
    lock_passwd: true
    shell: /bin/bash
    sudo: ["ALL=(ALL) NOPASSWD:ALL"]
growpart:
  mode: auto
  devices: ['/']
  ignore_growroot_disabled: false
resize_rootfs: true
CONF
}

# Keep the baked locale; neither runtime needs cloud-init to invoke locale-gen.
configure_locale() {
    if [ "$IMAGE_LIBC" = musl ]; then
        printf '%s\n' 'locale: C.UTF-8' 'locale_configfile: /etc/locale.conf' \
            > "$root/etc/cloud/cloud.cfg.d/91_corvus_locale.cfg"
    else
        printf '%s\n' 'locale: false' > "$root/etc/cloud/cloud.cfg.d/91_corvus_locale.cfg"
    fi
    sed -i '/^[[:space:]]*-[[:space:]]*locale[[:space:]]*$/d' "$root/etc/cloud/cloud.cfg"
}

# dhcpcd owns OpenRC networking; retain systemd cloud-init networking behavior.
configure_networking() {
    if [ "$IMAGE_INIT" = openrc ]; then
        cat > "$root/etc/cloud/cloud.cfg.d/92_corvus_network.cfg" <<'CONF'
network:
  config: disabled
CONF
        adapt_cloudinit_for_openrc
    fi
}

# Select the native OpenRC service command in the installed Gentoo cloud-init backend.
adapt_cloudinit_for_openrc() {
    # Gentoo's cloud-init inherits the generic SysV "service" command. Select
    # OpenRC's native command without installing another service manager.
    distro=$(find "$root/usr/lib" -path '*/cloudinit/distros/gentoo.py' -print -quit)
    test -n "$distro"
    grep -q '^class Distro(distros.Distro):$' "$distro"
    sed -i '/^class Distro(distros.Distro):$/a\    init_cmd = ["rc-service"]' "$distro"
}

# Schedule NoCloud discovery before the remaining cloud-init boot stages.
enable_cloudinit_services() {
    # Order local discovery after syslog too: cloud-init's fingerprint helper
    # invokes logger, which needs /dev/log even before the default runlevel.
    mkdir -p "$root/etc/conf.d"
    printf '%s\n' 'rc_use="logger"' > "$root/etc/conf.d/cloud-init-local"
    chroot "$root" rc-update add cloud-init-local boot
    for service in cloud-init cloud-config cloud-final; do
        chroot "$root" rc-update add "$service" default
    done
}

# Install packages before enabling services and writing configuration.
main() {
    : "${IMAGE_ROOT:?}"
    IMAGE_LIBC=${IMAGE_LIBC:-glibc}
    case "$IMAGE_LIBC" in glibc|musl) ;; *) echo "Unsupported libc: $IMAGE_LIBC" >&2; exit 1 ;; esac
    if [ "$IMAGE_LIBC" = musl ]; then
        IMAGE_INIT=${IMAGE_INIT:-openrc}
    else
        IMAGE_INIT=${IMAGE_INIT:-systemd}
    fi
    case "$IMAGE_INIT" in systemd|openrc) ;; *) echo "Unsupported init: $IMAGE_INIT" >&2; exit 1 ;; esac
    root=$IMAGE_ROOT
    install_packages
    enable_services
    configure_datasource
    configure_locale
    configure_networking
}

main "$@"
