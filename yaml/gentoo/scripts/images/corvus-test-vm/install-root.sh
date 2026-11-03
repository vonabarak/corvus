#!/bin/bash
# Add integration-test capabilities to the shared minimal musl runtime.
set -euxo pipefail

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/portage.sh"

# Disable optional desktop and TPM policy stacks that the test guest never uses.
configure_package_features() {
    cat > /etc/portage/package.use/corvus-test-vm <<'CONF'
sys-apps/lshw -gtk
app-crypt/tpm2-tss -fapi -policy
app-crypt/tpm2-tools -fapi
app-admin/doas -pam
sys-apps/busybox -make-symlinks -pam -mdev -syslog
CONF
}

# Install runtime tools into the artifact; build dependencies stay on the builder.
install_test_packages() {
    ROOT="${IMAGE_ROOT:?}" portage_emerge --buildpkg --quiet-build \
        app-admin/doas \
        app-crypt/tpm2-tools \
        app-crypt/tpm2-tss \
        sys-power/acpid \
        sys-apps/diffutils \
        sys-apps/pciutils \
        sys-apps/lshw \
        sys-boot/efibootmgr \
        net-misc/iputils \
        sys-apps/busybox
}

# Expose only the DNS lookup applet; retain Gentoo's normal system utilities.
install_dns_lookup() {
    ln -s busybox "${IMAGE_ROOT:?}/bin/nslookup"
    chroot "$IMAGE_ROOT" /bin/busybox --list | grep -qx nslookup
}

# Gentoo's compiler package owns these runtime libraries; keep only the DSOs.
install_cxx_runtime() {
    local library source
    for library in libstdc++.so.6 libgcc_s.so.1; do
        source=$(gcc -print-file-name="$library")
        test -f "$source"
        install -m 0755 "$source" "${IMAGE_ROOT:?}/usr/lib/$library"
    done
    # Catch missing runtime dependencies before publishing an unusable image.
    chroot "$IMAGE_ROOT" /usr/sbin/lshw -version
}

# Use the caller's shared binary/distfile caches and native musl toolchain.
main() {
    configure_package_features
    install_test_packages
    install_cxx_runtime
    install_dns_lookup
}

main "$@"
