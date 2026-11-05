#!/bin/bash
# Definitions only; policy exclusive to the nested test guest.
test_vm_finish() {
    test_vm_verify_packages || return 1
    test_vm_remove_documentation
    test_vm_remove_build_artifacts
    test_vm_remove_package_metadata
    test_vm_remove_non_english_locales
}

# Reject toolchains and package managers before removing their package records.
test_vm_verify_packages() {
    local package
    for package in sys-devel/gcc sys-devel/binutils llvm-core/clang \
        dev-lang/rust dev-lang/rust-bin dev-lang/go sys-apps/portage \
        sys-libs/glibc sys-apps/systemd app-admin/sudo dev-lang/python; do
        if compgen -G "$IMAGE_ROOT/var/db/pkg/${package}-[0-9]*" >/dev/null; then
            echo "Unexpected runtime package: $package" >&2
            return 1
        fi
    done
}

# Drop package-manager state only from the immutable test guest, after verification.
test_vm_remove_package_metadata() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/var/db/pkg" "$root/var/lib/portage"
}

# Keep English gettext catalogs, including regional and modifier variants.
test_vm_remove_non_english_locales() {
    local root=${IMAGE_ROOT:?}
    local directory
    for directory in "$root/usr/share/locale" "$root/usr/lib/locale"; do
        if [[ -d "$directory" ]]; then
            find "$directory" -mindepth 1 -maxdepth 1 \
                ! -name en ! -name 'en_*' ! -name 'en@*' \
                ! -name C ! -name C.utf8 ! -name C.UTF-8 -exec rm -rf -- {} +
        fi
    done
}

# Remove documentation only from the minimal test guest.
test_vm_remove_documentation() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/usr/share/doc" "$root/usr/share/man" "$root/usr/share/info"
}

# Keep licenses and shared libraries while dropping development files.
test_vm_remove_build_artifacts() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/etc/portage" "$root/var/db/repos" "$root/usr/src" "$root/usr/include"
    local libdir
    for libdir in "$root/usr/lib" "$root/usr/lib64"; do
        if [[ -d "$libdir" ]]; then
            find "$libdir" -type f \( -name '*.a' -o -name '*.la' \) -delete
        fi
    done
}
