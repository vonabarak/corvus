#!/bin/bash
# Definitions only; policy exclusive to the nested test guest.
test_vm_finish() {
    local package
    for package in app-admin/sudo dev-lang/python; do
        if compgen -G "$IMAGE_ROOT/var/db/pkg/${package}-[0-9]*" >/dev/null; then
            echo "Unexpected runtime package: $package" >&2
            return 1
        fi
    done
    test_vm_remove_package_metadata
    test_vm_remove_non_english_locales
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
