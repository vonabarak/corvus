#!/bin/bash
# Definitions only; callers initialize state and own traps.

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/image.sh"

# Reject toolchains, Portage, glibc, and systemd in any published musl runtime.
runtime_verify() {
    local root=${IMAGE_ROOT:?}
    local package
    local -a forbidden
    # A dependency change must fail the bake rather than silently ship a build toolchain.
    forbidden=(sys-devel/gcc sys-devel/binutils llvm-core/clang dev-lang/rust dev-lang/rust-bin dev-lang/go
        sys-apps/portage sys-libs/glibc sys-apps/systemd)
    for package in "${forbidden[@]}"; do
        if compgen -G "$root/var/db/pkg/${package}-[0-9]*" >/dev/null; then
            echo "Unexpected runtime package: $package" >&2
            exit 1
        fi
    done
}

# Remove documentation that is not needed by the minimal runtime.
runtime_remove_documentation() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/usr/share/doc" "$root/usr/share/man" "$root/usr/share/info"
}

# Keep package records and licenses while removing development files and static libraries.
runtime_remove_build_artifacts() {
    local root=${IMAGE_ROOT:?}
    rm -rf "$root/etc/portage" "$root/var/db/repos" "$root/usr/src" "$root/usr/include"
    local libdir
    for libdir in "$root/usr/lib" "$root/usr/lib64"; do
        if [[ -d "$libdir" ]]; then
            find "$libdir" -type f \( -name '*.a' -o -name '*.la' \) -delete
        fi
    done
}

# Apply explicit flavor policy, then finalize both runtimes identically.
runtime_finish() {
    if [[ "$IMAGE_LIBC" == musl ]]; then
        runtime_remove_documentation
        runtime_remove_build_artifacts
        runtime_verify
    fi
    image_clean_temporary_files "$IMAGE_ROOT"
    image_reset_identity "$IMAGE_ROOT"
    image_finalize "$IMAGE_ROOT" "${CORVUS_BUILD_NAME:?}"
}

