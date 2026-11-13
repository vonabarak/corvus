#!/bin/bash
# Isolated configuration, ordering, and failure tests; never run host Portage.
set -euo pipefail
here=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
export PORTAGE_TEST_WORK=$work TMPDIR=$work
initial_options=$(set +o)
initial_traps=$(trap -p)
. "$here/scripts/lib/portage.sh"
test "$(set +o)" = "$initial_options"
test "$(trap -p)" = "$initial_traps"

root="$work/root"
mkdir -p "$root/etc/portage"
printf '%s\n' 'FEATURES="binpkg-ignore-signature binpkg-request-signature"' \
    'EMERGE_DEFAULT_OPTS="--getbinpkg"' 'GPG_VERIFY_GROUP_DROP="nogroup"' \
    'USE="retained"' > "$root/etc/portage/make.conf"
export PORTAGE_KEYSERVER=hkps://example.org
portage_configure_binhost glibc "$root" /var/cache/binhost
cp -a "$root/etc/portage" "$work/first"
portage_configure_binhost glibc "$root" /var/cache/binhost
diff -ru "$work/first" "$root/etc/portage"
. "$root/etc/portage/make.conf"
[[ $FEATURES == '-binpkg-request-signature -binpkg-ignore-signature' ]]
[[ $USE == retained && $GPG_VERIFY_USER_DROP == '' && $GPG_VERIFY_GROUP_DROP == '' ]]
! grep -q EMERGE_DEFAULT_OPTS "$root/etc/portage/make.conf"
grep -q '^verify-signature = true$' "$root/etc/portage/binrepos.conf/zz-corvus-cache.conf"
grep -q '^location = /var/cache/binhost/x86-64$' "$root/etc/portage/binrepos.conf/zz-corvus-cache.conf"
grep -q '^sync-openpgp-keyserver = hkps://example.org$' "$root/etc/portage/repos.conf/zz-corvus-signatures.conf"
grep -q '^sync-webrsync-verify-signature = true$' "$root/etc/portage/repos.conf/zz-corvus-signatures.conf"
grep -q '^sync-rsync-verify-metamanifest = true$' "$root/etc/portage/repos.conf/zz-corvus-signatures.conf"
portage_configure_binhost musl "$root"
grep -q '/x86-64_musl$' "$root/etc/portage/binrepos.conf/zz-corvus-cache.conf"
portage_configure_build_host "$root"
. "$root/etc/portage/make.conf"
[[ $FEATURES == '-userfetch -userpriv -getbinpkg -binpkg-request-signature -binpkg-ignore-signature' ]]

mkdir "$work/bin"
cat > "$work/bin/emerge" <<'SCRIPT'
#!/bin/bash
set -eu
if [[ ${1:-} == --sync ]]; then
    echo sync >> "$PORTAGE_TEST_WORK/calls"
    test -z "${ROOT-}${SYSROOT-}"
    test "${SYNC_STATUS:-0}" = 0
else
    printf '%s\n' "$FEATURES" > "$PORTAGE_TEST_WORK/features"
    printf '%s\n' "$@" > "$PORTAGE_TEST_WORK/args"
    printf '%s\n' "${ROOT-}" "${SYSROOT-}" > "$PORTAGE_TEST_WORK/roots"
fi
SCRIPT
cat > "$work/bin/emerge-webrsync" <<'SCRIPT'
#!/bin/bash
set -eu
echo snapshot >> "$PORTAGE_TEST_WORK/calls"
test -z "${ROOT-}${SYSROOT-}"
# Also exercise the real adapter rather than merely inspecting its source.
command -v gemato > "$PORTAGE_TEST_WORK/adapter"
gemato openpgp-verify-detached -K trusted-key -- snapshot.gpgsig snapshot
exit "${SNAPSHOT_STATUS:-0}"
SCRIPT
cat > "$work/bin/gemato" <<'SCRIPT'
#!/bin/sh
printf '%s\n' "$@" > "$PORTAGE_TEST_WORK/gemato-args"
SCRIPT
chmod +x "$work/bin/"*
export PATH="$work/bin:$PATH"

# The wrapper preserves ROOT for target installation, removes SYSROOT, and
# defeats signature-disable settings inherited from the provisioner environment.
ROOT=/target SYSROOT=/wrong FEATURES='binpkg-ignore-signature binpkg-request-signature' \
    USEPKG=false GETBINPKG=true portage_emerge --root-deps app-misc/example
grep -q -- '--usepkg=n' "$work/args"
grep -q -- '--getbinpkg=y' "$work/args"
grep -q -- '--root-deps' "$work/args"
[[ $(sed -n 1p "$work/roots") == /target && -z $(sed -n 2p "$work/roots") ]]
[[ $(cat "$work/features") == *' -binpkg-request-signature -binpkg-ignore-signature -getbinpkg' ]]
if USEPKG=invalid portage_emerge app-misc/example 2>/dev/null; then exit 1; fi

# Missing prerequisites stop before any keyring/configuration changes.
(
    command() {
        if [[ $* == '-v gemato' ]]; then return 1; fi
        builtin command "$@"
    }
    if portage_check_verification_prerequisites 2> "$work/missing"; then exit 1; fi
)
grep -q 'Missing Gentoo verification prerequisite: gemato' "$work/missing"

# Exercise the live initializer with host operations replaced by isolated fakes.
(
    portage_check_verification_prerequisites() { echo prerequisites >> "$work/prepare"; }
    # Model a first sync with no repository trees: Portage queries cannot run.
    portageq() { echo 'Repository trees are absent' >&2; return 1; }
    gcc() { [[ $* == -dumpmachine && -z ${ROOT-}${SYSROOT-} ]]; echo "$test_chost"; }
    portage_configure_binhost() { echo "binhost $*" >> "$work/prepare"; }
    portage_configure_build_host() { echo "host $*" >> "$work/prepare"; }
    getuto() { [[ -z ${ROOT-}${SYSROOT-} ]]; echo getuto >> "$work/prepare"; }
    test_chost=x86_64-pc-linux-gnu
    ROOT=/target SYSROOT=/wrong portage_prepare_verification
    printf '%s\n' prerequisites 'binhost glibc /' 'host /' getuto > "$work/expected"
    diff "$work/expected" "$work/prepare"
    : > "$work/prepare"
    test_chost=x86_64-pc-linux-musl
    portage_prepare_verification
    grep -q '^binhost musl /$' "$work/prepare"
    gcc() { return 24; }
    if portage_prepare_verification; then exit 1; else [[ $? == 24 ]]; fi
    gcc() { echo unsupported-target; }
    if portage_prepare_verification 2> "$work/unsupported"; then exit 1; fi
    grep -q 'Unsupported build host CHOST: unsupported-target' "$work/unsupported"
    gcc() { echo "$test_chost"; }
    getuto() { return 23; }
    if portage_prepare_verification; then exit 1; else [[ $? == 23 ]]; fi
)

# Setup must precede both sync commands. Failures stop subsequent phases, even
# when the caller uses the library in a conditional (where errexit is ignored).
portage_prepare_verification() {
    echo prepare >> "$work/calls"
    return "${PREPARE_STATUS:-0}"
}
for scenario in success prepare snapshot rsync; do
    : > "$work/calls"
    export PREPARE_STATUS=0 SNAPSHOT_STATUS=0 SYNC_STATUS=0
    case "$scenario" in
        prepare) PREPARE_STATUS=17 ;;
        snapshot) SNAPSHOT_STATUS=18 ;;
        rsync) SYNC_STATUS=19 ;;
    esac
    original_path=$PATH
    original_traps=$(trap -p)
    if ROOT=/target SYSROOT=/wrong portage_sync_repository 2> "$work/errors"; then
        [[ $scenario == success ]]
    else
        status=$?
        case "$scenario" in
            prepare) [[ $status == 17 ]] ;;
            snapshot) [[ $status == 18 ]] ;;
            rsync) [[ $status == 1 ]] ;; # test exits 1 in the fake emerge
            *) exit 1 ;;
        esac
    fi
    [[ $PATH == "$original_path" && $(trap -p) == "$original_traps" ]]
    case "$scenario" in
        prepare) expected=prepare ;;
        snapshot) expected=$'prepare\nsnapshot' ;;
        rsync) expected=$'prepare\nsnapshot\nsync\nsync\nsync' ;;
        success) expected=$'prepare\nsnapshot\nsync' ;;
    esac
    [[ $(cat "$work/calls") == "$expected" ]]
    if [[ $scenario != prepare ]]; then
        test ! -e "$(cat "$work/adapter")"
        printf '%s\n' openpgp-verify-detached --no-wkd --keyserver \
            hkps://example.org -K trusted-key -- snapshot.gpgsig snapshot > "$work/expected"
        diff "$work/expected" "$work/gemato-args"
    fi
done
# Failure to create the scoped adapter must not fall through to an unwrapped sync.
(
    : > "$work/calls"
    PREPARE_STATUS=0
    mktemp() { return 22; }
    if portage_sync_repository; then exit 1; else [[ $? == 22 ]]; fi
    [[ $(cat "$work/calls") == prepare ]]
)
echo 'Portage helper tests passed'
