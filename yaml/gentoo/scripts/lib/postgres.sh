#!/bin/bash
# Definitions only; callers initialize state and own traps.

. "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/lib/mounts.sh"

# Stop the target server before releasing its private runtime filesystem.
postgres_stop() {
    if "$postgres_active"; then
        chroot "$root" runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -m fast -w stop || return $?
        postgres_active=false
    fi
}

# Initialize the target cluster and database without starting its init system.
postgres_initialize() {
    chroot "$root" eselect postgresql set 18
    # Run the installed package's configuration with target accounts, locale, and paths.
    env -u ROOT -u SYSROOT -u PORTAGE_TMPDIR -u DISTDIR -u PKGDIR \
        chroot "$root" emerge --config dev-db/postgresql:18
    systemctl --root="$root" enable postgresql-18.service
    chroot "$root" install -d -o postgres -g postgres /run/postgresql
    # Mark ownership before startup so mounts_cleanup also handles a partially failed start.
    postgres_active=true
    chroot "$root" runuser -u postgres -- "$pgbin/pg_ctl" -D "$pgdata" -w start \
        -o "-c listen_addresses='' -c unix_socket_directories=/run/postgresql"
    chroot "$root" runuser -u postgres -- "$pgbin/psql" -h /run/postgresql -v ON_ERROR_STOP=1 <<'SQL'
CREATE USER corvus SUPERUSER;
CREATE DATABASE corvus OWNER corvus;
SQL
    postgres_stop
}
