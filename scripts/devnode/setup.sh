#!/bin/bash
# One-time idempotent setup for the corvus-web dev loop.
#
# RUN ME INSIDE the corvus-dev-node VM (boot it with `make dev-node-vm-create`
# on the host, SSH in with `make dev-node-vm-ssh`, then:
#
#   bash /mnt/corvus/scripts/devnode/setup.sh
#
# Or run `make dev-node-vm-setup` on the host to execute this over SSH.
# Select the bundled PostgreSQL database with DEV_NODE_DATABASE=postgresql
# on either host setup target, or --database postgresql when running manually.
# SQLite is the default. Existing configurations must match the selection.
#
# What it does: bootstraps an admin CA inside the VM
# (~/.config/corvus/admin/), then runs `corvus-admin quickstart
# --skip-web` to mint the daemon/nodeagent/netd certs and start the
# inner services. corvus-web itself is NOT installed here — we run it
# from the live source under /mnt/corvus/python via scripts/devnode/web.sh.
#
# Host-side prereqs (one-time per dev-node lifetime):
#
#   make dev-node-vm    # creates, sets up, registers images, and opens SSH
#
# The browser reaches corvus-web at the VM's managed-network IP; see
# doc/dev-node.md.

set -euo pipefail

die() { echo "setup.sh: $*" >&2; exit 1; }

database=sqlite
while [ "$#" -gt 0 ]; do
    case "$1" in
        --database)
            [ "$#" -ge 2 ] || die "--database requires sqlite or postgresql"
            database=$2
            shift 2
            ;;
        *) die "unknown argument: $1 (usage: setup.sh [--database sqlite|postgresql])" ;;
    esac
done
case "$database" in
    sqlite|postgresql) ;;
    *) die "--database must be sqlite or postgresql" ;;
esac

[ -d /mnt/corvus ]    || die "/mnt/corvus missing — run me inside corvus-dev-node"
[ -d /opt/corvus/bin ] || die "/opt/corvus/bin missing — run me inside corvus-dev-node"

CORVUS_PY=/mnt/corvus/python
corvus_admin() {
    PYTHONPATH="$CORVUS_PY" python3 -c \
        'from corvus_admin.cli import main; raise SystemExit(main())' "$@"
}

if [ ! -d "$HOME/.config/corvus/admin" ]; then
    quickstart_args=(--skip-web)
    if [ "$database" = postgresql ]; then
        psql postgresql:///corvus -v ON_ERROR_STOP=1 -c 'SELECT 1' \
            || die "cannot access guest PostgreSQL database corvus; check 'sudo systemctl status postgresql-18.service' or start it with 'sudo systemctl start postgresql-18.service', then check the corvus role and database"
        quickstart_args+=(--database postgresql:///corvus)
    fi
    echo "setup.sh: bootstrapping admin store with $database"
    corvus_admin quickstart "${quickstart_args[@]}"
else
    exec_start=$(systemctl --user show corvus.service --property=ExecStart --value) \
        || die "cannot inspect the configured daemon; configure it manually or recreate a disposable VM"
    database_arg='--database[[:space:]]+([^[:space:];]+)'
    [[ "$exec_start" =~ $database_arg ]] \
        || die "cannot determine the configured database; configure it manually or recreate a disposable VM"
    configured_database=${BASH_REMATCH[1]}
    case "$configured_database" in
        postgresql://*|postgres://*) configured_backend=postgresql ;;
        *://*|--*) die "unrecognized configured database; inspect the daemon service manually" ;;
        *) configured_backend=sqlite ;;
    esac
    [ "$configured_backend" = "$database" ] \
        || die "configured backend is $configured_backend, requested $database; no data was migrated. Recreate a disposable VM with make dev-node-vm-clean (use DEV_NODE_VM for a custom name), or configure the backend manually"
    echo "setup.sh: admin store already exists; reusing $configured_backend configuration"
fi

if systemctl --user is-active --quiet corvus.service; then
    echo "setup.sh: inner corvus.service is active"
else
    die "inner corvus.service is not active — check 'systemctl --user status corvus.service' and 'journalctl --user -u corvus -n 50'"
fi

echo "setup.sh: ready — run 'bash /mnt/corvus/scripts/devnode/web.sh' to start corvus-web"
