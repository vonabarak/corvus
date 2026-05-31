#!/bin/bash
# One-time idempotent setup for the corvus-web dev loop.
#
# RUN ME INSIDE the corvus-dev-node VM (boot it with `make dev-node-vm`
# on the host, SSH in with `make dev-node-vm-ssh`, then:
#
#   bash /mnt/corvus/scripts/devnode/setup.sh
#
# What it does: bootstraps an admin CA inside the VM
# (~/.config/corvus/admin/), then runs `corvus-admin quickstart
# --skip-web` to mint the daemon/nodeagent/netd certs and start the
# inner services. corvus-web itself is NOT installed here — we run it
# from the live source under /mnt/corvus/python via scripts/devnode/web.sh.
#
# Host-side prereqs (one-time per dev-node lifetime):
#
#   make dev-node-vm
#   make dev-node-vm-ssh
#
# The browser reaches corvus-web at the VM's managed-network IP; see
# doc/dev-node.md.

set -euo pipefail

die() { echo "setup.sh: $*" >&2; exit 1; }

[ -d /mnt/corvus ]    || die "/mnt/corvus missing — run me inside corvus-dev-node"
[ -d /opt/corvus/bin ] || die "/opt/corvus/bin missing — run me inside corvus-dev-node"

CORVUS_PY=/mnt/corvus/python
corvus_admin() {
    PYTHONPATH="$CORVUS_PY" python3 -c \
        'from corvus_admin.cli import main; raise SystemExit(main())' "$@"
}

if [ ! -d "$HOME/.config/corvus/admin" ]; then
    echo "setup.sh: bootstrapping admin store with corvus-admin quickstart --skip-web"
    corvus_admin quickstart --skip-web
else
    echo "setup.sh: admin store already exists at $HOME/.config/corvus/admin — skipping quickstart"
fi

if systemctl --user is-active --quiet corvus.service; then
    echo "setup.sh: inner corvus.service is active"
else
    die "inner corvus.service is not active — check 'systemctl --user status corvus.service' and 'journalctl --user -u corvus -n 50'"
fi

echo "setup.sh: ready — run 'bash /mnt/corvus/scripts/devnode/web.sh' to start corvus-web"
