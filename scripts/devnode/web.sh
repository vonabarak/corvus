#!/bin/bash
# Run corvus-web inside the corvus-dev-node VM against the live source
# at /mnt/corvus/python — no install, no venv, just PYTHONPATH.
#
# RUN ME INSIDE the corvus-dev-node VM. Run scripts/devnode/setup.sh
# first to bootstrap the inner daemon.
#
#   bash /mnt/corvus/scripts/devnode/web.sh           # defaults
#   bash /mnt/corvus/scripts/devnode/web.sh --help    # see corvus-web flags
#
# Env-var overrides:
#   CORVUS_WEB_PORT       (default 8080)
#   CORVUS_WEB_LOG_LEVEL  (default debug)
#
# Extra args are passed through to corvus-web — e.g. `--daemon-host
# 127.0.0.1` to use the vsock-relay TCP instead of the Unix socket.

set -euo pipefail

die() { echo "web.sh: $*" >&2; exit 1; }

[ -d /mnt/corvus ] || die "/mnt/corvus missing — run me inside corvus-dev-node"

CORVUS_PY=/mnt/corvus/python

frontend_dir=$CORVUS_PY/corvus_web/static
if [ -f /mnt/corvus/frontend/dist/index.html ]; then
    frontend_dir=/mnt/corvus/frontend/dist
fi

port=${CORVUS_WEB_PORT:-8080}
vm_ip=$(ip -4 -o addr show 2>/dev/null \
    | awk '$2 ~ /^enp/ {split($4, a, "/"); print a[1]; exit}')
if [ -n "$vm_ip" ]; then
    echo "web.sh: open http://$vm_ip:$port/ in your host browser"
else
    echo "web.sh: no enp* interface has an IPv4 address yet — check 'ip -4 addr'" >&2
fi

exec env PYTHONPATH="$CORVUS_PY" python3 -c \
    'from corvus_web.cli import main; raise SystemExit(main())' \
    --bind-host 0.0.0.0 \
    --bind-port "$port" \
    --log-level "${CORVUS_WEB_LOG_LEVEL:-debug}" \
    --frontend-dir "$frontend_dir" \
    "$@"
