#!/usr/bin/env bash
# Host setup: private temporary PKI, SSH deployment, and inner-node registration.
# Usage: setup.sh [--vm NAME_OR_ID] [--ssh-key PATH] [--database sqlite|postgresql]
# The CA key stays on the host. Role/client credentials are deployed to the guest.
if [[ -d /mnt/corvus && -d /opt/corvus/bin ]]; then
    echo "setup.sh runs on the host; use corvus-admin quickstart for manual guest setup" >&2
    exit 1
fi
source "$(dirname -- "${BASH_SOURCE[0]}")/../node-helper.sh"
exec "$checkout/.venv/bin/python" -m corvus_test_harness.devnode_setup "$@"
