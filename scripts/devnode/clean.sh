#!/usr/bin/env bash
# Remove the dedicated network and host PKI after successful VM deletion.
source "$(dirname -- "${BASH_SOURCE[0]}")/../node-helper.sh"
exec "$checkout/.venv/bin/python" -m corvus_test_harness.host_access clean "$@"
