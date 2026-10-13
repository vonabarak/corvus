#!/usr/bin/env bash
source "$(dirname -- "${BASH_SOURCE[0]}")/../node-helper.sh"
exec "$checkout/.venv/bin/python" -m corvus_test_harness.devnode_create "$@"
