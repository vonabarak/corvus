#!/usr/bin/env bash
# Host-side registration against an already configured and running dev node.
source "$(dirname -- "${BASH_SOURCE[0]}")/../node-helper.sh"
exec "$checkout/.venv/bin/python" -m corvus_test_harness.devnode_images "$@"
