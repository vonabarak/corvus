#!/bin/bash
# Host-side registration against an already configured and running dev node.
set -euo pipefail
checkout=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
if [ ! -x "$checkout/.venv/bin/python" ]; then
    make -C "$checkout" venv
fi
export PYTHONPATH="$checkout/python:$checkout/integration_tests${PYTHONPATH:+:$PYTHONPATH}"
exec "$checkout/.venv/bin/python" -m corvus_test_harness.devnode_images "$@"
