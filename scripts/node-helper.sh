#!/usr/bin/env bash
# Source from host entry points to use the checkout's Python environment.
set -euo pipefail
checkout=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
if [ ! -x "$checkout/.venv/bin/python" ]; then
    make -C "$checkout" venv
fi
export PYTHONPATH="$checkout/python:$checkout/integration_tests${PYTHONPATH:+:$PYTHONPATH}"
