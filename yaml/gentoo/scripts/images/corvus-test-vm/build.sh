#!/bin/bash
set -euo pipefail
exec /bin/bash "${CORVUS_BUILD_SCRIPTS:-/usr/local/lib/corvus-build}/workflows/build-runtime.sh" "$@"
