#!/usr/bin/env bash
# Regenerate maintained Cap'n Proto bindings with the local generator.
set -euo pipefail
cd "$(dirname "$0")/.."
stack "$@" build capnp:exe:capnpc-haskell
plugin=$(stack "$@" path --local-install-root)/bin/capnpc-haskell
root=$PWD/vendor/haskell-capnp
mkdir -p "$root/capnp-tests/gen/tests" "$root/capnp-examples/gen/lib"
capnp compile -I "$root/core-schema" --src-prefix="$root/core-schema" \
  -o"$plugin:$root/capnp/gen/lib" "$root"/core-schema/capnp/*.capnp "$root"/core-schema/capnp/compat/*.capnp
capnp compile -I "$root/core-schema" --src-prefix="$root/capnp-tests/tests/data" \
  -o"$plugin:$root/capnp-tests/gen/tests" "$root"/capnp-tests/tests/data/{aircraft,generics}.capnp
capnp compile -I "$root/core-schema" --src-prefix="$root/capnp-examples" \
  -o"$plugin:$root/capnp-examples/gen/lib" "$root"/capnp-examples/{calculator,echo}.capnp
