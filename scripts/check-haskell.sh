#!/usr/bin/env bash
# Shared, sequential quality build. Positional arguments are STACK_BUILD_FLAGS.
set -euo pipefail

if [[ -n ${MATCH:-} ]]; then
  echo 'make check requires the complete Haskell suite; use make unit-tests MATCH=... for filtered tests' >&2
  exit 1
fi

quality_dir=.stack-work/quality
quality_stack() {
  stack "${stack_flags[@]}" --work-dir "$quality_dir" "$@"
}
stack_flags=("$@")
build_flags=(--flag corvus:with-sqlite --flag corvus:with-postgresql --coverage --ghc-options=-fwrite-ide-info)

# Build development tools with the compiler used for application HIE files.
tool_stack() {
  stack "${stack_flags[@]}" --stack-yaml tools/stack.yaml "$@"
}
tool_stack build weeder corvus-tools --test --no-run-tests --ghc-options=-fwrite-ide-info
tool_stack test corvus-tools --rerun-tests --ghc-options=-fwrite-ide-info
quality_stack build corvus --test --no-run-tests "${build_flags[@]}"
tool_stack exec -- corvus-code-metrics

dist_dir=$(quality_stack path --dist-dir)
test -f "$dist_dir/build/Corvus/Database.hie"
for component in corvus crv corvus-netd corvus-nodeagent corvus-test; do
  if ! find "$dist_dir/build/$component" -name '*.hie' -print -quit | grep -q .; then
    echo "Missing HIE files for $component; rebuild the quality directory" >&2
    exit 1
  fi
done
test -f weeder.toml
tool_stack exec -- weeder --config weeder.toml --hie-directory "$dist_dir/build" --require-hs-files --no-default-fields
tool_dist_dir=$(tool_stack path --dist-dir)
(
  cd tools
  stack "${stack_flags[@]}" --stack-yaml stack.yaml exec -- weeder --config ../weeder.toml \
    --hie-directory "$tool_dist_dir/build" --require-hs-files --no-default-fields
)

hpc_root=$(quality_stack path --local-hpc-root)
# Never accept a trace left behind by an earlier test run.
rm -rf "$hpc_root/corvus/corvus-test" "$hpc_root/combined" "$quality_dir/authored-coverage"
TEST_DB_BACKEND=sqlite quality_stack test corvus:test:corvus-test "${build_flags[@]}" --rerun-tests --test-arguments '--jobs=1 --seed=20261010'

package_db=$(quality_stack path --local-pkg-db)
unit_id=$(quality_stack exec -- ghc-pkg --package-db="$package_db" field corvus id --simple-output)
test -n "$unit_id"
tool_stack exec -- corvus-coverage-check coverage-baseline.json "$dist_dir/hpc/$unit_id" \
  "$hpc_root/corvus/corvus-test/corvus-test.tix" "$quality_dir/authored-coverage"
