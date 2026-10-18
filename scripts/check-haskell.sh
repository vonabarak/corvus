#!/usr/bin/env bash
# Shared, sequential quality build. Positional arguments are STACK_BUILD_FLAGS.
set -euo pipefail

if [[ -n ${MATCH:-} ]]; then
  echo 'make check requires the complete Haskell suite; use make unit-tests MATCH=... for filtered tests' >&2
  exit 1
fi

project_root=$PWD
# Keep this a single path component: Stack cannot create a nested work directory
# for freshly unpacked dependencies whose .stack-work parent does not exist.
quality_dir=.stack-work-quality
packages=(corvus capnp capnp-tests lifetimes)
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
quality_stack build "${packages[@]}" --test --no-run-tests "${build_flags[@]}"
tool_stack exec -- corvus-code-metrics

# Stack reports package-relative dist paths, so ask from each package root.
package_roots=(. vendor/haskell-capnp/capnp vendor/haskell-capnp/capnp-tests vendor/lifetimes)
dist_dirs=()
weeder_args=()
for package_root in "${package_roots[@]}"; do
  dist_dir=$(cd "$package_root" && stack "${stack_flags[@]}" --stack-yaml "$project_root/stack.yaml" --work-dir "$quality_dir" path --dist-dir)
  dist_dir=$(realpath "$package_root/$dist_dir")
  test -d "$dist_dir/build"
  dist_dirs+=("$dist_dir")
  weeder_args+=(--hie-directory "$dist_dir/build")
  case "$package_root" in
    .) artifacts=(Corvus/Database.hie corvus/corvus-tmp/Main.hie crv/crv-tmp/Main.hie corvus-netd/corvus-netd-tmp/Main.hie corvus-nodeagent/corvus-nodeagent-tmp/Main.hie corvus-test/corvus-test-tmp/Main.hie) ;;
    vendor/haskell-capnp/capnp) artifacts=(Capnp.hie capnpc-haskell/capnpc-haskell-tmp/Main.hie) ;;
    vendor/haskell-capnp/capnp-tests) artifacts=(tests/tests-tmp/Main.hie) ;;
    vendor/lifetimes) artifacts=(Lifetimes.hie tests/tests-tmp/Main.hie) ;;
  esac
  for artifact in "${artifacts[@]}"; do
    if [[ ! -f $dist_dir/build/$artifact ]]; then
      echo "Missing HIE file: $dist_dir/build/$artifact; rebuild the quality directory" >&2
      exit 1
    fi
  done
done
# HIE stores source paths relative to each package. Supply a source mirror so
# --require-hs-files can locate the authored sources in every package.
source_mirror="$project_root/$quality_dir/weeder-sources"
rm -rf "$source_mirror"
mkdir -p "$source_mirror"
# capnp-tests also compiles calculator support from its neighboring package.
ln -sfn "$project_root/vendor/haskell-capnp/capnp-examples" "$project_root/$quality_dir/capnp-examples"
for package_root in "${package_roots[@]}"; do
  while IFS= read -r -d '' source; do
    relative=${source#"$package_root/"}
    destination="$source_mirror/$relative"
    mkdir -p "$(dirname "$destination")"
    if [[ ! -e $destination ]]; then
      ln -s "$project_root/$source" "$destination"
    fi
  done < <(find "$package_root" \( -name .stack-work -o -name .stack-work-quality \) -prune -o -path ./vendor -prune -o -name '*.hs' -print0)
done
weeder=$(tool_stack exec -- which weeder)
(cd "$source_mirror" && "$weeder" --config "$project_root/weeder.toml" "${weeder_args[@]}" --require-hs-files --no-default-fields)
tool_dist_dir=$(tool_stack path --dist-dir)
(
  cd tools
  stack "${stack_flags[@]}" --stack-yaml stack.yaml exec -- weeder --config ../weeder.toml \
    --hie-directory "$tool_dist_dir/build" --require-hs-files --no-default-fields
)

hpc_root=$(quality_stack path --local-hpc-root)
# Never accept traces left behind by earlier runs.
for package in "${packages[@]}"; do
  rm -rf "$hpc_root/$package"
done
rm -rf "$hpc_root/combined" "$quality_dir/authored-coverage"
TEST_DB_BACKEND=sqlite quality_stack test corvus capnp-tests lifetimes "${build_flags[@]}" --rerun-tests --test-arguments '--jobs=1 --seed=20261010'

package_db=$(quality_stack path --local-pkg-db)
coverage_args=()
for index in 0 1 3; do
  package=${packages[$index]}
  unit_id=$(quality_stack exec -- ghc-pkg --package-db="$package_db" field "$package" id --simple-output)
  test -n "$unit_id"
  case "$package" in
    corvus) suite=corvus-test ;;
    capnp) suite=tests; trace_package=capnp-tests ;;
    lifetimes) suite=tests ;;
  esac
  trace_package=${trace_package:-$package}
  trace="$hpc_root/$trace_package/$suite/$suite.tix"
  test -f "$trace"
  coverage_args+=("${package_roots[$index]}" "${dist_dirs[$index]}/hpc/$unit_id" "$trace")
  unset trace_package
done
tool_stack exec -- corvus-coverage-check coverage-baseline.json "$quality_dir/authored-coverage" "${coverage_args[@]}"
