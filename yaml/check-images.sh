#!/bin/bash
# Exit 3 means a known output is missing; all other failures are operational.
set -euo pipefail
missing=0
for output in "$@"; do
  kind=${output%%:*}
  name=${output#*:}
  if result=$("${CRV:-crv}" -o json "$kind" show "$name"); then
    jq -e '.id | type == "number"' <<< "$result" >/dev/null
  elif jq -e --arg code "${kind}_not_found" '.code == $code' <<< "$result" >/dev/null; then
    printf 'Missing %s: %s\n' "$kind" "$name" >&2
    missing=3
  else
    printf '%s\n' "$result" >&2
    exit 1
  fi
done
exit "$missing"
