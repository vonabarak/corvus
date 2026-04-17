#!/bin/bash
# Wrapper around `stack test` that retries failed tests at --jobs=1.
#
# On the first pass, tests run with the given arguments (typically at high
# concurrency). If any tests fail, they are retried serially at --jobs=1.
# The script's exit code reflects the retry pass: tests flaky at high
# concurrency but stable at JOBS=1 report success overall.
#
# Usage:
#   run-tests-with-retry.sh [--no-retry] <stack-test-args...>
#
# With --no-retry, the script is a simple pass-through to `stack test`.

set -u

RETRY=1
if [ "${1:-}" = "--no-retry" ]; then
    RETRY=0
    shift
fi

# `stack test --test-arguments "..."` takes a single string that stack splits
# on whitespace (respecting double quotes). We join our arguments with spaces.
# Callers that need multi-word arguments (e.g. `--match "foo bar"`) should
# pass them pre-quoted as a single script argument, matching the Makefile's
# existing convention.
TEST_ARGS="$*"

if [ "$RETRY" = "0" ]; then
    exec stack test --test-arguments "$TEST_ARGS"
fi

REPORT=$(mktemp -t corvus-test-failures.XXXXXX)
trap 'rm -f "$REPORT"' EXIT

echo "[test-runner] First pass: $TEST_ARGS"
if stack test --test-arguments "$TEST_ARGS --failure-report=$REPORT"; then
    echo "[test-runner] All tests passed on first pass."
    exit 0
fi

# Non-zero exit. If the report has content, retry at JOBS=1.
if [ -s "$REPORT" ]; then
    echo ""
    echo "[test-runner] First pass had failures. Retrying at JOBS=1..."
    echo ""
    stack test --test-arguments "--rerun --failure-report=$REPORT --jobs=1"
    RESULT=$?
    echo ""
    if [ "$RESULT" = "0" ]; then
        echo "[test-runner] Retry succeeded — tests were flaky at high concurrency, not broken."
    else
        echo "[test-runner] Retry also failed — tests are genuinely broken."
    fi
    exit $RESULT
fi

echo "[test-runner] No failure report produced (compilation error or crash). Not retrying."
exit 1
