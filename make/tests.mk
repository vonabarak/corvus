.PHONY: test unit-tests python-test integration-tests integration-tests-clean

# Worker count for `make integration-tests`. When unset, the recipe auto-detects
# from host CPU + MemAvailable via integration_tests/scripts/detect_workers.py;
# override explicitly with: make integration-tests WORKERS=4
WORKERS ?=

# Run BOTH Python pytest suites (corvus_client + corvus_admin) against
# a real corvus daemon spawned per-test on a temp Unix socket. Both
# packages share the consolidated /python/tests tree and one editable
# install at the repo root covers both. The conftest discovers the
# freshly built daemon from `stack path --local-install-root` — no
# `make install` required.
python-test: build venv
	PYTHONPATH=python:integration_tests .venv/bin/pytest python/tests -v

# Run the pytest integration test suite (nested VMs; rootful inner
# Corvus; multi-node). The orchestrator VMs mount the host's
# `stack path --local-install-root`/bin/ over virtiofs at
# /opt/corvus/bin, so `make build` refreshes those binaries before the suite
# starts. Install and run the outer daemon separately. Prepare missing
# fixtures with `make image-ensure IMAGE=<name>` before running tests;
# the harness never starts image bakes. Requires nested-KVM on the host.
#
# No arguments: runs the whole suite in parallel, with the worker count
# chosen by integration_tests/scripts/detect_workers.py — the smaller of
# (logical CPUs / 2) and (MemAvailable / 3 GiB), minimum 1. Each worker
# boots a nested VM, so RAM is the usual bottleneck and a naive
# `-n auto` (one worker per logical CPU) tends to OOM the host.
#
# MATCH=<expr>: runs the pytest -k filter (substring or boolean expr).
#               Suppresses `-n` so the matched tests run sequentially.
# WORKERS=<N>:  bypass the detector and pin the worker count.
#
# Examples:
#   make integration-tests
#   make integration-tests WORKERS=4
#   make integration-tests MATCH=test_apply
#   make integration-tests MATCH="lifecycle and not edit"
integration-tests: build venv
	@workers="$(WORKERS)"; \
	  if [ -z "$$workers" ]; then \
	    workers=$$(python3 integration_tests/scripts/detect_workers.py --explain); \
	  fi; \
	  echo "integration tests: $$workers parallel workers"; \
	  PYTHONPATH=python:integration_tests .venv/bin/pytest integration_tests/tests -v -W always \
	    $(if $(MATCH),-k "$(MATCH)",-n $$workers)

# Sweep orphan integration-test VMs left behind by aborted test runs.
# pycapnp 2.x sometimes triggers SIGABRT on inner-daemon disconnects,
# which kills pytest before fixture teardown — VMs stay around. Names
# always start with `corvus-it-`; this target lists them and deletes
# each (the harness marks overlay rootfs disks as ephemeral, so they
# go with the VM by default).
integration-tests-clean:
	@names=$$(crv -o json vm list 2>/dev/null | jq -r '.[].name | select(startswith("corvus-it-"))'); \
	if [ -n "$$names" ]; then \
	  echo "$$names" | while read -r n; do \
	    echo "deleting VM $$n"; \
	    crv vm reset "$$n" 2>/dev/null || true; \
	    crv vm delete "$$n" || true; \
	  done; \
	else \
	  echo "no corvus-it-* VMs"; \
	fi
	@nets=$$(crv -o json network list 2>/dev/null | jq -r '.[].name | select(startswith("corvus-it-"))'); \
	if [ -n "$$nets" ]; then \
	  echo "$$nets" | while read -r n; do \
	    echo "deleting network $$n"; \
	    crv network stop "$$n" --force 2>/dev/null || true; \
	    crv network delete "$$n" || true; \
	  done; \
	else \
	  echo "no corvus-it-* networks"; \
	fi

# Umbrella: run every test suite in escalating-cost order so a
# failure in the cheap pure-Haskell layer surfaces before we
# burn minutes booting nested VMs. Make stops on the first
# non-zero exit, so a busted `unit-tests` skips the Python +
# integration phases.
#
#   1. unit-tests          — pure Haskell, ~10s.
#   2. python-test         — Python client + corvus-admin against
#                            a per-test daemon on a temp socket,
#                            ~minutes.
#   3. integration-tests   — nested-VM end-to-end suite, ~6+ min.
#
# This is the local equivalent of CI's pre-merge gate.
test: unit-tests python-test integration-tests

# Run the Haskell unit-test suite. Integration tests have moved to
# pytest under integration_tests/ (see `make integration-tests`);
# the Haskell test/ tree is unit-tests-only now. Hspec job count
# auto-detects from `nproc` — pure-Haskell tests are CPU-bound, so
# one worker per logical CPU is the right default.
#
# No arguments: runs the whole suite.
# MATCH=<expr>: runs only tests whose name matches <expr>.
#
# Examples:
#   make unit-tests
#   make unit-tests MATCH=SnapshotSpec
#   make unit-tests MATCH="cloud-init: persists"
#
# Uses script(1) to provide a pseudo-terminal, preventing hangs when piping output.
unit-tests:
	script -qec 'stack $(STACK_BUILD_FLAGS) --stack-yaml tools/stack.yaml test --test-arguments "$(if $(MATCH),--match \"$(MATCH)\",)"' /dev/null
	script -qec 'stack test $(STACK_BUILD_FLAGS) --test-arguments "--jobs=$(shell nproc)$(if $(MATCH), --match \"$(MATCH)\",)"' /dev/null
