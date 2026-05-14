# Makefile for corvus project

.PHONY: all build install install-run uninstall cleanup unit-tests integration-tests all-tests test test-image test-image-alpine test-image-windows lint format capnp python-schema-sync python-test integration-tests-py integration-test-py integration-clean-py

# Add ~/.local/bin to PATH for tools like hlint and fourmolu
export PATH := $(HOME)/.local/bin:$(PATH)

# Number of parallel test jobs (override with: make integration-tests JOBS=8)
JOBS ?= 6

# Auto-retry failed integration tests at JOBS=1 (override with: RETRY=false)
RETRY ?= true
RETRY_FLAG := $(if $(filter false,$(RETRY)),--no-retry,)

# Default target: build
all: build

# Build the project
build:
	stack build

# Regenerate src-generated/Capnp/Gen/*.hs from schema/*.capnp.
# The capnp CLI invokes the `capnpc-haskell` plugin shipped with the
# capnp Haskell library; `stack exec` puts it on PATH after a build.
# Run this after editing any schema/*.capnp file and commit the result.
capnp:
	@command -v capnp >/dev/null 2>&1 || { \
	  echo "error: capnp compiler not on PATH (install dev-libs/capnproto)" >&2; \
	  exit 1; \
	}
	stack build capnp
	rm -rf src-generated
	mkdir -p src-generated
	stack exec --no-ghc-package-path -- env PATH="$$(stack path --compiler-bin):$$(stack path --local-install-root)/bin:$$PATH" \
	  capnp compile -ohaskell:src-generated --src-prefix=schema $$(ls schema/*.capnp)
	$(MAKE) python-schema-sync

# Mirror schema/*.capnp into python/corvus_client/schema/ so the Python
# client (which loads schemas at runtime via pycapnp) sees the same
# contract as the daemon. Run automatically as part of `make capnp`.
python-schema-sync:
	mkdir -p python/corvus_client/schema
	cp schema/*.capnp python/corvus_client/schema/

# Run the Python client's test suite against a real corvus daemon
# spawned per-test on a temp Unix socket. Depends on `install` so
# a fresh daemon binary is on $PATH.
python-test: install python-schema-sync
	cd python && .venv-corvus-py/bin/pytest tests -v

# Run the integration test suite (nested VMs; rootful inner Corvus;
# multi-node). Depends on `build` so the inner-side binary is fresh,
# and on `install` so `crv` is on PATH for the outer driver. The first
# session run will `crv apply` the integration-test image YAML, which
# takes 30-60 min if the upstream gentoo-base-headless isn't already
# in the daemon's disk store. Requires nested-KVM enabled on the host.
integration-tests-py: build install
	test -d tests-integration/.venv || python3 -m venv tests-integration/.venv
	tests-integration/.venv/bin/pip install -q -e ./python -e ./tests-integration
	tests-integration/.venv/bin/pytest tests-integration/tests -v

# Run a single integration test (or a subset). MATCH is passed to
# pytest's `-k` filter; it can be a test name, a substring, or a
# boolean expression over test names. Examples:
#   make integration-test-py MATCH=test_inner_daemon_reachable
#   make integration-test-py MATCH="lifecycle and not edit"
integration-test-py: build install
	@test -n "$(MATCH)" || { echo "usage: make integration-test-py MATCH=<pytest -k expr>" >&2; exit 2; }
	test -d tests-integration/.venv || python3 -m venv tests-integration/.venv
	tests-integration/.venv/bin/pip install -e ./python -e ./tests-integration
	tests-integration/.venv/bin/pytest tests-integration/tests -v -k "$(MATCH)"

# Sweep orphan integration-test VMs left behind by aborted test runs.
# pycapnp 2.x sometimes triggers SIGABRT on inner-daemon disconnects,
# which kills pytest before fixture teardown — VMs stay around. Names
# always start with `corvus-it-`; this target lists them and deletes
# each with --delete-disks (overlay rootfs goes too).
integration-clean-py:
	@names=$$(crv -o json vm list 2>/dev/null | jq -r '.[].name | select(startswith("corvus-it-"))'); \
	if [ -z "$$names" ]; then echo "no corvus-it-* VMs"; exit 0; fi; \
	echo "$$names" | while read -r n; do \
	  echo "deleting $$n"; \
	  crv vm reset "$$n" 2>/dev/null || true; \
	  crv vm delete --delete-disks "$$n" || true; \
	done

# Install binaries to ~/.local/bin/, drop the systemd unit file in
# place, and install shell completions. Does NOT enable, start, or
# restart the daemon — use `make install-run` for that. Splitting it
# this way lets you upgrade the binaries without bouncing a daemon
# that may be mid-task (build, apply, etc.).
install:
	stack install

# Install binaries (as `install` does), then enable + restart the
# corvus systemd user service. Use this when you want a freshly built
# daemon running immediately.
install-run: install
	mkdir -p $(HOME)/.config/systemd/user/
	cp corvus.service $(HOME)/.config/systemd/user/
	systemctl --user daemon-reload
	systemctl --user enable corvus.service
	systemctl --user restart corvus.service

	# Shell completions
	mkdir -p $(HOME)/.local/share/bash-completion/completions
	crv completion bash > $(HOME)/.local/share/bash-completion/completions/crv
	mkdir -p $(HOME)/.local/share/zsh/site-functions
	crv completion zsh > $(HOME)/.local/share/zsh/site-functions/_crv
	mkdir -p $(HOME)/.config/fish/completions
	crv completion fish > $(HOME)/.config/fish/completions/crv.fish

# Uninstall binaries, systemd service, and shell completions
uninstall:
	-systemctl --user stop corvus.service
	-systemctl --user disable corvus.service
	rm -f $(HOME)/.config/systemd/user/corvus.service
	-systemctl --user daemon-reload
	rm -f $(HOME)/.local/bin/corvus
	rm -f $(HOME)/.local/bin/crv
	rm -f $(HOME)/.local/share/bash-completion/completions/crv
	rm -f $(HOME)/.local/share/zsh/site-functions/_crv
	rm -f $(HOME)/.config/fish/completions/crv.fish

# Cleanup the project build artifacts and test cache
cleanup:
	stack clean
	rm -rf .test-cache

# Run only unit tests (excluding those with "Integration" in their name)
# Uses script(1) to provide a pseudo-terminal, preventing hangs when piping output.
unit-tests:
	script -qec 'stack test --test-arguments "--skip Integration"' /dev/null

# Run only integration tests (those with "Integration" in their name).
# By default, failed tests are retried at JOBS=1 — tests are considered failed
# only if the retry also fails. Disable with: make integration-tests RETRY=false.
integration-tests:
	script -qec 'scripts/run-tests-with-retry.sh $(RETRY_FLAG) --match Integration --jobs=$(JOBS)' /dev/null

# Run all tests
all-tests:
	script -qec 'stack test --test-arguments "--jobs=$(JOBS)"' /dev/null

# Run specific tests (e.g., make test MATCH="test name")
test:
	script -qec 'stack test --test-arguments "--match \"$(MATCH)\" --jobs=$(JOBS)"' /dev/null

# Run linter on src, app and test directories
lint:
	hlint src app test

# Build all test images via `crv build`
test-image: test-image-alpine test-image-windows

# Build the minimal Alpine integration-test image.
#
# Steps:
#   1. Generate the SSH keypair under ~/VMs/BaseImages/Alpine/ if it
#      doesn't exist (the keypair lives next to the baked image at a
#      stable host-side path).
#   2. Stage the public key next to the build YAML so the build's
#      `file: from: ./corvus-test-key.pub` provisioner finds it.
#   3. Apply the multi-OS template library (provides the `debian12`
#      bake VM template — it bootstraps Alpine via apk-tools-static
#      from inside the bake VM, so it doesn't need any pre-cached ISO).
#   4. Run `crv build`. The artifact is registered as a Corvus disk
#      named `corvus-test` at ~/VMs/BaseImages/Alpine/corvus-test.qcow2.
test-image-alpine:
	mkdir -p $(HOME)/VMs/BaseImages/Alpine
	test -f $(HOME)/VMs/BaseImages/Alpine/corvus-test-key || \
	  ssh-keygen -t ed25519 -f $(HOME)/VMs/BaseImages/Alpine/corvus-test-key -N '' -C corvus-test
	cp $(HOME)/VMs/BaseImages/Alpine/corvus-test-key.pub yaml/alpine-test/corvus-test-key.pub
	crv apply yaml/multi-os/multi-os.yml --skip-existing --wait
	crv build yaml/alpine-test/alpine-test.yml --wait
	rm -f yaml/alpine-test/corvus-test-key.pub

# Build the Windows Server 2025 test image.
#
# `crv apply` downloads the Microsoft evaluation ISO (~8 GiB) and the
# VirtIO-Win drivers ISO (~750 MiB) on first run; later applies are
# no-ops. The autounattend.xml floppy is materialised per-build by
# `crv build` from yaml/windows-server-2025/autounattend.xml — edit
# it freely; no manual mkfs.fat/mcopy. Bake takes 45–55 min on KVM.
# The artifact lands at ~/VMs/BaseImages/WindowsServer2025/.
test-image-windows:
	crv apply yaml/windows-server-2025/windows-installer.yml --skip-existing --wait
	crv build yaml/windows-server-2025/windows-server-2025.yml --wait

# Format the code using fourmolu
format:
	fourmolu --mode inplace $(shell find src app test -name '*.hs')
