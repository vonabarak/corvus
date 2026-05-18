# Makefile for corvus project

.PHONY: all build install install-run install-system uninstall uninstall-system cleanup unit-tests integration-tests integration-tests-clean test-image test-image-key test-image-vm test-image-vm-clean test-image-node test-image-node-clean test-image-multi-os test-image-windows test-image-windows-clean lint format capnp python-schema-sync python-test

# Add ~/.local/bin to PATH for tools like hlint and fourmolu
export PATH := $(HOME)/.local/bin:$(PATH)

# Number of parallel test jobs (override with: make unit-tests JOBS=8)
JOBS ?= 6

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

# Run the pytest integration test suite (nested VMs; rootful inner
# Corvus; multi-node). Depends on `build` so the inner-side binary
# is fresh, and on `install` so `crv` is on PATH for the outer
# driver. The first session run will `crv build` the integration-
# test image YAML (~30-60 min cold). Requires nested-KVM on the host.
#
# No arguments: runs the whole suite in parallel (-n auto).
# MATCH=<expr>: runs the pytest -k filter (substring or boolean expr).
#
# Examples:
#   make integration-tests
#   make integration-tests MATCH=test_apply
#   make integration-tests MATCH="lifecycle and not edit"
integration-tests: build install
	test -d integration_tests/.venv || python3 -m venv integration_tests/.venv
	integration_tests/.venv/bin/pip install -q -e ./python -e ./integration_tests
	integration_tests/.venv/bin/pytest integration_tests/tests -v \
	  $(if $(MATCH),-k "$(MATCH)",-n auto)

# Sweep orphan integration-test VMs left behind by aborted test runs.
# pycapnp 2.x sometimes triggers SIGABRT on inner-daemon disconnects,
# which kills pytest before fixture teardown — VMs stay around. Names
# always start with `corvus-it-`; this target lists them and deletes
# each with --delete-disks (overlay rootfs goes too).
integration-tests-clean:
	@names=$$(crv -o json vm list 2>/dev/null | jq -r '.[].name | select(startswith("corvus-it-"))'); \
	if [ -z "$$names" ]; then echo "no corvus-it-* VMs"; exit 0; fi; \
	echo "$$names" | while read -r n; do \
	  echo "deleting $$n"; \
	  crv vm reset "$$n" 2>/dev/null || true; \
	  crv vm delete --delete-disks "$$n" || true; \
	done

# Run the Haskell unit-test suite. Integration tests have moved to
# pytest under integration_tests/ (see `make integration-tests`);
# the Haskell test/ tree is unit-tests-only now.
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
	script -qec 'stack test --test-arguments "--jobs=$(JOBS)$(if $(MATCH), --match \"$(MATCH)\",)"' /dev/null

# Build all test images via `crv build`
test-image: test-image-node test-image-vm test-image-multi-os test-image-windows

# Fetch the multi-OS cloud images (Debian, Ubuntu, AlmaLinux, FreeBSD,
# Alpine) used by the cloud-init integration test class. Idempotent:
# `crv apply --skip-existing` no-ops once the disks are registered.
test-image-multi-os:
	crv apply yaml/multi-os/multi-os.yml --skip-existing --wait

# Generate the SSH keypair the integration-test images embed.
#
# Both the Alpine test image and the Gentoo integration-test image
# inject the same `corvus-test-key.pub` into authorized_keys at bake
# time, so the host-side SSH tunnel can reach the inner Alpine VM
# through the outer Gentoo VM with a single keypair. Idempotent:
# subsequent runs no-op if the key already exists.
test-image-key:
	mkdir -p integration_tests/keys
	test -f integration_tests/keys/corvus-test-key || \
	  ssh-keygen -t ed25519 -f integration_tests/keys/corvus-test-key -N '' -C corvus-test

# Build the minimal Alpine test-VM image (the inner VM the harness
# boots inside the outer test node).
#
# Steps:
#   1. Make sure the shared SSH keypair exists at
#      integration_tests/keys/corvus-test-key{,.pub}.
#   2. Apply the multi-OS template library (provides the `debian12`
#      bake VM template — it bootstraps Alpine via apk-tools-static
#      from inside the bake VM, so it doesn't need any pre-cached ISO).
#   3. Run `crv build`. The artifact is registered as a Corvus disk
#      named `corvus-test-vm` under the daemon's BaseImages tree at
#      `BaseImages/Alpine/corvus-test-vm.qcow2`. The build's `file:` step
#      reads the pubkey directly out of `integration_tests/keys/`.
test-image-vm: test-image-key
	crv apply yaml/multi-os/multi-os.yml --skip-existing --wait
	crv build yaml/corvus-test-vm/corvus-test-vm.yml --wait
test-image-vm-clean:
	crv disk delete corvus-test-vm || true

# Build the Gentoo test-node image (the harness's outer VM).
#
# Steps:
#   1. Make sure the shared SSH keypair exists at
#      integration_tests/keys/corvus-test-key{,.pub} — the image
#      bakes its public half into /home/corvus/.ssh/authorized_keys.
#   2. Drop any prior `corvus-test-node` disk so the build always
#      rebakes (the YAML changes we land here only take effect
#      after a fresh bake).
#   3. Run yaml/gentoo-test/gentoo-headless.yml — registers the
#      OVMF + gentoo-base-cloud disks and bakes both the
#      `gentoo-cloud` and `gentoo-headless` templates the
#      test-node build is an overlay on. First-run cost is
#      the headless bake itself (~30-60 min: kernel + stage3 +
#      emerges); later runs no-op via `ifExists: skip`.
#   4. Run `crv build` on yaml/corvus-test-node/corvus-test-node.yml.
#      The artifact is registered as a Corvus disk named
#      `corvus-test-node`. The pytest harness's `ImageReady.ensure()`
#      reuses it instead of baking a fresh one on first run.
test-image-node: test-image-key
	crv build yaml/gentoo-test/gentoo-headless.yml --wait
	crv build yaml/corvus-test-node/corvus-test-node.yml --wait
test-image-node-clean:
	crv disk delete corvus-test-node || true

# Build the Windows Server 2025 test image.
#
# windows-server-2025.yml is a self-contained pipeline: its first
# `apply` step downloads the Microsoft evaluation ISO (~8 GiB) and the
# VirtIO-Win drivers ISO (~750 MiB) on first run (later runs are
# no-ops via `ifExists: skip`), the `build` step drives the autounattend
# install end-to-end, and a final `apply` registers a convenience
# `windows-server-2025` runtime template that overlays the baked image.
# The autounattend.xml floppy is materialised per-build by `crv build`
# from yaml/windows-server-2025/autounattend.xml — edit it freely; no
# manual mkfs.fat/mcopy. Bake takes 45–55 min on KVM. The artifact
# lands at ~/VMs/BaseImages/WindowsServer2025/.
test-image-windows:
	crv build yaml/windows-server-2025/windows-server-2025.yml --wait
test-image-windows-clean:
	crv disk delete windows-server-2025-eval || true
	crv template delete windows-server-2025 || true

# Run linter on src, app and test directories
lint:
	hlint src app test

# Format the code using fourmolu
format:
	fourmolu --mode inplace $(shell find src app test -name '*.hs')

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

	sleep 1
	crv status

# Install the system-wide privileged network agent (corvus-netd).
# Requires root: copies the binary to /usr/local/bin/, drops the
# system systemd unit in place, reloads, enables, and starts it.
#
# Run after `make install` so the binary is built and present in
# ~/.local/bin. The system unit copies it into /usr/local/bin so
# system systemd can find it regardless of the invoking user's home.
install-system: install
	install -d /usr/local/bin
	install -m 0755 $(HOME)/.local/bin/corvus-netd /usr/local/bin/corvus-netd
	install -d /etc/systemd/system
	install -m 0644 systemd/corvus-netd.service /etc/systemd/system/corvus-netd.service
	systemctl daemon-reload
	systemctl enable corvus-netd.service
	systemctl restart corvus-netd.service

# Stop, disable, and remove the system-wide agent.
uninstall-system:
	-systemctl stop corvus-netd.service
	-systemctl disable corvus-netd.service
	rm -f /etc/systemd/system/corvus-netd.service
	-systemctl daemon-reload
	rm -f /usr/local/bin/corvus-netd

# Uninstall binaries, systemd service, and shell completions
uninstall:
	-systemctl --user stop corvus.service
	-systemctl --user disable corvus.service
	rm -f $(HOME)/.config/systemd/user/corvus.service
	-systemctl --user daemon-reload
	rm -f $(HOME)/.local/bin/corvus
	rm -f $(HOME)/.local/bin/crv
	rm -f $(HOME)/.local/bin/corvus-netd
	rm -f $(HOME)/.local/share/bash-completion/completions/crv
	rm -f $(HOME)/.local/share/zsh/site-functions/_crv
	rm -f $(HOME)/.config/fish/completions/crv.fish

# Cleanup the project build artifacts and test cache
cleanup:
	stack clean
	rm -rf .test-cache
