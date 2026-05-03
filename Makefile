# Makefile for corvus project

.PHONY: all build install uninstall cleanup unit-tests integration-tests all-tests test test-image test-image-alpine test-image-windows lint format python-codegen python-lib python-test

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

# Install binaries to ~/.local/bin/ and setup systemd user service
install:
	stack install
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
#   1. Generate the SSH keypair under .test-images/ if it doesn't exist
#      (the integration tests expect the private key at a stable path).
#   2. Stage the public key next to the build YAML so the build's
#      `file: from: ./corvus-test-key.pub` provisioner finds it.
#   3. Apply the multi-OS template library (provides the `debian12`
#      bake VM template — it bootstraps Alpine via apk-tools-static
#      from inside the bake VM, so it doesn't need any pre-cached ISO).
#   4. Run `crv build`. The artifact is registered as a Corvus disk
#      named `corvus-test`.
test-image-alpine:
	mkdir -p .test-images
	test -f .test-images/corvus-test-key || \
	  ssh-keygen -t ed25519 -f .test-images/corvus-test-key -N '' -C corvus-test
	cp .test-images/corvus-test-key.pub yaml/alpine-test/corvus-test-key.pub
	crv apply yaml/multi-os/multi-os.yml --skip-existing
	crv build yaml/alpine-test/alpine-test.yml --wait
	rm -f yaml/alpine-test/corvus-test-key.pub
	# Symlink the registered artifact into .test-images/ so the
	# integration-test harness (test/Test/VM/Image.hs) finds it.
	@artifact=$$(crv -o yaml disk show corvus-test \
	             | awk '/^file_path:/ {print $$2; exit}'); \
	  ln -sf "$$artifact" .test-images/corvus-test.qcow2; \
	  echo "linked .test-images/corvus-test.qcow2 -> $$artifact"

# Build the Windows Server 2025 test image.
#
# `crv apply` downloads the Microsoft evaluation ISO (~8 GiB) and the
# VirtIO-Win drivers ISO (~750 MiB) on first run; later applies are
# no-ops. The autounattend.xml floppy is materialised per-build by
# `crv build` from yaml/windows-server-2025/autounattend.xml — edit
# it freely; no manual mkfs.fat/mcopy. Bake takes 45–55 min on KVM.
test-image-windows:
	crv apply yaml/windows-server-2025/windows-installer.yml --skip-existing
	crv build yaml/windows-server-2025/windows-server-2025.yml --wait
	mkdir -p .test-images
	@artifact=$$(crv -o yaml disk show windows-server-2025-eval \
	             | awk '/^file_path:/ {print $$2; exit}'); \
	  ln -sf "$$artifact" .test-images/windows-server-eval.qcow2; \
	  echo "linked .test-images/windows-server-eval.qcow2 -> $$artifact"

# Format the code using fourmolu
format:
	fourmolu --mode inplace $(shell find src app test -name '*.hs')

# Regenerate python/corvus_client/_generated.py from the Haskell
# Request / Response types. Must run whenever src/Corvus/Protocol.hs
# changes.
#
# The Python extension + codegen executable are gated behind the cabal
# flag `python-client` (default: off). These targets enable it so the
# foreign-library and gen-python-client are built; plain `stack build`
# still works without pulling ginger / file-embed.
#
# We'd prefer `stack build :gen-python-client` but component-level
# targets don't work reliably when hpack and the committed .cabal file
# disagree on version, so we build everything and lean on ccache.
python-codegen:
	stack build --flag corvus:python-client
	stack exec gen-python-client > python/corvus_client/_generated.py

# Build the Haskell-backed Python extension and drop it into
# python/corvus_client/_corvus.abi3.so so pytest can import it.
# Stack doesn't expose foreign-library components as explicit targets
# and its incremental-rebuild tracking for ffi-src/ is unreliable; we
# use --force-dirty to guarantee the foreign-library is rebuilt when
# sources under ffi-src/ or cbits/ change.
python-lib: python-codegen
	stack build --flag corvus:python-client --force-dirty
	@built=$$(find .stack-work/install -name 'libcorvus-python.so' -print -quit); \
	  if [ -z "$$built" ]; then \
	    echo "libcorvus-python.so not found under .stack-work/install"; exit 1; \
	  fi; \
	  echo "built: $$built"; \
	  cp "$$built" python/corvus_client/_corvus.abi3.so

# Run the Python skeleton tests. Requires a venv at python/.venv-corvus-py
# with pytest installed (see doc/python-client.md for setup).
python-test: python-lib
	python/.venv-corvus-py/bin/pytest python/tests/ -v
