# Makefile for corvus project

.PHONY: all build install uninstall cleanup unit-tests integration-tests integration-tests-clean test-image test-image-key test-image-vm test-image-vm-clean test-image-node test-image-node-clean dev-node-vm dev-node-vm-clean dev-node-vm-ssh test-image-multi-os test-image-windows test-image-windows-clean lint format check capnp python-test

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

# Note: python/corvus_client/schema is a directory symlink to ../../schema,
# so the Python client (which loads schemas at runtime via pycapnp) and the
# wheel-build's `package-data` glob both see the regenerated `.capnp` files
# with no extra step.

# Run BOTH Python pytest suites (corvus_client + corvus_admin) against
# a real corvus daemon spawned per-test on a temp Unix socket. Both
# packages share the consolidated /python/tests tree and one editable
# install at the repo root covers both. The conftest discovers the
# freshly built daemon from `stack path --local-install-root` — no
# `make install` required.
python-test: build
	@if [ -x python/.venv-corvus-py/bin/pytest ]; then \
	  python/.venv-corvus-py/bin/pip install --quiet -e . ; \
	  python/.venv-corvus-py/bin/pytest python/tests -v ; \
	else \
	  python3 -m pytest python/tests -v ; \
	fi

# Run the pytest integration test suite (nested VMs; rootful inner
# Corvus; multi-node). The orchestrator VMs mount the host's
# `stack path --local-install-root`/bin/ over virtiofs at
# /opt/corvus/bin, so a `stack build` is the only thing the suite
# needs — the user-driven `make install` step (which copies to
# $HOME/.local/bin) is *not* a dependency here, and the harness
# resolves `crv` from the same stack-install path. The first
# session run will `crv build` the integration-test image YAML
# (~30-60 min cold). Requires nested-KVM on the host.
#
# No arguments: runs the whole suite in parallel (-n auto).
# MATCH=<expr>: runs the pytest -k filter (substring or boolean expr).
#
# Examples:
#   make integration-tests
#   make integration-tests MATCH=test_apply
#   make integration-tests MATCH="lifecycle and not edit"
integration-tests: build
	test -d integration_tests/.venv || python3 -m venv integration_tests/.venv
	# One editable install at the repo root with the `harness` extra
	# replaces the three separate installs (corvus_client, corvus_admin,
	# corvus_test_harness now ship as a single distribution).
	integration_tests/.venv/bin/pip install -q -e .[harness]
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
	crv template delete corvus-test-node || true
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


# Instantiate a one-off VM from the `corvus-test-node` template
# (registered by `make test-image-node`), attach the developer's
# freshly built binaries over virtiofs at the `corvus_host` tag —
# matching the image's `opt-corvus-bin.mount` — and start it. The
# `corvus_host` mount makes /opt/corvus/bin inside the VM the
# *same* tree as `stack path --local-install-root`/bin/ on the
# host, so a `stack build` on the host is reflected in the VM
# without a rebake.
#
# After it boots, the daemon inside is reachable over VSOCK
# (the image runs corvus-tcp-relay.service: VSOCK:9876 → TCP:9876)
# and SSH is up via systemd's VSOCK socket-activation for the
# `corvus` user.
#
# Idempotent: if the VM already exists the instantiate / shared-dir
# / start steps are skipped. Run `make dev-node-vm-clean` first to
# get a fresh VM.

# Override to rename the manual-testing VM, e.g.:
#   make dev-node-vm DEV_NODE_VM=corvus-foo
DEV_NODE_VM ?= corvus-dev-node
# Private half of the SSH key the image bakes into
# /home/corvus/.ssh/authorized_keys at build time
# (see yaml/corvus-test-node/corvus-test-node.yml).
DEV_NODE_SSH_KEY ?= integration_tests/keys/corvus-test-key

dev-node-vm: build
	@if crv -o json vm show $(DEV_NODE_VM) >/dev/null 2>&1; then \
	  echo "VM '$(DEV_NODE_VM)' already exists; leaving it alone."; \
	  echo "Run 'make dev-node-vm-clean' to remove it first if you want a fresh VM."; \
	else \
	  stack_bin=$$(stack path --local-install-root)/bin; \
	  echo "Instantiating template 'corvus-test-node' as VM '$(DEV_NODE_VM)'"; \
	  crv template instantiate corvus-test-node $(DEV_NODE_VM); \
	  echo "Attaching $$stack_bin -> corvus_host (read-only)"; \
	  crv shared-dir add $(DEV_NODE_VM) "$$stack_bin" corvus_host --read-only; \
	  echo "Attaching $(CURDIR) -> corvus_src (read-only, mounted at /mnt/corvus)"; \
	  crv shared-dir add $(DEV_NODE_VM) "$(CURDIR)" corvus_src --read-only; \
	  echo "Attaching $$HOME/VMs/BaseImages -> base_images (read-only, mounted at /home/corvus/VMs/BaseImages)"; \
	  crv shared-dir add $(DEV_NODE_VM) "$$HOME/VMs/BaseImages" base_images --read-only; \
	  echo "Starting $(DEV_NODE_VM)"; \
	  crv vm start $(DEV_NODE_VM) --wait; \
	fi
	@echo
	@echo "VM is ready. Use:"
	@echo "  crv vm show $(DEV_NODE_VM)     # connection details (vsock CID, sockets)"
	@echo "  make dev-node-vm-ssh           # ssh in over VSOCK"
	@echo "  make dev-node-vm-clean         # reset + delete the VM (and its overlay)"


# Reset + delete the manual-testing VM and its overlay disk.
dev-node-vm-clean:
	-crv vm reset $(DEV_NODE_VM)
	-crv vm delete --delete-disks $(DEV_NODE_VM)


# SSH into the dev VM over VSOCK. Pulls the CID from `crv vm show`
# and uses the test image's baked-in SSH key. Pass extra ssh args
# via `make dev-node-vm-ssh SSH_ARGS='-v'`.
dev-node-vm-ssh:
	@cid=$$(crv -o json vm show $(DEV_NODE_VM) | jq -r '.vsock_cid // empty'); \
	  if [ -z "$$cid" ]; then \
	    echo "$(DEV_NODE_VM) has no vsock_cid (is the VM up?)" >&2; \
	    exit 1; \
	  fi; \
	  exec ssh -i $(DEV_NODE_SSH_KEY) $(SSH_ARGS) corvus@vsock%$$cid


# Python tool resolution: prefer the venv copies when they exist —
# the venv has the project's runtime deps (pycapnp, pyyaml, click,
# pytest, types-PyYAML stubs) so mypy resolves third-party types
# correctly. Falls back to PATH for users who set up tooling
# globally (e.g. via pipx) and never created the venv.
MYPY  ?= $(if $(wildcard python/.venv-corvus-py/bin/mypy),python/.venv-corvus-py/bin/mypy,mypy)
RUFF  ?= $(if $(wildcard python/.venv-corvus-py/bin/ruff),python/.venv-corvus-py/bin/ruff,ruff)

# Format Python (ruff) + Haskell (fourmolu) sources in place.
format:
	$(RUFF) format python integration_tests
	fourmolu --mode inplace $(shell find src app test -name '*.hs')


# Lint Python (ruff check + mypy) + Haskell (hlint). Mypy lives here
# so a single `make lint` covers both style and types end-to-end.
lint:
	$(RUFF) check python integration_tests
	$(MYPY) python integration_tests
	hlint src app test


# Read-only verification. Runs every check `make lint` does, plus a
# `--check` pass of every formatter (Ruff + fourmolu) that exits
# non-zero if any file would be reformatted. Does NOT edit code —
# suited for CI / pre-merge gates and pre-push hooks.
check:
	$(RUFF) check python integration_tests
	$(RUFF) format --check python integration_tests
	$(MYPY) python integration_tests
	hlint src app test
	fourmolu --mode check $(shell find src app test -name '*.hs')


# Place Haskell binaries on $PATH, install shell completions, and
# pipx-install the corvus-admin Python CLI. Nothing else: CA
# generation, cert deployment, systemd unit files, and service
# bring-up live in `corvus-admin quickstart`. Run it once after
# `make install` for a turn-key single-node setup.
#
# The CA private key corvus-admin manages lives under
# $XDG_CONFIG_HOME/corvus/admin/ — see python/corvus_admin/.
install:
	stack install

	# Shell completions
	mkdir -p $(HOME)/.local/share/bash-completion/completions
	$(HOME)/.local/bin/crv completion bash > $(HOME)/.local/share/bash-completion/completions/crv
	mkdir -p $(HOME)/.local/share/zsh/site-functions
	$(HOME)/.local/bin/crv completion zsh > $(HOME)/.local/share/zsh/site-functions/_crv
	mkdir -p $(HOME)/.config/fish/completions
	$(HOME)/.local/bin/crv completion fish > $(HOME)/.config/fish/completions/crv.fish

	# corvus-admin CLI — pipx-installed from the root distribution.
	@if command -v pipx >/dev/null 2>&1; then \
	  pipx install --force . ; \
	else \
	  echo "pipx not found; falling back to pip --user" >&2 ; \
	  python3 -m pip install --user --upgrade . ; \
	fi

	@echo ""
	@echo "Haskell binaries + corvus-admin installed."
	@echo "Next step: run 'corvus-admin quickstart' to set up a single-node deployment."

# Remove the binaries that `make install` placed. Service teardown
# is corvus-admin's job (`systemctl --user disable corvus.service`
# etc.) — `make uninstall` does *not* touch generated unit files
# or certs, since the operator may have edited them.
uninstall:
	rm -f $(HOME)/.local/bin/corvus
	rm -f $(HOME)/.local/bin/corvus-nodeagent
	rm -f $(HOME)/.local/bin/corvus-netd
	rm -f $(HOME)/.local/bin/crv
	@if command -v pipx >/dev/null 2>&1; then \
	  pipx uninstall corvus 2>/dev/null || true; \
	fi
	@echo "Binaries removed. Generated systemd units + certs are left in place;"
	@echo "remove them by hand if no longer needed:"
	@echo "  rm -f ~/.config/systemd/user/corvus*.service /etc/systemd/system/corvus*.service"
	@echo "  rm -rf ~/.config/corvus /etc/corvus"
	rm -f $(HOME)/.local/bin/corvus-netd
	rm -f $(HOME)/.local/bin/corvus-nodeagent
	rm -f $(HOME)/.local/share/bash-completion/completions/crv
	rm -f $(HOME)/.local/share/zsh/site-functions/_crv
	rm -f $(HOME)/.config/fish/completions/crv.fish


# Cleanup the project build artifacts and test cache
cleanup:
	stack clean
	rm -rf .test-cache
