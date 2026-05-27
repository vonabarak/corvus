# Makefile for corvus project

.PHONY: all build install uninstall cleanup unit-tests integration-tests integration-tests-clean test-image test-image-key test-image-vm test-image-vm-clean test-image-node test-image-node-clean dev-node-vm dev-node-vm-clean dev-node-vm-ssh test-image-multi-os test-image-windows test-image-windows-clean test-image-installer test-image-installer-clean lint format capnp python-test release release-clean

# Add ~/.local/bin to PATH for tools like hlint and fourmolu
export PATH := $(HOME)/.local/bin:$(PATH)

# Worker count for `make integration-tests`. When unset, the recipe auto-detects
# from host CPU + MemAvailable via integration_tests/scripts/detect_workers.py;
# override explicitly with: make integration-tests WORKERS=4
WORKERS ?=

# Extra flags appended to every stack invocation. Empty for the
# dev tree (ghcup + stack do the right thing); CI overrides
# with `STACK_BUILD_FLAGS='--system-ghc --no-install-ghc'` so
# stack reuses the runner-provisioned GHC instead of trying to
# install its own. Honoured by `build`, `unit-tests`, `capnp`,
# `install`, `release` — the targets that invoke `stack`.
STACK_BUILD_FLAGS ?=

# Default target: build
all: build

# Build the project
build:
	stack build $(STACK_BUILD_FLAGS)

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
integration-tests: build
	test -d integration_tests/.venv || python3 -m venv integration_tests/.venv
	# One editable install at the repo root with the `harness` extra
	# replaces the three separate installs (corvus_client, corvus_admin,
	# corvus_test_harness now ship as a single distribution).
	integration_tests/.venv/bin/pip install -q -e .[harness]
	@workers="$(WORKERS)"; \
	  if [ -z "$$workers" ]; then \
	    workers=$$(python3 integration_tests/scripts/detect_workers.py --explain); \
	  fi; \
	  echo "integration tests: $$workers parallel workers"; \
	  integration_tests/.venv/bin/pytest integration_tests/tests -v \
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
	script -qec 'stack test $(STACK_BUILD_FLAGS) --test-arguments "--jobs=$(shell nproc)$(if $(MATCH), --match \"$(MATCH)\",)"' /dev/null

# Umbrella: build (or no-op) every disk the integration-test suite
# needs — the corvus test-node, the inner Alpine test-vm, the
# multi-OS cloud base images, and Windows Server 2025. Each
# prerequisite is individually idempotent (a `crv disk show` guard
# wraps every bake), so re-running this target on a warm tree
# costs a handful of `crv disk show` calls; on a cold tree first
# run is ~2 hours total (Gentoo + Windows are the long bakes).
#
# The pytest harness expects these images to be present and fails
# fast otherwise — see `ImageReady.ensure()` and the
# `register_base_images()` flow. The build path lives here so the
# bake never races between pytest-xdist workers.
test-image: test-image-node test-image-vm test-image-multi-os test-image-windows test-image-installer

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
# Prereqs (declared as make deps so we don't redo them each invocation):
#   1. test-image-key — the shared SSH keypair at
#      integration_tests/keys/corvus-test-key{,.pub}.
#   2. test-image-multi-os — registers the `debian12` bake template
#      this VM is overlaid on (it bootstraps Alpine via
#      apk-tools-static from inside the bake VM, so it doesn't need
#      any pre-cached ISO).
#
# Idempotent: a `crv disk show` guard skips the bake when
# `corvus-test-vm` is already registered. The artifact lands at
# `BaseImages/Alpine/corvus-test-vm.qcow2` under the daemon's
# BaseImages tree. The build's `file:` step reads the pubkey
# directly out of `integration_tests/keys/`.
test-image-vm: test-image-key test-image-multi-os
	@crv -o json disk show corvus-test-vm >/dev/null 2>&1 || \
	  crv build yaml/corvus-test-vm/corvus-test-vm.yml --wait
test-image-vm-clean:
	crv disk delete corvus-test-vm || true

# Build the Gentoo test-node image (the harness's outer VM).
#
# Prereq: test-image-key — the shared SSH keypair at
# integration_tests/keys/corvus-test-key{,.pub}, baked into
# /home/corvus/.ssh/authorized_keys.
#
# Two independently-guarded bakes (each skipped when its target
# disk is already registered):
#   1. yaml/gentoo-test/gentoo-headless.yml — produces the
#      `gentoo-base-headless` disk plus the `gentoo-cloud` and
#      `gentoo-headless` templates the test-node build is an
#      overlay on. First-run cost is the headless bake itself
#      (~30-60 min: kernel + stage3 + emerges).
#   2. yaml/corvus-test-node/corvus-test-node.yml — produces the
#      `corvus-test-node` disk consumed by the harness's
#      `ImageReady.ensure()` check.
#
# To force a rebake, `crv disk delete <name>` first; the guard
# will then re-run the corresponding `crv build`.
test-image-node: test-image-key
	@crv -o json disk show gentoo-base-headless >/dev/null 2>&1 || \
	  crv build yaml/gentoo-test/gentoo-headless.yml --wait
	@crv -o json disk show corvus-test-node >/dev/null 2>&1 || \
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
	@crv -o json disk show windows-server-2025-eval >/dev/null 2>&1 || \
	  crv build yaml/windows-server-2025/windows-server-2025.yml --wait
test-image-windows-clean:
	crv disk delete windows-server-2025-eval || true
	crv template delete windows-server-2025 || true

# Build the synthetic installer-strategy ISO used by
# `integration_tests/tests/test_build_installer.py`.
#
# The ISO is tiny (~13 MB): an Alpine `linux-virt` kernel +
# busybox-static initramfs + isolinux. Its PID 1 (sourced from
# yaml/corvus-test-installer/init.sh) mounts the floppy supplied
# by `crv build`, copies the marker payload onto /dev/vda, and
# powers off — exercising Corvus's `installer` build path
# end-to-end without any vendor installer.
#
# The bake itself lives in `scripts/build-synthetic-installer.sh`
# (kernel + busybox + syslinux fetched from Alpine's CDN and
# cached under `build/synthetic-installer-cache/`), not in a
# `crv build` pipeline — assembling a 13 MB ISO from scratch in a
# bake VM would be many minutes for no benefit. Idempotent: a
# `crv disk show` guard skips the assembly when the disk is
# already registered.
test-image-installer:
	@crv -o json disk show corvus-test-installer-iso >/dev/null 2>&1 || \
	  scripts/build-synthetic-installer.sh
test-image-installer-clean:
	crv disk delete corvus-test-installer-iso || true
	crv template delete corvus-test-installer || true


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
# The overlay is template-instantiated, so it's ephemeral by default
# and `vm delete` reaps it together with the VM.
dev-node-vm-clean:
	-crv vm reset $(DEV_NODE_VM)
	-crv vm delete $(DEV_NODE_VM)


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


# Read-only verification. Lints Python (ruff check + mypy) and Haskell
# (hlint), plus a `--check` pass of every formatter (Ruff + fourmolu)
# that exits non-zero if any file would be reformatted. Does NOT edit
# code — suited for CI / pre-merge gates and pre-push hooks. Run
# `make format` first to fix any formatting violations this flags.
lint:
	hlint src app test
	fourmolu --mode check $(shell find src app test -name '*.hs')
	$(RUFF) check python integration_tests
	$(RUFF) format --check python integration_tests
	$(MYPY) python integration_tests


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


# Stage a self-contained release tree under release/ and tarball it.
# Same recipe the GitHub Release workflow runs; producing it
# locally is the way to dry-run a release before pushing the tag.
#
# Layout (per the release workflow's docstring):
#
#   release/
#     corvus-<VERSION>-linux-amd64/
#       bin/                    # 4 stripped Haskell binaries
#       completions/{bash,zsh,fish}/
#       python/                 # corvus-<pyver>.{whl,tar.gz}
#       doc/                    # verbatim from the source tree
#       yaml/                   # every example
#       schema/                 # capnp schemas pycapnp loads at runtime
#       scripts/build-synthetic-installer.sh
#       README.md
#       INSTALL.md
#       VERSION
#     corvus-<VERSION>-linux-amd64.tar.gz   # the tarball
#     python/                   # standalone python release assets
#       corvus-<pyver>-py3-none-any.whl
#       corvus-<pyver>.tar.gz
#
# VERSION default is derived from the cabal/package.yaml version
# field; CI overrides it from the git tag via `make release
# VERSION=0.10.1`. The Python wheel/sdist version is independent
# (lives in pyproject.toml).
VERSION ?= $(shell awk '/^version:/ {print $$2; exit}' package.yaml)
RELEASE_DIR := release/corvus-$(VERSION)-linux-amd64
RELEASE_TARBALL := release/corvus-$(VERSION)-linux-amd64.tar.gz

release: build
	# Fresh staging tree on every invocation.
	rm -rf release
	mkdir -p $(RELEASE_DIR)/bin
	mkdir -p $(RELEASE_DIR)/completions/bash
	mkdir -p $(RELEASE_DIR)/completions/zsh
	mkdir -p $(RELEASE_DIR)/completions/fish
	mkdir -p $(RELEASE_DIR)/python
	mkdir -p $(RELEASE_DIR)/scripts
	mkdir -p release/python
	#
	# 1. Binaries. `stack path --local-install-root` resolves to
	#    the same prefix the integration-test harness and
	#    `make install` use. STACK_BUILD_FLAGS must be passed here
	#    too — `stack path` reads the same platform-tag logic
	#    `stack build` does, and on CI without `--system-ghc` it
	#    refuses with "No compiler found, expected minor version
	#    match with ghc-9.8.4 (x86_64-tinfo6)" even when the
	#    global config has `system-ghc: true` (stack 3.x quirk).
	@bindir=$$(stack $(STACK_BUILD_FLAGS) path --local-install-root)/bin; \
	  for b in corvus crv corvus-netd corvus-nodeagent; do \
	    cp $$bindir/$$b $(RELEASE_DIR)/bin/$$b; \
	    strip $(RELEASE_DIR)/bin/$$b; \
	  done
	#
	# 2. Shell completions. Re-use the binary's own completion
	#    generator (same source the `install` target uses).
	$(RELEASE_DIR)/bin/crv completion bash > $(RELEASE_DIR)/completions/bash/crv
	$(RELEASE_DIR)/bin/crv completion zsh  > $(RELEASE_DIR)/completions/zsh/_crv
	$(RELEASE_DIR)/bin/crv completion fish > $(RELEASE_DIR)/completions/fish/crv.fish
	#
	# 3. Python wheel + sdist. The standard PEP 517 frontend
	#    drives the build via the project's setuptools backend
	#    (see pyproject.toml). Copies land both inside the
	#    tarball-staged tree AND in release/python/ for upload
	#    as standalone GitHub Release assets. The `build`
	#    package is installed into the project venv at
	#    python/.venv-corvus-py/ (the same venv `make lint` /
	#    `make python-test` use); create it if missing.
	@if [ ! -x python/.venv-corvus-py/bin/python3 ]; then \
	  python3 -m venv python/.venv-corvus-py ; \
	fi
	python/.venv-corvus-py/bin/pip install --quiet --upgrade pip build
	python/.venv-corvus-py/bin/python3 -m build --sdist --wheel --outdir release/python .
	cp release/python/*.whl release/python/*.tar.gz $(RELEASE_DIR)/python/
	#
	# 4. Verbatim source trees: docs, every YAML example, the
	#    Cap'n Proto schemas pycapnp loads at runtime, the
	#    synthetic-installer helper script, and the top-level
	#    README.
	cp -r doc $(RELEASE_DIR)/doc
	cp -r yaml $(RELEASE_DIR)/yaml
	cp -r schema $(RELEASE_DIR)/schema
	cp scripts/build-synthetic-installer.sh $(RELEASE_DIR)/scripts/
	cp README.md $(RELEASE_DIR)/README.md
	#
	# 5. Version stamp + a short pointer to the existing docs.
	echo "$(VERSION)" > $(RELEASE_DIR)/VERSION
	@printf '%s\n' \
	  '# Corvus $(VERSION)' \
	  '' \
	  '1. Drop `bin/*` somewhere on `$$PATH` (e.g. `/usr/local/bin`).' \
	  '2. `pip install python/corvus-*.whl` for the client library + `corvus-admin` CLI.' \
	  '3. Run `corvus-admin quickstart` for a single-node setup, or follow `doc/multi-node.md` for a cluster.' \
	  '' \
	  'Shell completions are under `completions/{bash,zsh,fish}/`.' \
	  'See `doc/INDEX.md` for the full documentation tree.' \
	  > $(RELEASE_DIR)/INSTALL.md
	#
	# 6. Tarball. `--owner=root --group=root` keeps the archive
	#    portable so untar on the operator's host doesn't carry
	#    the build user's uid.
	tar -czf $(RELEASE_TARBALL) \
	  --owner=root --group=root \
	  -C release \
	  corvus-$(VERSION)-linux-amd64
	@echo ""
	@echo "Release artifacts:"
	@echo "  $(RELEASE_TARBALL)"
	@ls release/python/

# Remove the staged release tree.
release-clean:
	rm -rf release
