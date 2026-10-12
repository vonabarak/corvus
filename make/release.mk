.PHONY: release release-clean set-version

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
#       schema/                 # capnp schemas pycapnp loads at runtime
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
	# Web UI: build the frontend bundle so it lands in
	# python/corvus_web/static/ — the wheel embeds it as package
	# data (see pyproject.toml's `corvus_web = ["static/**/*"]`)
	# and step 4a below stages the same tree at the top level
	# of the tarball. Mandatory for release: `web-build` errors
	# loudly if npm is missing. Operator-side `make install`
	# keeps a silent skip — only releases require the SPA.
	$(MAKE) web-build
	# Fresh staging tree on every invocation.
	rm -rf release
	mkdir -p $(RELEASE_DIR)/bin
	mkdir -p $(RELEASE_DIR)/completions/bash
	mkdir -p $(RELEASE_DIR)/completions/zsh
	mkdir -p $(RELEASE_DIR)/completions/fish
	mkdir -p $(RELEASE_DIR)/python
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
	# corvus-admin completions: invoke the package via -m, no install
	# needed. PYTHONPATH lets us run straight from the source tree.
	PYTHONPATH=python python3 -m corvus_admin completion bash > $(RELEASE_DIR)/completions/bash/corvus-admin
	PYTHONPATH=python python3 -m corvus_admin completion zsh  > $(RELEASE_DIR)/completions/zsh/_corvus-admin
	PYTHONPATH=python python3 -m corvus_admin completion fish > $(RELEASE_DIR)/completions/fish/corvus-admin.fish
	#
	# 3. Python wheel + sdist. The standard PEP 517 frontend
	#    drives the build via the project's setuptools backend
	#    (see pyproject.toml). Copies land both inside the
	#    tarball-staged tree AND in release/python/ for upload
	#    as standalone GitHub Release assets. The `build`
	#    package is installed into the shared project venv at .venv/;
	#    create it if missing.
	@if [ ! -x .venv/bin/python3 ]; then \
	  python3 -m venv .venv ; \
	fi
	.venv/bin/python3 -m pip install --quiet --upgrade pip build
	.venv/bin/python3 -m build --sdist --wheel --outdir release/python .
	cp release/python/*.whl release/python/*.tar.gz $(RELEASE_DIR)/python/
	#
	# 4. Verbatim source trees: docs, every YAML example, the
	#    Cap'n Proto schemas pycapnp loads at runtime, the
	#    synthetic-installer helper (included in yaml), and the top-level
	#    README.
	cp -r doc $(RELEASE_DIR)/doc
	cp -r yaml $(RELEASE_DIR)/yaml
	cp -r schema $(RELEASE_DIR)/schema
	cp README.md $(RELEASE_DIR)/README.md
	#
	# 4a. Built SPA. Mirrors python/corvus_web/static/ (populated
	#     by the `web-build` step above) so operators can serve
	#     the SPA standalone via nginx / Caddy / similar without
	#     installing the Python wheel. The same tree is also
	#     embedded in the wheel via pyproject.toml's
	#     `corvus_web = ["static/**/*"]`.
	mkdir -p $(RELEASE_DIR)/web
	cp -r python/corvus_web/static/. $(RELEASE_DIR)/web/
	#
	# 5. Version stamp + a short pointer to the existing docs.
	echo "$(VERSION)" > $(RELEASE_DIR)/VERSION
	@printf '%s\n' \
	  '# Corvus $(VERSION)' \
	  '' \
	  '1. Drop `bin/*` somewhere on `$$PATH` (e.g. `/usr/local/bin`).' \
	  '2. `pip install python/corvus-*.whl` for the client library + `corvus-admin` CLI.' \
	  '3. Run `corvus-admin quickstart` for a single-node setup, or follow `doc/multi-node.md` for a cluster.' \
	  '4. The web UI ships inside the wheel (served by `corvus-web`). To serve the SPA standalone instead, point any static host (nginx, Caddy) at `web/` and proxy `/api` + `/ws` to the `corvus-web` gateway.' \
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


# Bump the project version in every file that carries it. Three
# files hold the source of truth and must stay in lockstep so
# the released Haskell binaries, the Python wheel/sdist, and the
# release-page tarball all advertise the same number:
#
#   * package.yaml            — Haskell source-of-truth (hpack)
#   * corvus.cabal            — generated from package.yaml,
#                               but bumped inline here so the
#                               edit is visible in `git diff`
#                               without a `stack build` round-trip
#   * pyproject.toml          — Python package version
#
# Usage:
#   make set-version VERSION=0.12.0.0
#
# Pick a 4-component PVP-style number (`major.major.minor.patch`)
# so it works as both a Haskell PVP version and a PEP 440 Python
# version. Matches the existing release tag convention
# (`vX.Y.Z.W`).
# Detect whether VERSION was explicitly passed on the command
# line — needed because the `release` target already defaults
# VERSION from package.yaml, so a bare `test -n "$(VERSION)"`
# would always pass and silently no-op `set-version`.
ifeq ($(origin VERSION),command line)
VERSION_CMDLINE := 1
endif

set-version:
	@test -n "$(VERSION_CMDLINE)" || { \
	  echo "Usage: make set-version VERSION=X.Y.Z.W" >&2; \
	  echo "   e.g. make set-version VERSION=0.12.0.0" >&2; \
	  exit 1; \
	}
	@echo "Bumping to $(VERSION)..."
	sed -i 's/^version: .*/version: $(VERSION)/' package.yaml
	sed -i 's/^\(version:[[:space:]]\+\).*/\1$(VERSION)/' corvus.cabal
	sed -i 's/^version = "[^"]*"/version = "$(VERSION)"/' pyproject.toml
	@echo
	@echo "Updated:"
	@grep -H '^version' package.yaml corvus.cabal pyproject.toml
