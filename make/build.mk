.PHONY: build capnp venv install uninstall cleanup

# Build the project. Resolve the destination with the same compiler flags
# as the build, and stop if path lookup fails instead of copying to /bin.
build:
	set -eu; \
	  stack_install_root=$$(stack $(STACK_BUILD_FLAGS) path --local-install-root); \
	  test -n "$$stack_install_root"; \
	  PATH="$$stack_install_root/bin:$$PATH" stack $(STACK_BUILD_FLAGS) --local-bin-path "$$stack_install_root/bin" build --copy-bins

# Regenerate src-generated/Capnp/Gen/*.hs from schema/*.capnp.
# The capnp CLI invokes the `capnpc-haskell` plugin shipped with the
# capnp Haskell library; `stack exec` puts it on PATH after a build.
# Run this after editing any schema/*.capnp file and commit the result.
capnp:
	@command -v capnp >/dev/null 2>&1 || { \
	  echo "error: capnp compiler not on PATH (install dev-libs/capnproto)" >&2; \
	  exit 1; \
	}
	stack $(STACK_BUILD_FLAGS) build capnp
	rm -rf src-generated
	mkdir -p src-generated
	stack $(STACK_BUILD_FLAGS) exec --no-ghc-package-path -- env PATH="$$(stack $(STACK_BUILD_FLAGS) path --compiler-bin):$$(stack $(STACK_BUILD_FLAGS) path --local-install-root)/bin:$$PATH" \
	  capnp compile -ohaskell:src-generated --src-prefix=schema $$(ls schema/*.capnp)

# Note: python/corvus_client/schema is a directory symlink to ../../schema,
# so the Python client (which loads schemas at runtime via pycapnp) and the
# wheel-build's `package-data` glob both see the regenerated `.capnp` files
# with no extra step.

# Shared environment for Python unit tests and integration tests. System site
# packages let the tests use host-installed packages such as PySide6.
venv:
	python3 -m venv --system-site-packages .venv
	.venv/bin/python -m pip install --upgrade pip
	.venv/bin/python -m pip install -q -e '.[harness,desktop,dev]'

# Place Haskell binaries on $PATH, install shell completions, and
# pipx-install the corvus-admin Python CLI. Nothing else: CA
# generation, cert deployment, systemd unit files, and service
# bring-up live in `corvus-admin quickstart`. Run it once after
# `make install` for a turn-key single-node setup.
#
# The CA private key corvus-admin manages lives under
# $XDG_CONFIG_HOME/corvus/admin/ — see python/corvus_admin/.
#
# Web UI: if Node.js + npm are available and the operator has a
# frontend/ tree, we build the SPA so the pipx-installed wheel ships
# the assets. Skipped silently otherwise — operators who only want
# the daemon + CLI don't need Node.
install:
	stack $(STACK_BUILD_FLAGS) install
	@if command -v $(NPM) >/dev/null 2>&1 && [ -f frontend/package.json ]; then \
	  $(MAKE) web-build ; \
	else \
	  echo "skip: web UI build (npm or frontend/ missing) — corvus-web will serve a 404 placeholder"; \
	fi

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

	# corvus-admin completions (mirrors the crv block above; pipx
	# placed corvus-admin under ~/.local/bin so the same target dirs
	# work).
	$(HOME)/.local/bin/corvus-admin completion bash > $(HOME)/.local/share/bash-completion/completions/corvus-admin
	$(HOME)/.local/bin/corvus-admin completion zsh  > $(HOME)/.local/share/zsh/site-functions/_corvus-admin
	$(HOME)/.local/bin/corvus-admin completion fish > $(HOME)/.config/fish/completions/corvus-admin.fish

	@echo ""
	@echo "Haskell binaries + corvus-admin + corvus-web installed."
	@echo "Next step: run 'corvus-admin quickstart' to set up a single-node deployment."
	@echo "Then run 'corvus-web' for the browser UI (defaults to http://127.0.0.1:8080)."

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
	rm -f $(HOME)/.local/bin/corvus-web
	rm -f $(HOME)/.local/share/bash-completion/completions/crv
	rm -f $(HOME)/.local/share/zsh/site-functions/_crv
	rm -f $(HOME)/.config/fish/completions/crv.fish
	rm -f $(HOME)/.local/share/bash-completion/completions/corvus-admin
	rm -f $(HOME)/.local/share/zsh/site-functions/_corvus-admin
	rm -f $(HOME)/.config/fish/completions/corvus-admin.fish


# Cleanup the project build artifacts and test cache
cleanup:
	stack $(STACK_BUILD_FLAGS) clean
