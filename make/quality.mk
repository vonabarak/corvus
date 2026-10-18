.PHONY: format check check-haskell haskell-format-tools typecheck-core desktop-typecheck code-metrics install-git-hooks yaml-schema-check

# Lint tools must run through the shared venv's Python, alongside project and
# test typing dependencies. With --system-site-packages, pip may reuse a host
# tool package without creating its executable inside .venv/bin.
# Run `make venv` first; explicit overrides remain possible.
MYPY ?= .venv/bin/python -m mypy
RUFF ?= .venv/bin/python -m ruff

# Authored sources in maintained packages; generated bindings remain excluded.
HASKELL_SOURCE_DIRS := src app test tools vendor/haskell-capnp/capnp/lib vendor/haskell-capnp/capnp/cmd vendor/haskell-capnp/capnp-tests/tests vendor/lifetimes/src vendor/lifetimes/tests
HASKELL_SOURCES = $(shell find $(HASKELL_SOURCE_DIRS) \( -name '*.hs' -o -name '*.hs-boot' \) -not -path '*/.stack-work/*' -not -path '*/.stack-work-quality/*') vendor/haskell-capnp/capnp-examples/lib/Examples/Rpc/CalculatorClient.hs vendor/haskell-capnp/capnp-examples/lib/Examples/Rpc/CalculatorServer.hs

# Build the pinned Haskell formatter and linter in the tools project.
haskell-format-tools:
	stack $(STACK_BUILD_FLAGS) --stack-yaml tools/stack.yaml build hlint fourmolu

# Format Python (ruff) + Haskell (fourmolu) sources in place. When
# `frontend/node_modules/` is present (operator has run
# `make web-build` or `npm install`), also run the frontend's
# prettier formatter — kept gated so the dev workflow for someone
# touching only the Haskell/Python side doesn't require Node.
format: haskell-format-tools
	$(RUFF) format python integration_tests
	$(RUFF) check --fix python integration_tests
	stack $(STACK_BUILD_FLAGS) --stack-yaml tools/stack.yaml exec -- fourmolu --mode inplace $(HASKELL_SOURCES)
	@if [ -d frontend/node_modules ]; then \
	  $(MAKE) web-format ; \
	fi


# Verification without edits to tracked files: formatting, static analysis,
# a strict Haskell build, Weeder, unit tests, and expression coverage. Run
# `make format` first to fix any formatting violations this flags.
# Frontend lint piggybacks on `frontend/node_modules/` being present;
# CI runs `make web-build` (which `npm ci`s) before `make check`.
check: haskell-format-tools
	stack $(STACK_BUILD_FLAGS) --stack-yaml tools/stack.yaml exec -- hlint --hint=.hlint.yaml $(HASKELL_SOURCES)
	stack $(STACK_BUILD_FLAGS) --stack-yaml tools/stack.yaml exec -- fourmolu --mode check $(HASKELL_SOURCES)
	$(RUFF) check python integration_tests
	$(RUFF) format --check python integration_tests
	$(MAKE) typecheck-core
	$(MAKE) yaml-schema-check
	@if [ -d frontend/node_modules ]; then \
	  $(MAKE) web-lint ; \
	fi
	$(MAKE) check-haskell

check-haskell:
	MATCH="$(MATCH)" script -qec 'bash scripts/check-haskell.sh $(STACK_BUILD_FLAGS)' /dev/null

# Check editor schemas and authored recipes without starting a daemon.
yaml-schema-check: venv
	.venv/bin/python -m pytest python/tests/test_build_yaml_schema.py -v

# The primary check environment deliberately omits the large optional Qt
# runtime.  It still type-checks every non-desktop Python module and the
# existing integration harness.  Desktop code has its own reproducible gate.
typecheck-core:
	$(MYPY) python integration_tests

# Requires `pip install -e '.[harness,desktop]'` so mypy can load PySide6's
# shipped annotations.  CI runs this in a dedicated job.
desktop-typecheck:
	$(MYPY) --config-file mypy-desktop.ini python/corvus_desktop python/tests/corvus_desktop

# Report and enforce size limits for authored Haskell source.
code-metrics:
	stack $(STACK_BUILD_FLAGS) --stack-yaml tools/stack.yaml run corvus-code-metrics

# Enable the repository-managed pre-commit hook for this checkout.
install-git-hooks:
	git config core.hooksPath .githooks
