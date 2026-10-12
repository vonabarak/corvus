# Makefile for corvus project

.DEFAULT_GOAL := all
.PHONY: all

# Add ~/.local/bin to PATH for locally installed executables
export PATH := $(HOME)/.local/bin:$(PATH)

# Extra flags appended to every stack invocation. Empty for the
# dev tree (ghcup + stack do the right thing); CI overrides
# with `STACK_BUILD_FLAGS='--system-ghc --no-install-ghc'` so
# stack reuses the runner-provisioned GHC instead of trying to
# install its own. Shared by all workflows that invoke `stack`.
STACK_BUILD_FLAGS ?=

# Default target: build
all: build

# Workflow fragments share this make invocation and repository-root paths.
include make/build.mk
include make/tests.mk
include make/quality.mk
include make/ui.mk
include make/vms.mk
include make/release.mk
