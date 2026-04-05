# Makefile for corvus project

.PHONY: all build install uninstall cleanup unit-tests integration-tests all-tests test test-image lint format

# Add ~/.local/bin to PATH for tools like hlint and fourmolu
export PATH := $(HOME)/.local/bin:$(PATH)

# Number of parallel test jobs (override with: make integration-tests JOBS=8)
JOBS ?= 4

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
unit-tests:
	stack test --test-arguments "--skip Integration"

# Run only integration tests (those with "Integration" in their name)
integration-tests:
	stack test --test-arguments "--match Integration --jobs=$(JOBS)"

# Run all tests
all-tests:
	stack test --test-arguments "--jobs=$(JOBS)"

# Run specific tests (e.g., make test MATCH="test name")
test:
	stack test --test-arguments "--match \"$(MATCH)\""

# Run linter on src, app and test directories
lint:
	hlint src app test

# Build the custom Alpine test image (requires root for qemu-nbd + mount)
test-image:
	doas scripts/build-test-image.sh --force

# Format the code using fourmolu
format:
	fourmolu --mode inplace $(shell find src app test -name '*.hs')
