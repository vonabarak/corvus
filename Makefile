# Makefile for corvus project

.PHONY: all build install cleanup unit-tests integration-tests all-tests lint format

# Add ~/.local/bin to PATH for tools like hlint and ormolu
export PATH := $(HOME)/.local/bin:$(PATH)

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

# Cleanup the project build artifacts
cleanup:
	stack clean

# Run only unit tests (excluding those with "Integration" in their name)
unit-tests:
	stack test --test-arguments "--skip Integration"

# Run only integration tests (those with "Integration" in their name)
integration-tests:
	stack test --test-arguments "--match Integration"

# Run all tests
all-tests:
	stack test

# Run linter on src, app and test directories
lint:
	hlint src app test

# Format the code using ormolu
format:
	ormolu --mode inplace $(shell find src app test -name '*.hs')
