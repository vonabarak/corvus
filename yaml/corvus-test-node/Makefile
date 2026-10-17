DISKS := corvus-test-node
TEMPLATES := corvus-test-node corvus-test-node-bake
PIPELINE := corvus-test-node.yml

IMAGE_POLICY ?= update
export GENTOO_CACHE_DIR ?= /data/corvus-cache
BUILD_ARGS := --var cache_dir="$(GENTOO_CACHE_DIR)"
BUILD_DEPENDENCIES := build-dependencies

include ../image.mk

.PHONY: build-dependencies

# Refresh upstream inputs for build; ensure only fills missing outputs.
build-dependencies:
	+$(MAKE) -C ../../integration_tests/keys build
	+$(MAKE) -C ../gentoo build-headless IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../../integration_tests/keys build
	+$(MAKE) -C ../gentoo ensure-headless
