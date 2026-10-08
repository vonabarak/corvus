DISKS := corvus-test-node
TEMPLATES := corvus-test-node
PIPELINE := corvus-test-node.yml

IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies

include ../image.mk

.PHONY: build-dependencies

# Refresh upstream inputs for build; ensure only fills missing outputs.
build-dependencies:
	+$(MAKE) -C ../../integration_tests/keys build
	+$(MAKE) -C ../gentoo-headless build-headless IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../../integration_tests/keys build
	+$(MAKE) -C ../gentoo-headless ensure-headless
