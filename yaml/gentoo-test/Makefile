DISKS := gentoo-corvus-test
PIPELINE := gentoo-test.yml
IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies

include ../image.mk

.PHONY: build-dependencies build-test publish-test

# A build evaluates the upstream chain; ensure only fills missing outputs.
build-dependencies:
	+$(MAKE) -C ../gentoo-headless build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../gentoo-headless ensure-headless

build-test: build

publish-test: publish
