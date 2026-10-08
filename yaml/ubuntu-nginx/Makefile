DISKS := ubuntu26-nginx
TEMPLATES :=
PIPELINE := ubuntu-nginx.yml

IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies

include ../image.mk

.PHONY: build-dependencies

# Refresh upstream inputs for build; ensure only fills missing outputs.
build-dependencies:
	+$(MAKE) -C ../multi-os build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../multi-os ensure
