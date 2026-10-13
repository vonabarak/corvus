IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies
DISKS := windows-11-desktop
TEMPLATES := windows-11-desktop
PIPELINE := windows-11-desktop.yml

include ../image.mk

.PHONY: build-dependencies
build-dependencies:
	+$(MAKE) -C ../windows-11 build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../windows-11 ensure
