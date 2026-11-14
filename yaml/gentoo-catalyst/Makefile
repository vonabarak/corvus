.DEFAULT_GOAL := all
IMAGE ?= gentoo-glibc-systemd-headless-catalyst
IMAGES := gentoo-glibc-systemd-headless-catalyst gentoo-glibc-systemd-catalyst-builder
ifneq ($(words $(IMAGE)),1)
$(error Select one IMAGE from: $(IMAGES))
endif
ifeq ($(filter $(IMAGES),$(IMAGE)),)
$(error Unknown IMAGE '$(IMAGE)'. Use: $(IMAGES))
endif

IMAGE_POLICY ?= update
export GENTOO_CACHE_DIR ?= /data/corvus-cache
export GENTOO_KEYSERVER ?= hkps://keys.gentoo.org
export STAGE3_URL STAGE3_SHA256 REPO_COMMIT
BUILD_DEPENDENCIES := build-dependencies
DISKS := $(IMAGE)
ifeq ($(IMAGE),gentoo-glibc-systemd-catalyst-builder)
TEMPLATES := $(IMAGE)-bootstrap $(IMAGE)-bake
else
TEMPLATES := $(IMAGE) $(IMAGE)-uefi
endif

include ../image.mk

.PHONY: build-dependencies test
ifeq ($(IMAGE),gentoo-glibc-systemd-catalyst-builder)
build-dependencies:
	+$(MAKE) -C ../gentoo build IMAGE=gentoo-glibc-systemd-upstream-cloudinit IMAGE_POLICY=$(IMAGE_POLICY)
dependencies:
	+$(MAKE) -C ../gentoo ensure IMAGE=gentoo-glibc-systemd-upstream-cloudinit
else
build-dependencies:
	+$(MAKE) build IMAGE=gentoo-glibc-systemd-catalyst-builder IMAGE_POLICY=$(IMAGE_POLICY)
dependencies:
	+$(MAKE) ensure IMAGE=gentoo-glibc-systemd-catalyst-builder
endif

publish:
	CRV="$(CRV)" bash scripts/publish.sh $(IMAGE) $(IMAGE_POLICY)

test:
	bash tests/build.sh
	find scripts catalyst -name '*.sh' -exec bash -n {} \;
