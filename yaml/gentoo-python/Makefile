.DEFAULT_GOAL := plan
PYTHON ?= python3
IMAGE_POLICY ?= update
CACHE_DIR ?= /data/corvus-cache
USEPKG ?= true
GETBINPKG ?= true
.PHONY: bundle plan build ensure-dependencies
bundle:
	$(PYTHON) ../../image_builder/bundle.py image-builder.pyz
ensure-dependencies:
	$(MAKE) -C ../gentoo IMAGE=gentoo-glibc-systemd-builder-cloudinit IMAGE_POLICY=$(IMAGE_POLICY) GENTOO_CACHE_DIR=$(CACHE_DIR) ensure
plan: bundle
	$(PYTHON) image-builder.pyz plan corvus_image_builder.gentoo:headless --root /mnt/sysroot --disk /dev/vdb --kernel-config ../gentoo/scripts/stages/kernel/gentoo-kernel
build: bundle ensure-dependencies
	crv build headless.yml --var cache_dir=$(CACHE_DIR) --var image_if_exists=$(IMAGE_POLICY) --var USEPKG=$(USEPKG) --var GETBINPKG=$(GETBINPKG) --wait
