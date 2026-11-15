.DEFAULT_GOAL := plan
PYTHON ?= python3
IMAGE_POLICY ?= update
CACHE_DIR ?= /data/corvus-cache
USEPKG ?= true
GETBINPKG ?= true
.PHONY: bundle plan build ensure-dependencies
bundle:
	$(PYTHON) bundle.py glibc-systemd-headless.py glibc-systemd-headless.pyz
ensure-dependencies:
	$(MAKE) -C ../gentoo IMAGE=gentoo-glibc-systemd-builder-cloudinit IMAGE_POLICY=$(IMAGE_POLICY) GENTOO_CACHE_DIR=$(CACHE_DIR) ensure
plan: bundle
	IMAGE_BUILD_PLAN=1 $(PYTHON) glibc-systemd-headless.pyz
build: bundle ensure-dependencies
	crv build headless.yml --var cache_dir=$(CACHE_DIR) --var image_if_exists=$(IMAGE_POLICY) --var USEPKG=$(USEPKG) --var GETBINPKG=$(GETBINPKG) --wait
