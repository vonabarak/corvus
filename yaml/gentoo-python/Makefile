.DEFAULT_GOAL := plan
PYTHON ?= python3
IMAGE ?= glibc-systemd-headless
IMAGES := rescue corvus-test glibc-systemd-headless musl-openrc-headless corvus-test-node corvus-test-vm
ifneq ($(words $(IMAGE)),1)
$(error Select one IMAGE from: $(IMAGES))
endif
ifneq ($(filter $(IMAGES),$(IMAGE)),$(IMAGE))
$(error Unknown IMAGE '$(IMAGE)'. Use: $(IMAGES))
endif
BUILDER_rescue := gentoo-glibc-systemd-builder-cloudinit
BUILDER_glibc-systemd-headless := gentoo-glibc-systemd-builder-cloudinit
BUILDER_musl-openrc-headless := gentoo-musl-openrc-builder
BUILDER_corvus-test-vm := gentoo-musl-openrc-builder
IMAGE_POLICY ?= update
CACHE_DIR ?= /data/corvus-cache
USEPKG ?= true
GETBINPKG ?= true
GENTOO_KEYSERVER ?= hkps://keys.gentoo.org
.PHONY: bundle plan build ensure-dependencies ensure-harness-key
bundle:
	$(PYTHON) bundle.py $(IMAGE).py $(IMAGE).pyz
ensure-dependencies:
ifneq ($(filter corvus-test corvus-test-node,$(IMAGE)),)
	$(MAKE) build IMAGE=glibc-systemd-headless IMAGE_POLICY=$(IMAGE_POLICY) CACHE_DIR=$(CACHE_DIR) USEPKG=$(USEPKG) GETBINPKG=$(GETBINPKG) GENTOO_KEYSERVER="$(GENTOO_KEYSERVER)"
else
	$(MAKE) -C ../gentoo IMAGE=$(BUILDER_$(IMAGE)) IMAGE_POLICY=$(IMAGE_POLICY) GENTOO_CACHE_DIR=$(CACHE_DIR) GENTOO_KEYSERVER="$(GENTOO_KEYSERVER)" ensure
endif
ensure-harness-key:
ifneq ($(filter corvus-test-node corvus-test-vm,$(IMAGE)),)
	$(MAKE) -C ../../integration_tests/keys build
endif
plan: bundle
	IMAGE_BUILD_PLAN=1 $(PYTHON) $(IMAGE).pyz
build: bundle ensure-harness-key ensure-dependencies
	crv build $(IMAGE).yml --var cache_dir=$(CACHE_DIR) --var image_if_exists=$(IMAGE_POLICY) --var USEPKG=$(USEPKG) --var GETBINPKG=$(GETBINPKG) --var portage_keyserver="$(GENTOO_KEYSERVER)" --wait
