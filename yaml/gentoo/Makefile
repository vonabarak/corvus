# Select a complete recipe, including its cloud-init variants and templates.
.DEFAULT_GOAL := all
IMAGE ?= glibc-systemd-headless
IMAGES := corvus-test musl-openrc-headless glibc-systemd-builder-cloudinit corvus-test-node glibc-systemd-headless rescue corvus-test-vm musl-openrc-builder glibc-systemd-upstream-cloudinit
ifneq ($(words $(IMAGE)),1)
$(error Select one IMAGE from: $(IMAGES))
endif
ifneq ($(filter $(IMAGES),$(IMAGE)),$(IMAGE))
$(error Unknown IMAGE '$(IMAGE)'. Use: $(IMAGES))
endif


DISKS_glibc-systemd-upstream-cloudinit := gentoo-glibc-systemd-upstream-cloudinit
DISKS_glibc-systemd-builder-cloudinit := gentoo-glibc-systemd-builder-cloudinit
DISKS_glibc-systemd-headless := gentoo-glibc-systemd-headless gentoo-glibc-systemd-headless-cloudinit
DISKS_musl-openrc-builder := gentoo-musl-openrc-builder
DISKS_musl-openrc-headless := gentoo-musl-openrc-headless gentoo-musl-openrc-headless-cloudinit
DISKS_rescue := gentoo-rescue
DISKS_corvus-test := gentoo-corvus-test
DISKS_corvus-test-vm := corvus-test-vm
DISKS_corvus-test-node := corvus-test-node

TEMPLATES_glibc-systemd-builder-cloudinit := gentoo-glibc-systemd-builder-cloudinit-bootstrap gentoo-glibc-systemd-builder-cloudinit-bake
TEMPLATES_glibc-systemd-headless := gentoo-glibc-systemd-builder-cloudinit-bake gentoo-glibc-systemd-headless gentoo-glibc-systemd-headless-cloudinit gentoo-glibc-systemd-headless-uefi gentoo-glibc-systemd-headless-cloudinit-uefi
TEMPLATES_musl-openrc-builder := gentoo-musl-openrc-builder-bake
TEMPLATES_musl-openrc-headless := gentoo-musl-openrc-builder-bake gentoo-musl-openrc-headless gentoo-musl-openrc-headless-uefi gentoo-musl-openrc-headless-cloudinit gentoo-musl-openrc-headless-cloudinit-uefi
TEMPLATES_rescue := gentoo-rescue-bake gentoo-rescue
TEMPLATES_corvus-test-node := corvus-test-node-bake corvus-test-node

PARENT_glibc-systemd-builder-cloudinit := glibc-systemd-upstream-cloudinit
PARENT_glibc-systemd-headless := glibc-systemd-builder-cloudinit
PARENT_musl-openrc-builder := glibc-systemd-builder-cloudinit
PARENT_musl-openrc-headless := musl-openrc-builder
PARENT_rescue := glibc-systemd-builder-cloudinit
PARENT_corvus-test := glibc-systemd-headless
PARENT_corvus-test-vm := musl-openrc-builder
PARENT_corvus-test-node := glibc-systemd-headless

PIPELINE := $(if $(filter glibc-systemd-upstream-cloudinit musl-openrc-builder,$(IMAGE)),,$(IMAGE)/$(IMAGE).yml)
DISKS := $(DISKS_$(IMAGE))
TEMPLATES_corvus-test := corvus-test-bake
TEMPLATES_corvus-test-vm := corvus-test-vm-bake
TEMPLATES := $(TEMPLATES_$(IMAGE))
PARENT := $(PARENT_$(IMAGE))
IMAGE_POLICY ?= update
export GENTOO_CACHE_DIR ?= /data/corvus-cache
USEPKG ?= true
GETBINPKG ?= true
GENTOO_KEYSERVER ?= hkps://keys.gentoo.org
export USEPKG GETBINPKG GENTOO_KEYSERVER
BUILD_ARGS := --var cache_dir="$(GENTOO_CACHE_DIR)" --var USEPKG="$(USEPKG)" --var GETBINPKG="$(GETBINPKG)"
BUILD_ARGS += --var portage_keyserver="$(GENTOO_KEYSERVER)"
BUILD_DEPENDENCIES := build-dependencies

# The key is a local input, not an image dependency; generate it before publication.
ifneq ($(filter corvus-test-node corvus-test-vm,$(IMAGE)),)
LOCAL_PREREQUISITES += test-node-key
.PHONY: test-node-key
test-node-key:
	+$(MAKE) -C ../../integration_tests/keys build
endif

LOCAL_PREREQUISITES += bundle
include ../image.mk

.PHONY: build-dependencies latest-url latest-sha256

# Build refreshes upstream metadata; ensure only fills missing outputs.
# Explicit IMAGE overrides the child's selector inherited through MAKEFLAGS.
ifneq ($(PARENT),)
build-dependencies:
	+$(MAKE) --no-print-directory build IMAGE=$(PARENT) IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) --no-print-directory ensure IMAGE=$(PARENT)
else
build-dependencies:
endif

CURL ?= curl
GENTOO_AUTOBUILDS := https://distfiles.gentoo.org/releases/amd64/autobuilds
GENTOO_BRANCH := current-di-amd64-cloudinit
LATEST_TXT := $(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/latest-di-amd64-cloudinit.txt

# The PGP-wrapped manifest has `<filename> <size>` rows. Resolve the
# filename once so the URL and checksum refer to the same upstream release.
LATEST_FILE_SH = "$(CURL)" -fsS "$(LATEST_TXT)" \
    | awk '/\.qcow2 [0-9]+$$/ {print $$1; exit}'

LATEST_SHA256_SH = "$(CURL)" -fsS "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$${file}.sha256" \
    | awk -v file="$${file}" 'length($$1) == 64 && $$2 == file {print $$1; exit}'

# Recipes with upstream downloads resolve a filename and its matching checksum.
ifeq ($(IMAGE),musl-openrc-builder)
MUSL_BASE := $(GENTOO_AUTOBUILDS)/current-stage3-amd64-musl-openrc
publish: bundle
	@file=$$( "$(CURL)" -fsS "$(MUSL_BASE)/latest-stage3-amd64-musl-openrc.txt" \
	  | awk '/^stage3-amd64-musl-openrc-[0-9]+T[0-9]+Z\.tar\.xz [0-9]+$$/ {print $$1; exit}' ); \
	if [ -z "$$file" ]; then echo "ERROR: invalid musl stage3 manifest" >&2; exit 1; fi; \
	sha256=$$( "$(CURL)" -fsS "$(MUSL_BASE)/$$file.sha256" \
	  | awk -v file="$$file" 'length($$1) == 64 && $$1 !~ /[^0-9a-fA-F]/ && ($$2 == file || $$2 == "*" file) {print $$1; exit}' ); \
	if [ -z "$$sha256" ]; then echo "ERROR: invalid musl stage3 SHA256" >&2; exit 1; fi; \
	"$(CRV)" build musl-openrc-builder/musl-openrc-builder.yml $(BUILD_ARGS) --var image_if_exists=$(IMAGE_POLICY) \
	  --var musl_stage3_url="$(MUSL_BASE)/$$file" --var musl_stage3_sha256="$$sha256" --wait

else ifeq ($(IMAGE),glibc-systemd-upstream-cloudinit)
publish: bundle
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then \
	  echo "ERROR: failed to parse image URL from $(LATEST_TXT)" >&2; \
	  exit 1; \
	fi; \
	url="$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"; \
	tag=$$(printf '%s\n' "$$file" | sed -n 's/^di-amd64-cloudinit-\([0-9][0-9]*T[0-9][0-9]*Z\)\.qcow2$$/\1/p'); \
	if [ -z "$$tag" ]; then \
	  echo "ERROR: failed to parse release tag from $$file" >&2; \
	  exit 1; \
	fi; \
	sha256=$$( $(LATEST_SHA256_SH) ); \
	if [ -z "$$sha256" ]; then \
	  echo "ERROR: failed to parse SHA256 for $$file" >&2; \
	  exit 1; \
	fi; \
	echo "Latest Gentoo cloud-init URL: $$url"; \
	echo "Latest Gentoo cloud-init tag: $$tag"; \
	echo "Latest Gentoo cloud-init SHA256: $$sha256"; \
	"$(CRV)" build glibc-systemd-upstream-cloudinit/glibc-systemd-upstream-cloudinit.yml --var image_if_exists=$(IMAGE_POLICY) --var gentoo_cloud_url="$$url" --var gentoo_cloud_tag="$$tag" --var gentoo_cloud_sha256="$$sha256" --wait

endif

# Resolve current Gentoo import variables for direct CLI use.
latest-url:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	echo "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"

latest-sha256:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	$(LATEST_SHA256_SH)

PYTHON ?= python3
SCRIPTS := $(wildcard $(IMAGE)/build*.py)
.PHONY: bundle plan test export
bundle:
	$(foreach script,$(SCRIPTS),$(PYTHON) bundle.py $(script) $(script:.py=.pyz);)
plan: bundle
	$(foreach script,$(SCRIPTS),IMAGE_BUILD_PLAN=1 $(PYTHON) $(script:.py=.pyz);)
test:
	PYTHONPATH=../../python $(PYTHON) -m pytest ../../python/tests/corvus_image_builder
ifeq ($(IMAGE),rescue)
OUTPUT_DIR ?= build/gentoo-rescue
export:
	$(PYTHON) rescue/export.py "$(OUTPUT_DIR)"
endif
