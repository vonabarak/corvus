# Select a complete recipe, including its cloud-init variants and templates.
.DEFAULT_GOAL := all
IMAGE ?= gentoo-corvus-test
IMAGES := gentoo-cloud gentoo-builder gentoo-musl-builder gentoo-headless gentoo-musl gentoo-corvus-test corvus-test-node corvus-test-vm
ifneq ($(words $(IMAGE)),1)
$(error Select one IMAGE from: $(IMAGES))
endif
ifneq ($(filter $(IMAGES),$(IMAGE)),$(IMAGE))
$(error Unknown IMAGE '$(IMAGE)'. Use: $(IMAGES))
endif

PIPELINE_gentoo-builder := gentoo-builder.yml
PIPELINE_gentoo-musl := gentoo-musl.yml
PIPELINE_gentoo-headless := gentoo-headless.yml
PIPELINE_gentoo-corvus-test := gentoo-test.yml
PIPELINE_corvus-test-vm := corvus-test-vm.yml
PIPELINE_corvus-test-node := corvus-test-node.yml

DISKS_gentoo-cloud := gentoo-cloud
DISKS_gentoo-builder := gentoo-builder
DISKS_gentoo-headless := gentoo-headless gentoo-headless-cloudinit
DISKS_gentoo-musl-builder := gentoo-musl-builder
DISKS_gentoo-musl := gentoo-musl gentoo-musl-cloudinit
DISKS_gentoo-corvus-test := gentoo-corvus-test
DISKS_corvus-test-vm := corvus-test-vm
DISKS_corvus-test-node := corvus-test-node

TEMPLATES_gentoo-builder := gentoo-builder-bootstrap gentoo-builder-bake
TEMPLATES_gentoo-headless := gentoo-headless gentoo-headless-cloudinit gentoo-headless-uefi gentoo-headless-cloudinit-uefi
TEMPLATES_gentoo-musl-builder := gentoo-musl-builder-bake
TEMPLATES_gentoo-musl := gentoo-musl gentoo-musl-cloudinit gentoo-musl-uefi gentoo-musl-cloudinit-uefi
TEMPLATES_gentoo-corvus-test := gentoo-test-builder
TEMPLATES_corvus-test-node := corvus-test-node

PARENT_gentoo-builder := gentoo-cloud
PARENT_gentoo-headless := gentoo-builder
PARENT_gentoo-musl-builder := gentoo-builder
PARENT_gentoo-musl := gentoo-musl-builder
PARENT_gentoo-corvus-test := gentoo-headless
PARENT_corvus-test-vm := gentoo-musl-builder
PARENT_corvus-test-node := gentoo-headless

PIPELINE := $(PIPELINE_$(IMAGE))
DISKS := $(DISKS_$(IMAGE))
TEMPLATES := $(TEMPLATES_$(IMAGE))
PARENT := $(PARENT_$(IMAGE))
IMAGE_POLICY ?= update
export GENTOO_CACHE_DIR ?= /data/corvus-cache
BUILD_ARGS := --var cache_dir="$(GENTOO_CACHE_DIR)"
BUILD_DEPENDENCIES := build-dependencies

# The key is a local input, not an image dependency; generate it before publication.
ifneq ($(filter corvus-test-node corvus-test-vm,$(IMAGE)),)
LOCAL_PREREQUISITES := test-node-key
.PHONY: test-node-key
test-node-key:
	+$(MAKE) -C ../../integration_tests/keys build
endif

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
ifeq ($(IMAGE),gentoo-musl-builder)
MUSL_BASE := $(GENTOO_AUTOBUILDS)/current-stage3-amd64-musl-openrc
publish:
	@file=$$( "$(CURL)" -fsS "$(MUSL_BASE)/latest-stage3-amd64-musl-openrc.txt" \
	  | awk '/^stage3-amd64-musl-openrc-[0-9]+T[0-9]+Z\.tar\.xz [0-9]+$$/ {print $$1; exit}' ); \
	if [ -z "$$file" ]; then echo "ERROR: invalid musl stage3 manifest" >&2; exit 1; fi; \
	sha256=$$( "$(CURL)" -fsS "$(MUSL_BASE)/$$file.sha256" \
	  | awk -v file="$$file" 'length($$1) == 64 && $$1 !~ /[^0-9a-fA-F]/ && ($$2 == file || $$2 == "*" file) {print $$1; exit}' ); \
	if [ -z "$$sha256" ]; then echo "ERROR: invalid musl stage3 SHA256" >&2; exit 1; fi; \
	"$(CRV)" build gentoo-musl-builder.yml $(BUILD_ARGS) --var image_if_exists=$(IMAGE_POLICY) \
	  --var musl_stage3_url="$(MUSL_BASE)/$$file" --var musl_stage3_sha256="$$sha256" --wait

else ifeq ($(IMAGE),gentoo-cloud)
publish:
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
	"$(CRV)" build gentoo-cloud.yml --var image_if_exists=$(IMAGE_POLICY) --var gentoo_cloud_url="$$url" --var gentoo_cloud_tag="$$tag" --var gentoo_cloud_sha256="$$sha256" --wait

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
