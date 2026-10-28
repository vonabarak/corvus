DISKS := gentoo-cloud gentoo-builder gentoo-headless gentoo-headless-cloudinit
TEMPLATES := gentoo-cloud gentoo-builder-bake gentoo-headless-bake gentoo-headless gentoo-headless-cloudinit gentoo-headless-uefi gentoo-headless-cloudinit-uefi
IMAGE_POLICY ?= update
export GENTOO_CACHE_DIR ?= /data/corvus-cache
BUILD_ARGS := --var cache_dir="$(GENTOO_CACHE_DIR)"
# The headless selector also serves node-image dependencies.
ifeq ($(HEADLESS_ONLY),1)
BUILD_DEPENDENCIES := build-cloud
else
DISKS += gentoo-corvus-test
BUILD_DEPENDENCIES := build-headless
endif

include ../image.mk

.PHONY: build-cloud ensure-cloud publish-cloud build-headless ensure-headless publish-headless build-test publish-test latest-url latest-sha256

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

# Ensure uses presence checks; build checks the current upstream checksum.
ifeq ($(HEADLESS_ONLY),1)
dependencies: ensure-cloud
publish: publish-headless
else
dependencies: ensure-headless
publish: publish-test
endif

build-cloud:
	+$(MAKE) --no-print-directory publish-cloud IMAGE_POLICY=$(IMAGE_POLICY)

ensure-cloud:
	$(call ensure-images,disk:gentoo-cloud,publish-cloud)

build-headless:
	+$(MAKE) --no-print-directory build HEADLESS_ONLY=1 IMAGE_POLICY=$(IMAGE_POLICY)

ensure-headless:
	+$(MAKE) --no-print-directory ensure HEADLESS_ONLY=1

build-test: build

publish-test:
	"$(CRV)" build gentoo-test.yml $(BUILD_ARGS) --var image_if_exists=$(IMAGE_POLICY) --wait

publish-headless:
	"$(CRV)" build gentoo-headless.yml $(BUILD_ARGS) --var image_if_exists=$(IMAGE_POLICY) --wait

publish-cloud:
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

# Resolve current Gentoo import variables for direct CLI use.
latest-url:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	echo "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"

latest-sha256:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	$(LATEST_SHA256_SH)
