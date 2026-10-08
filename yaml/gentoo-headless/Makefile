DISKS := gentoo-base-cloud gentoo-builder gentoo-base-headless gentoo-base-headless-cloudinit
TEMPLATES := gentoo-cloud gentoo-builder-bake gentoo-headless-bake gentoo-headless gentoo-headless-cloudinit
IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-cloud

include ../image.mk

.PHONY: build-cloud ensure-cloud publish-cloud build-headless ensure-headless publish-headless build-headless-standalone publish-headless-standalone latest-url latest-sha256

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
dependencies: ensure-cloud

publish: publish-headless

build-cloud:
	+$(MAKE) --no-print-directory publish-cloud IMAGE_POLICY=$(IMAGE_POLICY)

ensure-cloud:
	$(call ensure-images,disk:gentoo-base-cloud,publish-cloud)

build-headless: build

ensure-headless: ensure

publish-headless:
	"$(CRV)" build gentoo-headless.yml --var image_if_exists=$(IMAGE_POLICY) --wait

build-headless-standalone: build-cloud
	+$(MAKE) --no-print-directory publish-headless-standalone IMAGE_POLICY=$(IMAGE_POLICY)

publish-headless-standalone:
	"$(CRV)" build gentoo-headless-standalone.yml --var image_if_exists=$(IMAGE_POLICY) --wait

publish-cloud:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then \
	  echo "ERROR: failed to parse image URL from $(LATEST_TXT)" >&2; \
	  exit 1; \
	fi; \
	url="$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"; \
	sha256=$$( $(LATEST_SHA256_SH) ); \
	if [ -z "$$sha256" ]; then \
	  echo "ERROR: failed to parse SHA256 for $$file" >&2; \
	  exit 1; \
	fi; \
	echo "Latest Gentoo cloud-init URL: $$url"; \
	echo "Latest Gentoo cloud-init SHA256: $$sha256"; \
	"$(CRV)" build gentoo-base-cloud.yml --var image_if_exists=$(IMAGE_POLICY) --var gentoo_base_cloud_url="$$url" --var gentoo_base_cloud_sha256="$$sha256" --wait

# Resolve current Gentoo import variables for direct CLI use.
latest-url:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	echo "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"

latest-sha256:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	$(LATEST_SHA256_SH)
