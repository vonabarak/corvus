DISKS := gentoo-base-cloud gentoo-builder gentoo-base-headless gentoo-base-headless-cloudinit gentoo-corvus-test
TEMPLATES := gentoo-cloud gentoo-builder-bake gentoo-headless-bake gentoo-headless gentoo-headless-cloudinit
HEADLESS_OUTPUTS := disk:gentoo-builder disk:gentoo-base-headless disk:gentoo-base-headless-cloudinit $(addprefix template:,$(TEMPLATES))

include ../image.mk

.PHONY: build-cloud ensure-cloud publish-cloud build-headless ensure-headless publish-headless build-test publish-test latest-url latest-sha256

GENTOO_AUTOBUILDS := https://distfiles.gentoo.org/releases/amd64/autobuilds
GENTOO_BRANCH    := current-di-amd64-cloudinit
LATEST_TXT       := $(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/latest-di-amd64-cloudinit.txt

# Compute the current filename inline. The file is a PGP-wrapped text
# manifest with `<filename> <size>` lines and a few `#` headers; we
# pick the first `*.qcow2` row.
LATEST_FILE_SH = curl -fsS $(LATEST_TXT) \
    | awk '/\.qcow2 [0-9]+$$/ {print $$1; exit}'

LATEST_SHA256_SH = curl -fsS "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$${file}.sha256" \
    | awk -v file="$${file}" 'length($$1) == 64 && $$2 == file {print $$1; exit}'


dependencies: ensure-cloud

publish:
	+$(MAKE) --no-print-directory publish-headless
	+$(MAKE) --no-print-directory publish-test

build-cloud:
	+$(MAKE) --no-print-directory publish-cloud IMAGE_POLICY=overwrite

ensure-cloud:
	$(call ensure-images,disk:gentoo-base-cloud,publish-cloud)

build-headless: ensure-cloud
	+$(MAKE) --no-print-directory publish-headless IMAGE_POLICY=overwrite

ensure-headless: ensure-cloud
	$(call ensure-images,$(HEADLESS_OUTPUTS),publish-headless)

publish-headless:
	"$(CRV)" build gentoo-headless.yml --var image_if_exists=$(IMAGE_POLICY) --wait

build-test: ensure-headless
	+$(MAKE) --no-print-directory publish-test IMAGE_POLICY=overwrite

publish-test:
	"$(CRV)" build gentoo-test.yml --var image_if_exists=$(IMAGE_POLICY) --wait

publish-cloud:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then \
	  echo "ERROR: failed to parse image URL from $(LATEST_TXT)"; \
	  exit 1; \
	fi; \
	url="$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"; \
	sha256=$$( $(LATEST_SHA256_SH) ); \
	if [ -z "$$sha256" ]; then \
	  echo "ERROR: failed to parse SHA256 for $$file"; \
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
