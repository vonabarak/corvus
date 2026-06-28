.PHONY: all build clean build-headless build-test latest-url latest-sha256

# The Gentoo cloud-init autobuild URL changes every few days. We
# discover the current filename/checksum at build time and pass them
# into `crv build` via YAML variables — no in-place edits to the YAML,
# nothing to commit, nothing to drift.
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

all: build

build:
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
	crv build gentoo-headless.yml --var gentoo_base_cloud_url="$$url" --var gentoo_base_cloud_sha256="$$sha256" --wait; \
	crv build gentoo-test.yml --wait

build-headless:
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
	crv build gentoo-headless.yml --var gentoo_base_cloud_url="$$url" --var gentoo_base_cloud_sha256="$$sha256" --wait

build-test: build-headless
	crv build gentoo-test.yml --wait

# Print the currently-published Gentoo cloud-init URL. Handy when
# you want to invoke `crv build` by hand without the wrapper:
#
#     crv build gentoo-headless.yml \
#         --var gentoo_base_cloud_url="$(make -s latest-url)" \
#         --var gentoo_base_cloud_sha256="$(make -s latest-sha256)"
latest-url:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	echo "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/$$file"

latest-sha256:
	@file=$$( $(LATEST_FILE_SH) ); \
	if [ -z "$$file" ]; then exit 1; fi; \
	$(LATEST_SHA256_SH)

clean:
	crv vm delete gentoo-corvus-test || true
	crv template delete gentoo-headless-cloudinit || true
	crv template delete gentoo-headless || true
	crv template delete gentoo-headless-bake || true
	crv template delete gentoo-builder-bake || true
	crv template delete gentoo-cloud || true
	crv disk delete gentoo-base-headless-cloudinit || true
	crv disk delete gentoo-corvus-test || true
	crv disk delete gentoo-base-headless || true
	crv disk delete gentoo-builder || true
	crv disk delete gentoo-base-cloud || true
