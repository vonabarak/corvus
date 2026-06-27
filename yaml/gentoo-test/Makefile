.PHONY: all build clean build-headless build-test latest-url

# The Gentoo cloud-init autobuild URL changes every few days. We
# discover the current filename at build time and pass the full URL
# into `crv build` via the YAML's `gentoo_base_cloud_url` variable —
# no in-place edits to the YAML, nothing to commit, nothing to drift.
GENTOO_AUTOBUILDS := https://distfiles.gentoo.org/releases/amd64/autobuilds
GENTOO_BRANCH    := current-di-amd64-cloudinit
LATEST_TXT       := $(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/latest-di-amd64-cloudinit.txt

# Compute the absolute URL inline. The file is a PGP-wrapped text
# manifest with `<filename> <size>` lines and a few `#` headers; we
# pick the first `*.qcow2` row.
LATEST_URL_SH = curl -fsS $(LATEST_TXT) \
    | awk '/\.qcow2 [0-9]+$$/ {print "$(GENTOO_AUTOBUILDS)/$(GENTOO_BRANCH)/" $$1; exit}'

all: build

build:
	@url=$$( $(LATEST_URL_SH) ); \
	if [ -z "$$url" ]; then \
	  echo "ERROR: failed to parse image URL from $(LATEST_TXT)"; \
	  exit 1; \
	fi; \
	echo "Latest Gentoo cloud-init URL: $$url"; \
	crv build gentoo-headless.yml --var gentoo_base_cloud_url="$$url" --wait; \
	crv build gentoo-test.yml --wait

build-headless:
	@url=$$( $(LATEST_URL_SH) ); \
	if [ -z "$$url" ]; then \
	  echo "ERROR: failed to parse image URL from $(LATEST_TXT)"; \
	  exit 1; \
	fi; \
	echo "Latest Gentoo cloud-init URL: $$url"; \
	crv build gentoo-headless.yml --var gentoo_base_cloud_url="$$url" --wait

build-test: build-headless
	crv build gentoo-test.yml --wait

# Print the currently-published Gentoo cloud-init URL. Handy when
# you want to invoke `crv build` by hand without the wrapper:
#
#     crv build gentoo-headless.yml \
#         --var gentoo_base_cloud_url="$(make -s latest-url)"
latest-url:
	@$(LATEST_URL_SH)

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
