.PHONY: all build clean update-image

# The Gentoo cloud-init autobuild URL changes weekly. Run
# `make update-image` to rewrite gentoo-test.yml in place with
# the current dated filename pulled from upstream; `make build`
# does it automatically before invoking `crv build`.
GENTOO_AUTOBUILDS := https://distfiles.gentoo.org/releases/amd64/autobuilds
LATEST_TXT := $(GENTOO_AUTOBUILDS)/current-di-amd64-cloudinit/latest-di-amd64-cloudinit.txt

all: build

build: update-image
	crv build gentoo-headless.yml --wait
	crv build gentoo-test.yml --wait

clean:
	crv vm delete gentoo-corvus-test || true
	crv template delete gentoo-headless || true
	crv disk delete gentoo-base-headless || true
	crv template delete gentoo-cloud || true

# Pull the current cloud-init image filename from the upstream
# `latest-di-amd64-cloudinit.txt` and rewrite the dated qcow2
# name in gentoo-headless.yml. The file is PGP-signed text with a
# few `#`-prefixed header lines; the line that ends in
# `.qcow2 <size>` is the one we want. We also drop the `md5:`
# field because Gentoo's DIGESTS sidecar only publishes
# BLAKE2B / SHA512 hashes — there's no upstream MD5 to keep in
# sync, so leaving a stale hash would make `crv build` reject
# the freshly downloaded image. Re-add `md5:` manually after
# the first successful build if you want the verification.
update-image:
	@latest=$$(curl -fsS $(LATEST_TXT) \
	  | awk '/\.qcow2 [0-9]+$$/ {print $$1; exit}'); \
	if [ -z "$$latest" ]; then \
	  echo "ERROR: failed to parse image name from $(LATEST_TXT)"; \
	  exit 1; \
	fi; \
	echo "Latest Gentoo cloud-init image: $$latest"; \
	sed -i -E "s|di-amd64-cloudinit-[0-9TZ]+\.qcow2|$$latest|g" gentoo-headless.yml; \
	sed -i -E '/^[[:space:]]*md5:[[:space:]]*[0-9a-f]{32}[[:space:]]*$$/d' gentoo-headless.yml; \
	echo "Updated gentoo-headless.yml"
