IMAGE_POLICY ?= update
HEADLESS ?= 0

ifeq ($(HEADLESS),1)
DISKS := freebsd-headless
TEMPLATES := freebsd-headless-bake freebsd-headless
PIPELINE := freebsd-headless.yml
BUILD_DEPENDENCIES := build-source
else
DISKS := freebsd:15
TEMPLATES := freebsd15
endif

include ../image.mk

CURL ?= curl
FREEBSD_BASE := https://download.freebsd.org/releases/VM-IMAGES/15.1-RELEASE/amd64/Latest
FREEBSD_FILE := FreeBSD-15.1-RELEASE-amd64-BASIC-CLOUDINIT-ufs.qcow2.xz

.PHONY: build-source ensure-source
build-source:
	+$(MAKE) --no-print-directory build HEADLESS=0 BUILD_DEPENDENCIES=dependencies IMAGE_POLICY=$(IMAGE_POLICY)

ensure-source:
	+$(MAKE) --no-print-directory ensure HEADLESS=0

ifeq ($(HEADLESS),1)
dependencies: ensure-source
else
publish:
	@url="$(FREEBSD_BASE)/$(FREEBSD_FILE)"; \
	digest=$$("$(CURL)" -fsS "$(FREEBSD_BASE)/CHECKSUM.SHA512" | awk -v f="$(FREEBSD_FILE)" '$$2 == "(" f ")" {print $$4; exit}'); \
	if [[ ! "$$digest" =~ ^[[:xdigit:]]{128}$$ ]]; then \
	  echo "ERROR: failed to resolve valid FreeBSD SHA512" >&2; exit 1; \
	fi; \
	"$(CRV)" build freebsd.yml --var image_if_exists=$(IMAGE_POLICY) \
	  --var freebsd_url="$$url" --var freebsd_sha512="$$digest" --wait
endif
