DISKS := almalinux-10-dvd-iso
IMAGE_POLICY ?= update

include ../image.mk

CURL ?= curl
ALMALINUX_BASE := https://repo.almalinux.org/almalinux/10/isos/x86_64_v2
ALMALINUX_FILE := AlmaLinux-10-latest-x86_64_v2-dvd.iso

publish:
	@digest=$$("$(CURL)" -fsS "$(ALMALINUX_BASE)/CHECKSUM" | awk -v f="$(ALMALINUX_FILE)" '$$1 == "SHA256" && $$2 == "(" f ")" && $$3 == "=" {print $$4}'); \
	if [[ ! "$$digest" =~ ^[[:xdigit:]]{64}$$ ]]; then \
	  echo "ERROR: failed to resolve valid AlmaLinux DVD SHA256" >&2; exit 1; \
	fi; \
	"$(CRV)" build almalinux-10-media.yml --var image_if_exists=$(IMAGE_POLICY) \
	  --var almalinux_url="$(ALMALINUX_BASE)/$(ALMALINUX_FILE)" \
	  --var almalinux_sha256="$$digest" --wait
