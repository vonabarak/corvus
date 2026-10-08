DISKS := debian-12-generic-base ubuntu-26.04-server-base almalinux-10-base freebsd-14-base alpine-3.21-base
TEMPLATES := debian12 ubuntu26 almalinux10 freebsd14

IMAGE_POLICY ?= update

include ../image.mk

CURL ?= curl

DEBIAN_BASE := https://cloud.debian.org/images/cloud/bookworm/latest
DEBIAN_FILE := debian-12-generic-amd64.qcow2

UBUNTU_BASE := https://cloud-images.ubuntu.com/releases/resolute/release
UBUNTU_FILE := ubuntu-26.04-server-cloudimg-amd64.img

ALMALINUX_BASE := https://repo.almalinux.org/almalinux/10/cloud/x86_64_v2/images
ALMALINUX_FILE := AlmaLinux-10-GenericCloud-latest.x86_64_v2.qcow2

FREEBSD_BASE := https://download.freebsd.org/releases/VM-IMAGES/14.4-RELEASE/amd64/Latest
FREEBSD_FILE := FreeBSD-14.4-RELEASE-amd64-BASIC-CLOUDINIT-ufs.qcow2.xz

ALPINE_BASE := https://dl-cdn.alpinelinux.org/alpine/v3.21/releases/cloud
ALPINE_FILE_SH = "$(CURL)" -fsS $(ALPINE_BASE)/ \
	| sed -n 's/.*href="\(nocloud_alpine-3\.21\.[0-9][^"]*-x86_64-bios-cloudinit-r0\.qcow2\)".*/\1/p' \
	| sort -V \
	| tail -n 1

publish:
	@debian_url="$(DEBIAN_BASE)/$(DEBIAN_FILE)"; \
	debian_sha512=$$("$(CURL)" -fsS "$(DEBIAN_BASE)/SHA512SUMS" | awk -v f="$(DEBIAN_FILE)" '$$2 == f {print $$1; exit}'); \
	ubuntu_url="$(UBUNTU_BASE)/$(UBUNTU_FILE)"; \
	ubuntu_sha256=$$("$(CURL)" -fsS "$(UBUNTU_BASE)/SHA256SUMS" | awk -v f="$(UBUNTU_FILE)" '{gsub(/^\*/, "", $$2); if ($$2 == f) {print $$1; exit}}'); \
	almalinux_url="$(ALMALINUX_BASE)/$(ALMALINUX_FILE)"; \
	almalinux_sha256=$$("$(CURL)" -fsS "$(ALMALINUX_BASE)/CHECKSUM" | awk -v f="$(ALMALINUX_FILE)" '$$2 == f {print $$1; exit}'); \
	freebsd_url="$(FREEBSD_BASE)/$(FREEBSD_FILE)"; \
	freebsd_sha512=$$("$(CURL)" -fsS "$(FREEBSD_BASE)/CHECKSUM.SHA512" | awk -v f="$(FREEBSD_FILE)" '$$2 == "(" f ")" {print $$4; exit}'); \
	alpine_file=$$( $(ALPINE_FILE_SH) ); \
	if [[ ! "$$alpine_file" =~ ^nocloud_alpine-3\.21\.[0-9]+.*-x86_64-bios-cloudinit-r0\.qcow2$$ ]]; then \
	  echo "ERROR: failed to resolve Alpine filename" >&2; exit 1; \
	fi; \
	alpine_url="$(ALPINE_BASE)/$$alpine_file"; \
	alpine_sha512=$$("$(CURL)" -fsS "$$alpine_url.sha512" | awk -v f="$$alpine_file" '($$2 == f || $$2 == "*" f) {print $$1; exit}'); \
	for pair in \
	  "debian_sha512:128:$$debian_sha512" \
	  "ubuntu_sha256:64:$$ubuntu_sha256" \
	  "almalinux_sha256:64:$$almalinux_sha256" \
	  "freebsd_sha512:128:$$freebsd_sha512" \
	  "alpine_sha512:128:$$alpine_sha512"; do \
	  name=$${pair%%:*}; rest=$${pair#*:}; length=$${rest%%:*}; digest=$${rest#*:}; \
	  if [[ ! "$$digest" =~ ^[[:xdigit:]]+$$ ]] || [ "$${#digest}" -ne "$$length" ]; then \
	    echo "ERROR: failed to resolve valid $$name" >&2; exit 1; \
	  fi; \
	done; \
	echo "Debian:    $$debian_url"; \
	echo "Ubuntu:    $$ubuntu_url"; \
	echo "AlmaLinux: $$almalinux_url"; \
	echo "FreeBSD:   $$freebsd_url"; \
	echo "Alpine:    $$alpine_url"; \
	"$(CRV)" build multi-os.yml --var image_if_exists=$(IMAGE_POLICY) \
	  --var debian_url="$$debian_url" \
	  --var debian_sha512="$$debian_sha512" \
	  --var ubuntu_url="$$ubuntu_url" \
	  --var ubuntu_sha256="$$ubuntu_sha256" \
	  --var almalinux_url="$$almalinux_url" \
	  --var almalinux_sha256="$$almalinux_sha256" \
	  --var freebsd_url="$$freebsd_url" \
	  --var freebsd_sha512="$$freebsd_sha512" \
	  --var alpine_url="$$alpine_url" \
	  --var alpine_sha512="$$alpine_sha512" \
	  --wait
