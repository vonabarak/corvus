DISKS := corvus-test-installer-iso
DOWNLOAD_CACHE := cache
IMAGE_POLICY ?= update
CORVUS_BASE_IMAGES_DIR ?= $(HOME)/VMs/BaseImages
ISO := build/corvus-test-installer-iso.raw

ALPINE_MIRROR ?= https://dl-cdn.alpinelinux.org/alpine
ALPINE_VERSION ?= v3.21
ALPINE_ARCH ?= x86_64
ALPINE_KERNEL_PKG ?= linux-virt-6.12.110-r0.apk
ALPINE_BUSYBOX_PKG ?= busybox-static-1.37.0-r14.apk
ALPINE_SYSLINUX_PKG ?= syslinux-6.04_pre1-r16.apk
export ALPINE_MIRROR ALPINE_VERSION ALPINE_ARCH
export ALPINE_KERNEL_PKG ALPINE_BUSYBOX_PKG ALPINE_SYSLINUX_PKG
PACKAGES := $(addprefix cache/,$(ALPINE_KERNEL_PKG) $(ALPINE_BUSYBOX_PKG) $(ALPINE_SYSLINUX_PKG))

include ../image.mk

.PHONY: force-config iso
iso: $(ISO)

# Keep the ISO unchanged while its inputs stay unchanged. Package selection
# is recorded too, so switching back to an older cached package rebuilds it.
build/package-selection: force-config
	@mkdir -p build; \
	printf '%s\n' '$(ALPINE_MIRROR)' '$(ALPINE_VERSION)' '$(ALPINE_ARCH)' \
	  '$(ALPINE_KERNEL_PKG)' '$(ALPINE_BUSYBOX_PKG)' '$(ALPINE_SYSLINUX_PKG)' > $@.tmp; \
	if ! cmp -s $@.tmp $@; then mv $@.tmp $@; else rm $@.tmp; fi

$(PACKAGES):
	mkdir -p cache
	curl -fsSL -o $@.tmp '$(ALPINE_MIRROR)/$(ALPINE_VERSION)/main/$(ALPINE_ARCH)/$(@F)'
	mv $@.tmp $@

$(ISO): Makefile build-synthetic-installer.sh init.sh isolinux.cfg build/package-selection $(PACKAGES)
	./build-synthetic-installer.sh

publish: $(ISO)
	"$(CRV)" disk upload corvus-test-installer-iso "$(ISO)" --format raw \
	  --path "$(CORVUS_BASE_IMAGES_DIR)/SyntheticInstaller/" --if-exists $(IMAGE_POLICY)
