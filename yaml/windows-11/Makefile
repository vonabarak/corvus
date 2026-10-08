IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies
ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media
DISKS := windows-11-pro-base
TEMPLATES := windows-11-pro
PIPELINE := windows-11.yml
LOCAL_PREREQUISITES := $(ANSWER_ISO)

include ../image.mk

.PHONY: answer-media build-dependencies
answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): Makefile autounattend.xml runtime-unattend.xml provision.ps1
	rm -rf $(STAGING)
	mkdir -p $(STAGING)
	cp autounattend.xml $(STAGING)/Autounattend.xml
	cp provision.ps1 $(STAGING)/provision.ps1
	cp runtime-unattend.xml $(STAGING)/runtime-unattend.xml
	$(ISO_TOOL) -R -J -o $@.tmp $(STAGING)
	mv $@.tmp $@

build-dependencies:
	@"$(IMAGE_CHECK)" disk:windows-11-iso
	+$(MAKE) -C ../virtio-win build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	@"$(IMAGE_CHECK)" disk:windows-11-iso
	+$(MAKE) -C ../virtio-win ensure
