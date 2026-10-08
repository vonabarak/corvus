IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies
ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media
DISKS := windows-server-2025-eval
TEMPLATES := windows-server-2025
PIPELINE := windows-server-2025.yml
LOCAL_PREREQUISITES := $(ANSWER_ISO)

include ../image.mk

.PHONY: answer-media build-dependencies
answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): Makefile autounattend.xml
	rm -rf $(STAGING)
	mkdir -p $(STAGING)
	cp autounattend.xml $(STAGING)/Autounattend.xml
	$(ISO_TOOL) -R -J -o $@.tmp $(STAGING)
	mv $@.tmp $@

build-dependencies:
	+$(MAKE) -C ../virtio-win build IMAGE_POLICY=$(IMAGE_POLICY)
	+$(MAKE) -C ../windows-server-media build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../virtio-win ensure
	+$(MAKE) -C ../windows-server-media ensure
