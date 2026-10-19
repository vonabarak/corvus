IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies
ISO_TOOL ?= mkisofs
ANSWER_ISO := build/kickstart.iso
STAGING := build/answer-media
DISKS := almalinux-10-desktop-base
TEMPLATES := almalinux-10-desktop
PIPELINE := almalinux-10-desktop.yml
LOCAL_PREREQUISITES := $(ANSWER_ISO)

include ../image.mk

.PHONY: answer-media build-dependencies
answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): Makefile ks.cfg
	rm -rf $(STAGING)
	mkdir -p $(STAGING) $(dir $(ANSWER_ISO))
	cp ks.cfg $(STAGING)/ks.cfg
	$(ISO_TOOL) -R -J -V OEMDRV -o $@.tmp $(STAGING)
	mv $@.tmp $@

build-dependencies:
	+$(MAKE) -C ../almalinux-10-media build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../almalinux-10-media ensure
