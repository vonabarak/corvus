ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media
DISKS := windows-11-pro-base
TEMPLATES := windows-11-pro
PIPELINE := windows-11.yml
LOCAL_PREREQUISITES := $(ANSWER_ISO)

include ../image.mk

.PHONY: answer-media
answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): autounattend.xml runtime-unattend.xml provision.ps1
	mkdir -p $(STAGING)
	cp autounattend.xml $(STAGING)/Autounattend.xml
	cp provision.ps1 $(STAGING)/provision.ps1
	cp runtime-unattend.xml $(STAGING)/runtime-unattend.xml
	$(ISO_TOOL) -R -J -o $@ $(STAGING)
