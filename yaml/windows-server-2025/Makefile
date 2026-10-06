ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media
DISKS := windows-server-2025-eval
TEMPLATES := windows-server-2025
PIPELINE := windows-server-2025.yml
LOCAL_PREREQUISITES := $(ANSWER_ISO)

include ../image.mk

.PHONY: answer-media
answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): autounattend.xml
	mkdir -p $(STAGING)
	cp $< $(STAGING)/Autounattend.xml
	$(ISO_TOOL) -R -J -o $@ $(STAGING)
