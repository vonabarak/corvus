ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media

.PHONY: all answer-media build clean

all: build

answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): autounattend.xml provision.ps1
	mkdir -p $(STAGING)
	cp autounattend.xml $(STAGING)/Autounattend.xml
	cp provision.ps1 $(STAGING)/provision.ps1
	$(ISO_TOOL) -R -J -o $@ $(STAGING)

build: answer-media
	crv build windows-11.yml --wait

clean:
	crv template delete windows-11-pro || true
	crv template delete windows-11-installer || true
	crv disk delete windows-11-pro-base || true
	crv disk delete windows-11-autounattend || true
	rm -rf build
