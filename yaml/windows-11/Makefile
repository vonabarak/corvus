ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media

.PHONY: all answer-media build check clean rebuild cache-clean

all: build

answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): autounattend.xml provision.ps1
	mkdir -p $(STAGING)
	cp autounattend.xml $(STAGING)/Autounattend.xml
	cp provision.ps1 $(STAGING)/provision.ps1
	$(ISO_TOOL) -R -J -o $@ $(STAGING)

build: answer-media
	@crv -o json disk show windows-11-pro-base >/dev/null 2>&1 || crv build windows-11.yml --wait

check:
	@crv -o json disk show windows-11-pro-base >/dev/null
	@crv -o json template show windows-11-pro >/dev/null

clean:
	@crv -o json template show windows-11-pro >/dev/null 2>&1 && crv template delete windows-11-pro || true
	@crv -o json template show windows-11-installer >/dev/null 2>&1 && crv template delete windows-11-installer || true
	@crv -o json disk show windows-11-pro-base >/dev/null 2>&1 && crv disk delete windows-11-pro-base || true
	@crv -o json disk show windows-11-autounattend >/dev/null 2>&1 && crv disk delete windows-11-autounattend || true
	rm -rf build

rebuild: clean build
cache-clean:
	@:
