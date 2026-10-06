ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media

.PHONY: all answer-media build check clean rebuild cache-clean

all: build

answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): autounattend.xml runtime-unattend.xml provision.ps1
	mkdir -p $(STAGING)
	cp autounattend.xml $(STAGING)/Autounattend.xml
	cp provision.ps1 $(STAGING)/provision.ps1
	cp runtime-unattend.xml $(STAGING)/runtime-unattend.xml
	$(ISO_TOOL) -R -J -o $@ $(STAGING)

build: answer-media
	crv build windows-11.yml --wait

check:
	@crv -o json disk show windows-11-pro-base >/dev/null
	@crv -o json template show windows-11-pro >/dev/null

clean:
	@if crv -o json template show windows-11-pro >/dev/null 2>&1; then crv template delete windows-11-pro; fi
	@if crv -o json template show windows-11-installer >/dev/null 2>&1; then crv template delete windows-11-installer; fi
	@if crv -o json disk show windows-11-pro-base >/dev/null 2>&1; then crv disk delete windows-11-pro-base; fi
	@if crv -o json disk show windows-11-autounattend >/dev/null 2>&1; then crv disk delete windows-11-autounattend; fi
	rm -rf build

rebuild: clean
	$(MAKE) build
cache-clean:
	@:
