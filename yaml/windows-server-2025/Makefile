ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media

.PHONY: all build clean

all: build

$(ANSWER_ISO): autounattend.xml
	mkdir -p $(STAGING)
	cp $< $(STAGING)/Autounattend.xml
	$(ISO_TOOL) -R -J -o $@ $(STAGING)

build: $(ANSWER_ISO)
	crv build windows-server-2025.yml --wait

clean:
	rm -rf build
