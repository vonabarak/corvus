ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media

.PHONY: all answer-media build clean

all: build

answer-media: $(ANSWER_ISO)

$(ANSWER_ISO): autounattend.xml
	mkdir -p $(STAGING)
	cp $< $(STAGING)/Autounattend.xml
	$(ISO_TOOL) -R -J -o $@ $(STAGING)

build: answer-media
	crv build windows-11.yml --wait

clean:
	rm -rf build
