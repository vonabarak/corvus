ISO_TOOL ?= mkisofs
ANSWER_ISO := build/autounattend.iso
STAGING := build/answer-media

.PHONY: all build check clean rebuild cache-clean

all: build

$(ANSWER_ISO): autounattend.xml
	mkdir -p $(STAGING)
	cp $< $(STAGING)/Autounattend.xml
	$(ISO_TOOL) -R -J -o $@ $(STAGING)

build: $(ANSWER_ISO)
	@crv -o json disk show windows-server-2025-eval >/dev/null 2>&1 || crv build windows-server-2025.yml --wait

check:
	@crv -o json disk show windows-server-2025-eval >/dev/null
	@crv -o json template show windows-server-2025 >/dev/null

clean:
	@crv -o json template show windows-server-2025 >/dev/null 2>&1 && crv template delete windows-server-2025 || true
	@crv -o json template show windows-server-2025-installer >/dev/null 2>&1 && crv template delete windows-server-2025-installer || true
	@crv -o json disk show windows-server-2025-eval >/dev/null 2>&1 && crv disk delete windows-server-2025-eval || true
	@crv -o json disk show windows-server-2025-autounattend >/dev/null 2>&1 && crv disk delete windows-server-2025-autounattend || true
	rm -rf build

rebuild: clean build
cache-clean:
	@:
