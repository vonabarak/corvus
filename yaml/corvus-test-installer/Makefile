DISK := corvus-test-installer-iso
TEMPLATE := corvus-test-installer

.PHONY: all build check clean rebuild cache-clean
all: build

build:
	@crv -o json disk show $(DISK) >/dev/null 2>&1 || ./build-synthetic-installer.sh

check:
	@crv -o json disk show $(DISK) >/dev/null
	@crv -o json template show $(TEMPLATE) >/dev/null

clean:
	@crv -o json template show $(TEMPLATE) >/dev/null 2>&1 && crv template delete $(TEMPLATE) || true
	@crv -o json disk show $(DISK) >/dev/null 2>&1 && crv disk delete $(DISK) || true
	rm -rf build

rebuild:
	$(MAKE) clean
	$(MAKE) build

cache-clean:
	rm -rf cache
