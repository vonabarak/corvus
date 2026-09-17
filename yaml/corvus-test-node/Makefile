KEY_DIR := ../../integration_tests/keys
DISK := corvus-test-node
TEMPLATE := corvus-test-node

.PHONY: all build check clean rebuild cache-clean
all: build

build:
	+$(MAKE) -C $(KEY_DIR) build
	@crv -o json template show gentoo-headless >/dev/null 2>&1 || $(MAKE) -C ../gentoo-test build-headless
	@crv -o json disk show $(DISK) >/dev/null 2>&1 || crv build corvus-test-node.yml --wait

check:
	@crv -o json disk show $(DISK) >/dev/null
	@crv -o json template show $(TEMPLATE) >/dev/null
	@crv -o json template show gentoo-headless >/dev/null

clean:
	@crv -o json template show $(TEMPLATE) >/dev/null 2>&1 && crv template delete $(TEMPLATE) || true
	@crv -o json disk show $(DISK) >/dev/null 2>&1 && crv disk delete $(DISK) || true

rebuild:
	$(MAKE) clean
	$(MAKE) build
cache-clean:
	@:
