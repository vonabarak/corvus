DISK := debian-12-nginx

.PHONY: all build check clean rebuild cache-clean
all: build

build:
	+$(MAKE) -C ../multi-os build
	@crv -o json disk show $(DISK) >/dev/null 2>&1 || crv build debian-nginx.yml --wait

check:
	@crv -o json disk show $(DISK) >/dev/null

clean:
	@crv -o json disk show $(DISK) >/dev/null 2>&1 && crv disk delete $(DISK) || true

rebuild:
	$(MAKE) clean
	$(MAKE) build
cache-clean:
	@:
