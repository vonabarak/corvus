DISK := corvus-monitor

.PHONY: all build check clean rebuild cache-clean
all: build

build:
	+$(MAKE) -C ../multi-os build
	@crv -o json disk show $(DISK) >/dev/null 2>&1 || crv build corvus-monitor.yml $(if $(CORVUS_WEB_TARGET),--var corvus_web_target="$(CORVUS_WEB_TARGET)") --wait

check:
	@crv -o json disk show $(DISK) >/dev/null

clean:
	@crv -o json disk show $(DISK) >/dev/null 2>&1 && crv disk delete $(DISK) || true

rebuild:
	$(MAKE) clean
	$(MAKE) build
cache-clean:
	@:
