DISKS := corvus-monitor
TEMPLATES :=
PIPELINE := corvus-monitor.yml
CORVUS_WEB_TARGET ?= 192.168.72.1:8080
BUILD_ARGS = $(if $(CORVUS_WEB_TARGET),--var corvus_web_target="$(CORVUS_WEB_TARGET)")

IMAGE_POLICY ?= update
BUILD_DEPENDENCIES := build-dependencies

include ../image.mk

.PHONY: build-dependencies

# Refresh upstream inputs for build; ensure only fills missing outputs.
build-dependencies:
	+$(MAKE) -C ../multi-os build IMAGE_POLICY=$(IMAGE_POLICY)

dependencies:
	+$(MAKE) -C ../multi-os ensure
