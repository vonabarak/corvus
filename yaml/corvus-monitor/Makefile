DISKS := corvus-monitor
TEMPLATES :=
PIPELINE := corvus-monitor.yml
BUILD_ARGS = $(if $(CORVUS_WEB_TARGET),--var corvus_web_target="$(CORVUS_WEB_TARGET)")

include ../image.mk

dependencies:
	+$(MAKE) -C ../multi-os ensure
