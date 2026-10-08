# Shared image lifecycle. Recipes declare outputs and own publication logic.
.DEFAULT_GOAL := all
SHELL := /bin/bash
.SHELLFLAGS := -eu -o pipefail -c
CRV ?= crv
export CRV
IMAGE_POLICY ?= overwrite
# Recipes may refresh dependencies for build while ensure only checks presence.
BUILD_DEPENDENCIES ?= dependencies
IMAGE_CHECK := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))check-images.sh)
OUTPUTS = $(addprefix disk:,$(DISKS)) $(addprefix template:,$(TEMPLATES))

.PHONY: all build ensure dependencies publish check clean rebuild cache-clean
all: build

build: $(BUILD_DEPENDENCIES)
	+$(MAKE) --no-print-directory publish IMAGE_POLICY=$(IMAGE_POLICY)

# Only the check helper's missing-output status permits publication.
define ensure-images
	+@status=0; "$(IMAGE_CHECK)" $(1) || status=$$?; \
	case $$status in \
	  0) ;; \
	  3) $(MAKE) --no-print-directory $(2) IMAGE_POLICY=skip ;; \
	  *) exit $$status ;; \
	esac
endef

ensure: dependencies
	$(call ensure-images,$(OUTPUTS),publish)

check:
	@"$(IMAGE_CHECK)" $(OUTPUTS)

dependencies:

ifneq ($(PIPELINE),)
publish: $(LOCAL_PREREQUISITES)
	"$(CRV)" build $(PIPELINE) --var image_if_exists=$(IMAGE_POLICY) $(BUILD_ARGS) --wait
endif

# Published versions and their users are retained. Cache cleanup is explicit.
clean:
	rm -rf build

rebuild:
	+$(MAKE) --no-print-directory build IMAGE_POLICY=overwrite

cache-clean:
	$(if $(DOWNLOAD_CACHE),rm -rf $(DOWNLOAD_CACHE),@:)
