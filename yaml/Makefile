# Dispatcher for images used by the integration-test harness.  Each image's
# directory owns its inputs and build logic; this file only selects it.
IMAGE ?=

IMAGE_DIR_node := corvus-test-node
IMAGE_DIR_vm := corvus-test-vm
IMAGE_DIR_multi-os := multi-os
IMAGE_DIR_virtio-win := virtio-win
IMAGE_DIR_windows-server-media := windows-server-media
IMAGE_DIR_windows := windows-server-2025
IMAGE_DIR_installer := corvus-test-installer
IMAGE_DIR_key := ../integration_tests/keys
IMAGE_DIR_gentoo := gentoo-test
IMAGE_DIR_gentoo-headless := gentoo-headless
IMAGE_DIR_windows-11 := windows-11
IMAGE_DIR_debian-nginx := debian-nginx
IMAGE_DIR_ubuntu-nginx := ubuntu-nginx
IMAGE_DIR_monitor := corvus-monitor

IMAGES := node vm multi-os virtio-win windows-server-media windows installer key gentoo gentoo-headless windows-11 debian-nginx ubuntu-nginx monitor

.PHONY: image image-ensure image-clean image-rebuild image-check image-cache-clean image-list \
	images images-ensure images-clean images-rebuild

define require-image
	@case "$(IMAGE)" in $(subst $(space),|,$(IMAGES))) ;; *) \
		echo "Unknown image '$(IMAGE)'. Use: $(IMAGES)" >&2; exit 2 ;; esac
endef
space := $(empty) $(empty)
empty :=

image:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) build

image-ensure:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) ensure

image-clean:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) clean

image-rebuild:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) rebuild

image-check:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) check

image-cache-clean:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) cache-clean

image-list:
	@printf '%s\n' $(IMAGES)

# Shared sources are prepared before consumers; avoid refreshing them again.
images:
	@for image in key multi-os gentoo node vm virtio-win windows-server-media windows installer windows-11 debian-nginx ubuntu-nginx monitor; do \
	  case "$$image" in node|vm|windows|windows-11|debian-nginx|ubuntu-nginx|monitor) \
	    $(MAKE) --no-print-directory image IMAGE=$$image BUILD_DEPENDENCIES=dependencies ;; \
	  *) $(MAKE) --no-print-directory image IMAGE=$$image ;; esac || exit $$?; \
	done

images-clean:
	@for image in monitor ubuntu-nginx debian-nginx windows-11 windows virtio-win windows-server-media installer vm gentoo gentoo-headless multi-os node; do $(MAKE) --no-print-directory image-clean IMAGE=$$image || exit $$?; done

images-rebuild:
	@for image in gentoo multi-os node vm virtio-win windows-server-media windows installer windows-11 debian-nginx ubuntu-nginx monitor; do \
	  case "$$image" in node|vm|windows|windows-11|debian-nginx|ubuntu-nginx|monitor) \
	    $(MAKE) --no-print-directory image-rebuild IMAGE=$$image BUILD_DEPENDENCIES=dependencies ;; \
	  *) $(MAKE) --no-print-directory image-rebuild IMAGE=$$image ;; esac || exit $$?; \
	done

images-ensure:
	@for image in key multi-os gentoo node vm virtio-win windows-server-media windows installer windows-11 debian-nginx ubuntu-nginx monitor; do $(MAKE) --no-print-directory image-ensure IMAGE=$$image || exit $$?; done
