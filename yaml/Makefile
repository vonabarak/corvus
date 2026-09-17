# Dispatcher for images used by the integration-test harness.  Each image's
# directory owns its inputs and build logic; this file only selects it.
IMAGE ?=

IMAGE_DIR_node := corvus-test-node
IMAGE_DIR_vm := corvus-test-vm
IMAGE_DIR_multi-os := multi-os
IMAGE_DIR_windows := windows-server-2025
IMAGE_DIR_installer := corvus-test-installer
IMAGE_DIR_key := ../integration_tests/keys
IMAGE_DIR_gentoo := gentoo-test
IMAGE_DIR_windows-11 := windows-11
IMAGE_DIR_debian-nginx := debian-nginx
IMAGE_DIR_ubuntu-nginx := ubuntu-nginx
IMAGE_DIR_monitor := corvus-monitor

IMAGES := node vm multi-os windows installer key gentoo windows-11 debian-nginx ubuntu-nginx monitor

.PHONY: image image-clean image-rebuild image-check image-cache-clean image-list \
	images images-clean images-rebuild

define require-image
	@case "$(IMAGE)" in $(subst $(space),|,$(IMAGES))) ;; *) \
		echo "Unknown image '$(IMAGE)'. Use: $(IMAGES)" >&2; exit 2 ;; esac
endef
space := $(empty) $(empty)
empty :=

image:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) build

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

images:
	@for image in key multi-os gentoo node vm windows installer windows-11 debian-nginx ubuntu-nginx monitor; do $(MAKE) --no-print-directory image IMAGE=$$image || exit $$?; done

images-clean:
	@for image in monitor ubuntu-nginx debian-nginx windows-11 windows installer vm gentoo multi-os node; do $(MAKE) --no-print-directory image-clean IMAGE=$$image || exit $$?; done

images-rebuild:
	@for image in gentoo multi-os node vm windows installer windows-11 debian-nginx ubuntu-nginx monitor; do $(MAKE) --no-print-directory image-rebuild IMAGE=$$image || exit $$?; done
