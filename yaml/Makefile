# Dispatcher for images used by the integration-test harness.  Each image's
# directory owns its inputs and build logic; this file only selects it.
IMAGE ?=

IMAGE_DIR_corvus-test-node := gentoo
IMAGE_ARGS_corvus-test-node := IMAGE=corvus-test-node
IMAGE_DIR_corvus-test-vm := gentoo
IMAGE_ARGS_corvus-test-vm := IMAGE=corvus-test-vm
IMAGE_DIR_multi-os := multi-os
IMAGE_DIR_freebsd := freebsd
IMAGE_DIR_freebsd-headless := freebsd
IMAGE_ARGS_freebsd-headless := HEADLESS=1
IMAGE_DIR_virtio-win := virtio-win
IMAGE_DIR_windows-server-media := windows-server-media
IMAGE_DIR_almalinux-10-media := almalinux-10-media
IMAGE_DIR_almalinux-10-desktop := almalinux-10-desktop
IMAGE_DIR_windows := windows-server-2025
IMAGE_DIR_installer := corvus-test-installer
IMAGE_DIR_key := ../integration_tests/keys
IMAGE_DIR_gentoo-corvus-test := gentoo
IMAGE_ARGS_gentoo-corvus-test := IMAGE=gentoo-corvus-test
IMAGE_DIR_gentoo-glibc-systemd-headless := gentoo
IMAGE_ARGS_gentoo-glibc-systemd-headless := IMAGE=gentoo-glibc-systemd-headless
IMAGE_DIR_gentoo-musl-openrc-headless := gentoo
IMAGE_ARGS_gentoo-musl-openrc-headless := IMAGE=gentoo-musl-openrc-headless
IMAGE_DIR_windows-11 := windows-11
IMAGE_DIR_windows-11-desktop := windows-11-desktop
IMAGE_DIR_debian-nginx := debian-nginx
IMAGE_DIR_ubuntu-nginx := ubuntu-nginx
IMAGE_DIR_monitor := corvus-monitor

IMAGES := corvus-test-node corvus-test-vm multi-os freebsd freebsd-headless virtio-win windows-server-media almalinux-10-media almalinux-10-desktop windows installer key gentoo-corvus-test gentoo-glibc-systemd-headless gentoo-musl-openrc-headless windows-11 windows-11-desktop debian-nginx ubuntu-nginx monitor

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
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) build $(IMAGE_ARGS_$(IMAGE))

image-ensure:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) ensure $(IMAGE_ARGS_$(IMAGE))

image-clean:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) clean $(IMAGE_ARGS_$(IMAGE))

image-rebuild:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) rebuild $(IMAGE_ARGS_$(IMAGE))

image-check:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) check $(IMAGE_ARGS_$(IMAGE))

image-cache-clean:
	$(require-image)
	+$(MAKE) -C $(IMAGE_DIR_$(IMAGE)) cache-clean $(IMAGE_ARGS_$(IMAGE))

image-list:
	@printf '%s\n' $(IMAGES)

# Shared sources are prepared before consumers; avoid refreshing them again.
# The musl runtime prepares its intermediate builder through its dependency chain.
images:
	@for image in key multi-os freebsd freebsd-headless gentoo-corvus-test gentoo-musl-openrc-headless corvus-test-node corvus-test-vm virtio-win windows-server-media almalinux-10-media almalinux-10-desktop windows installer windows-11 windows-11-desktop debian-nginx ubuntu-nginx monitor; do \
	  case "$$image" in gentoo-musl-openrc-headless) \
	    $(MAKE) --no-print-directory -C gentoo build IMAGE=gentoo-musl-openrc-builder BUILD_DEPENDENCIES=dependencies && \
	    $(MAKE) --no-print-directory image IMAGE=$$image BUILD_DEPENDENCIES=dependencies ;; \
	  freebsd-headless|corvus-test-node|corvus-test-vm|almalinux-10-desktop|windows|windows-11|windows-11-desktop|debian-nginx|ubuntu-nginx|monitor) \
	    $(MAKE) --no-print-directory image IMAGE=$$image BUILD_DEPENDENCIES=dependencies ;; \
	  *) $(MAKE) --no-print-directory image IMAGE=$$image ;; esac || exit $$?; \
	done

images-clean:
	@for image in monitor ubuntu-nginx debian-nginx windows-11-desktop windows-11 windows almalinux-10-desktop almalinux-10-media virtio-win windows-server-media installer corvus-test-vm gentoo-musl-openrc-headless gentoo-corvus-test gentoo-glibc-systemd-headless freebsd-headless freebsd multi-os corvus-test-node; do $(MAKE) --no-print-directory image-clean IMAGE=$$image || exit $$?; done

images-rebuild:
	@for image in gentoo-corvus-test gentoo-musl-openrc-headless multi-os freebsd freebsd-headless corvus-test-node corvus-test-vm virtio-win windows-server-media almalinux-10-media almalinux-10-desktop windows installer windows-11 windows-11-desktop debian-nginx ubuntu-nginx monitor; do \
	  case "$$image" in gentoo-musl-openrc-headless) \
	    $(MAKE) --no-print-directory -C gentoo rebuild IMAGE=gentoo-musl-openrc-builder BUILD_DEPENDENCIES=dependencies && \
	    $(MAKE) --no-print-directory image-rebuild IMAGE=$$image BUILD_DEPENDENCIES=dependencies ;; \
	  freebsd-headless|corvus-test-node|corvus-test-vm|almalinux-10-desktop|windows|windows-11|windows-11-desktop|debian-nginx|ubuntu-nginx|monitor) \
	    $(MAKE) --no-print-directory image-rebuild IMAGE=$$image BUILD_DEPENDENCIES=dependencies ;; \
	  *) $(MAKE) --no-print-directory image-rebuild IMAGE=$$image ;; esac || exit $$?; \
	done

images-ensure:
	@for image in key multi-os freebsd freebsd-headless gentoo-corvus-test gentoo-musl-openrc-headless corvus-test-node corvus-test-vm virtio-win windows-server-media almalinux-10-media almalinux-10-desktop windows installer windows-11 windows-11-desktop debian-nginx ubuntu-nginx monitor; do $(MAKE) --no-print-directory image-ensure IMAGE=$$image || exit $$?; done
