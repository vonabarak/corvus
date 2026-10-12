.PHONY: image image-ensure image-clean image-rebuild image-check image-cache-clean image-list images images-ensure images-clean images-rebuild dev-node-vm dev-node-vm-create dev-node-vm-database-check dev-node-vm-images dev-node-vm-setup dev-node-vm-clean dev-node-vm-ssh

# Image build commands are dispatched to yaml/Makefile. Each image recipe
# owns its build logic alongside its YAML and supporting files; see
# doc/test-images.md for the inventory and command reference.
image image-ensure image-clean image-rebuild image-check image-cache-clean image-list images images-ensure images-clean images-rebuild:
	+$(MAKE) -C yaml $@

# Instantiate a one-off VM from the `corvus-test-node` template
# (built by `make image IMAGE=node`), attach the developer's
# freshly built binaries over virtiofs at the `corvus_host` tag —
# matching the image's `opt-corvus-bin.mount` — and start it. The
# `corvus_host` mount makes /opt/corvus/bin inside the VM the
# *same* tree as `stack path --local-install-root`/bin/ on the
# host, so a `stack build` on the host is reflected in the VM
# without a rebake.
#
# After it boots, the daemon inside is reachable over VSOCK
# (the image runs corvus-tcp-relay.service: VSOCK:9876 → TCP:9876)
# and SSH is up via systemd's VSOCK socket-activation for the
# `corvus` user.
#
# Idempotent: if the VM already exists the instantiate / shared-dir
# / start steps are skipped. Run `make dev-node-vm-clean` first to
# get a fresh VM.

# Override to rename the manual-testing VM, e.g.:
#   make dev-node-vm DEV_NODE_VM=corvus-foo
DEV_NODE_VM ?= corvus-dev-node
# Backend for fresh setup; existing configurations must match.
DEV_NODE_DATABASE ?= sqlite
export DEV_NODE_DATABASE
# Private half of the SSH key the image bakes into
# /home/corvus/.ssh/authorized_keys at build time
# (see yaml/corvus-test-node/corvus-test-node.yml).
DEV_NODE_SSH_KEY ?= integration_tests/keys/corvus-test-key

# Run each step through a separate recursive Make invocation so even -j
# preserves the order and stops before later steps when an earlier one fails.
dev-node-vm: dev-node-vm-database-check
	+$(MAKE) dev-node-vm-create
	+$(MAKE) dev-node-vm-setup
	+$(MAKE) dev-node-vm-images
	+$(MAKE) dev-node-vm-ssh

# Validate before creation or SSH; other standalone steps do not select a DB.
dev-node-vm-database-check:
	@case "$$DEV_NODE_DATABASE" in sqlite|postgresql) ;; *) \
	  echo "DEV_NODE_DATABASE must be sqlite or postgresql" >&2; exit 1 ;; esac

dev-node-vm-setup: dev-node-vm-database-check

# Build binaries, create the VM and its shares, and boot it.
dev-node-vm-create: build
	@set -eu; \
	  crv() { command "$${CORVUS_CRV:-crv}" "$$@"; }; \
	  if crv -o json vm show "$(DEV_NODE_VM)" >/dev/null 2>&1; then \
	  echo "VM '$(DEV_NODE_VM)' already exists; leaving it alone."; \
	  echo "Run 'make dev-node-vm-clean' to remove it first if you want a fresh VM."; \
	else \
	  stack_bin=$$(stack $(STACK_BUILD_FLAGS) path --local-install-root)/bin; \
	  echo "Instantiating template 'corvus-test-node' as VM '$(DEV_NODE_VM)'"; \
	  crv template instantiate corvus-test-node "$(DEV_NODE_VM)"; \
	  echo "Attaching $$stack_bin -> corvus_host (read-only)"; \
	  crv shared-dir add "$(DEV_NODE_VM)" "$$stack_bin" corvus_host --read-only; \
	  echo "Attaching $(CURDIR) -> corvus_src (read-only, mounted at /mnt/corvus)"; \
	  crv shared-dir add "$(DEV_NODE_VM)" "$(CURDIR)" corvus_src --read-only; \
	  echo "Attaching $$HOME/VMs/BaseImages -> base_images (read-only, mounted at /home/corvus/VMs/BaseImages)"; \
	  crv shared-dir add "$(DEV_NODE_VM)" "$$HOME/VMs/BaseImages" base_images --read-only; \
	  echo "Starting $(DEV_NODE_VM)"; \
	  crv vm start "$(DEV_NODE_VM)" --wait; \
	fi
	@echo
	@echo "VM is ready. Use:"
	@echo "  crv vm show $(DEV_NODE_VM)     # connection details (vsock CID, sockets)"
	@echo "  make dev-node-vm-ssh           # ssh in over VSOCK"
	@echo "  make dev-node-vm-setup         # run setup inside VM once"
	@echo "  make dev-node-vm-images        # register base images after setup"
	@echo "  make dev-node-vm-clean         # reset + delete the VM (and its overlay)"

# Register images without building binaries or creating/starting a VM.
dev-node-vm-images:
	@bash scripts/devnode/register-images.sh --vm "$(DEV_NODE_VM)" --ssh-key "$(DEV_NODE_SSH_KEY)"


# Reset + delete the manual-testing VM and its overlay disk.
# The overlay is template-instantiated, so it's ephemeral by default
# and `vm delete` reaps it together with the VM.
dev-node-vm-clean:
	-crv vm delete --force $(DEV_NODE_VM)


# SSH into the dev VM over VSOCK. Pulls the CID from `crv vm show`
# and uses the test image's baked-in SSH key. Pass extra ssh args
# via `make dev-node-vm-ssh SSH_ARGS='-v'`.
# The setup target runs the guest script with the mounted binaries on PATH.
dev-node-vm-ssh dev-node-vm-setup:
	@cid=$$("$${CORVUS_CRV:-crv}" -o json vm show "$(DEV_NODE_VM)" | jq -r '.vsock_cid // empty'); \
	  if [ -z "$$cid" ]; then \
	    echo "$(DEV_NODE_VM) has no vsock_cid (is the VM up?)" >&2; \
	    exit 1; \
	  fi; \
	  exec ssh -i "$(DEV_NODE_SSH_KEY)" $(SSH_ARGS) corvus@vsock%$$cid \
	    $(if $(filter dev-node-vm-setup,$@),'export PATH=/opt/corvus/bin:$$PATH; exec bash /mnt/corvus/scripts/devnode/setup.sh --database $(DEV_NODE_DATABASE)')
