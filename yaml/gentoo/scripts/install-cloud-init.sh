#!/bin/sh
# Install Cloud-init and its Python build helpers into the overlay root.
env -u SYSROOT emerge --buildpkg --sysroot="$IMAGE_ROOT" --root="$IMAGE_ROOT" \
  --noreplace --usepkg --binpkg-respect-use=y --quiet-build \
  app-emulation/cloud-init \
  dev-python/gpep517 \
  dev-python/installer

systemctl --root="$IMAGE_ROOT" enable \
  cloud-init.target \
  cloud-init-main.service \
  cloud-init-local.service \
  cloud-config.service \
  cloud-final.service \
  cloud-init-hotplugd.socket

install -d "$IMAGE_ROOT/etc/cloud/cloud.cfg.d"
cat > "$IMAGE_ROOT/etc/cloud/cloud.cfg.d/90_corvus_nocloud.cfg" <<'EOF'
# Corvus supplies NoCloud data; pin it to avoid early CDROM detection races.
datasource_list: [ NoCloud, None ]
# en_US.UTF-8 is generated at bake time using glibc's localedef. Gentoo's
# cloud-init locale module calls locale-gen, which this image does not ship.
locale: false
EOF
