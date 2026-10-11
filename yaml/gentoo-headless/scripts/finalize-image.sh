#!/bin/sh
cat > "$IMAGE_ROOT/etc/corvus-image-info" <<EOF
image: $CORVUS_BUILD_NAME
built: $(date -Iseconds)
EOF

sync
if [ "$IMAGE_ROOT" != / ]; then
  umount "$IMAGE_ROOT"
fi
