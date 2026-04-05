#!/bin/sh
# Build a Windows Server 2022 test image with QEMU Guest Agent pre-installed.
# Requires: qemu-system-x86_64, qemu-img, mcopy (mtools), mkfs.fat
#
# Usage: ./scripts/build-windows-test-image.sh [--force] [--viewer]
#
# The script automatically downloads the Windows Server 2022 evaluation ISO
# (~5 GB) and the VirtIO-Win drivers ISO (~700 MB) if not already cached.
#
# The unattended installation uses autounattend.xml to partition, install,
# and configure Windows with the QEMU guest agent. The resulting QCOW2
# image is saved to ~/.test-images/windows-server-eval.qcow2

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
IMAGES_DIR="${PROJECT_ROOT}/.test-images"
WIN_ISO="${IMAGES_DIR}/windows-server-2022-eval.iso"
VIRTIO_ISO="${IMAGES_DIR}/virtio-win.iso"
OUTPUT="${IMAGES_DIR}/windows-server-eval.qcow2"
AUTOUNATTEND="$(dirname "$0")/windows-autounattend.xml"
FLOPPY_IMG="${IMAGES_DIR}/autounattend.img"
OVMF_CODE="/usr/share/edk2-ovmf/OVMF_CODE_4M.qcow2"
OVMF_VARS_TEMPLATE="/usr/share/edk2-ovmf/OVMF_VARS_4M.qcow2"
OVMF_VARS="${IMAGES_DIR}/windows-ovmf-vars.qcow2"

WIN_ISO_URL="https://software-static.download.prss.microsoft.com/dbazure/888969d5-f34g-4e03-ac9d-1f9786c66749/SERVER_EVAL_x64FRE_en-us.iso"
VIRTIO_ISO_URL="https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso"

FORCE=0
VIEWER=0
SPICE_SOCK="/tmp/build-windows-spice.sock"
MONITOR_SOCK="/tmp/build-windows-monitor.sock"

for arg in "$@"; do
    case "$arg" in
        --force)  FORCE=1 ;;
        --viewer) VIEWER=1 ;;
    esac
done

# Ensure cache directory exists
mkdir -p "$IMAGES_DIR"

# Always rebuild the floppy image from the latest autounattend.xml
dd if=/dev/zero of="$FLOPPY_IMG" bs=1440k count=1 2>/dev/null
mkfs.fat "$FLOPPY_IMG" >/dev/null
mcopy -i "$FLOPPY_IMG" "$AUTOUNATTEND" ::autounattend.xml

if [ -f "$OUTPUT" ] && [ "$FORCE" -eq 0 ]; then
    echo "Image already exists: $OUTPUT"
    echo "Use --force to rebuild"
    exit 0
fi

# Download a file if not already cached. Verifies the download completed
# by checking that the file size is at least $3 bytes.
download() {
    url="$1"
    dest="$2"
    min_size="$3"
    label="$4"

    if [ -f "$dest" ]; then
        size=$(wc -c < "$dest")
        if [ "$size" -ge "$min_size" ]; then
            echo "$label already cached: $dest"
            return
        fi
        echo "$label incomplete (${size} bytes), re-downloading..."
        rm -f "$dest"
    fi

    echo "Downloading $label..."
    echo "  URL:  $url"
    echo "  Dest: $dest"
    if command -v curl >/dev/null 2>&1; then
        curl -L -o "$dest" -# "$url"
    elif command -v wget >/dev/null 2>&1; then
        wget -O "$dest" "$url"
    else
        echo "Error: neither curl nor wget is available" >&2
        exit 1
    fi

    size=$(wc -c < "$dest")
    if [ "$size" -lt "$min_size" ]; then
        echo "Error: downloaded file is too small (${size} bytes, expected >= ${min_size})" >&2
        rm -f "$dest"
        exit 1
    fi
    echo "$label downloaded (${size} bytes)."
}

# Download ISOs if not cached (~5 GB + ~700 MB)
download "$WIN_ISO_URL"    "$WIN_ISO"    4000000000 "Windows Server 2022 evaluation ISO"
download "$VIRTIO_ISO_URL" "$VIRTIO_ISO" 100000000  "VirtIO-Win drivers ISO"

# Check local prerequisites
for f in "$AUTOUNATTEND" "$OVMF_CODE" "$OVMF_VARS_TEMPLATE"; do
    if [ ! -f "$f" ]; then
        echo "Missing: $f"
        exit 1
    fi
done

for cmd in qemu-system-x86_64 qemu-img mcopy mkfs.fat; do
    if ! command -v "$cmd" >/dev/null 2>&1; then
        echo "Missing command: $cmd" >&2
        exit 1
    fi
done

echo "Building Windows test image..."

# Create the disk image (40GB, thin-provisioned)
qemu-img create -f qcow2 "$OUTPUT" 40G

# Create a copy of OVMF_VARS for this VM
cp "$OVMF_VARS_TEMPLATE" "$OVMF_VARS"

echo "Starting unattended Windows installation..."
echo "This will take 15-30 minutes. The VM will shut down when installation completes."

if [ "$VIEWER" -eq 1 ]; then
    DISPLAY_ARGS="-spice unix=on,addr=$SPICE_SOCK,disable-ticketing=on -device virtio-vga"
    echo "SPICE socket: $SPICE_SOCK"
else
    DISPLAY_ARGS="-display none -vga none -serial stdio"
fi
echo ""

# Run the installation in background when using viewer, foreground otherwise
# shellcheck disable=SC2086
qemu-system-x86_64 \
    -machine type=q35,accel=kvm \
    -cpu host \
    -enable-kvm \
    -m 4096 \
    -smp 4 \
    -drive if=pflash,format=qcow2,readonly=on,file="$OVMF_CODE" \
    -drive if=pflash,format=qcow2,file="$OVMF_VARS" \
    -drive file="$OUTPUT",format=qcow2,if=virtio,cache=writeback \
    -drive file="$WIN_ISO",media=cdrom,index=1 \
    -drive file="$VIRTIO_ISO",media=cdrom,index=2 \
    -drive file="$FLOPPY_IMG",if=floppy,format=raw \
    -device virtio-serial \
    -chardev socket,id=qga0,path=/tmp/qga-test.sock,server=on,wait=off \
    -device virtserialport,chardev=qga0,name=org.qemu.guest_agent.0 \
    -netdev user,id=net0 \
    -device virtio-net-pci,netdev=net0 \
    -monitor unix:"$MONITOR_SOCK",server,nowait \
    $DISPLAY_ARGS &
QEMU_PID=$!

# Send keypresses to get past "Press any key to boot from CD or DVD..." prompt.
# The prompt appears ~3-5s after QEMU starts; we send Enter repeatedly to be safe.
(
    sleep 3
    python3 -c "
import socket, time
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.settimeout(5)
try:
    s.connect('$MONITOR_SOCK')
    s.recv(4096)
    for _ in range(5):
        s.sendall(b'sendkey ret\n')
        time.sleep(1)
except: pass
finally: s.close()
" 2>/dev/null
) &

if [ "$VIEWER" -eq 1 ]; then
    sleep 1
    remote-viewer "spice+unix://$SPICE_SOCK" &
fi

wait $QEMU_PID

echo ""
echo "Windows installation complete."
echo "Image saved to: $OUTPUT"

# Cleanup
rm -f "$FLOPPY_IMG" "$OVMF_VARS" "$SPICE_SOCK" "$MONITOR_SOCK"
