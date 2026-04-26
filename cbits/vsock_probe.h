#ifndef CORVUS_VSOCK_PROBE_H
#define CORVUS_VSOCK_PROBE_H

/*
 * corvus_vsock_cid_available: Test whether a guest CID is currently
 * unclaimed on the host kernel.
 *
 * The kernel's vhost-vsock driver enforces global CID uniqueness:
 * QEMU's vhost-vsock-pci device fails at startup if another process
 * already owns the requested guest_cid. The driver's hash-table check
 * runs inside VHOST_VSOCK_SET_GUEST_CID. We replicate that check by
 * opening /dev/vhost-vsock, attempting the same ioctl, and closing
 * immediately — closing releases the CID, so the brief claim is
 * race-safe enough for the allocator.
 *
 * Returns:
 *    1  - CID is available (briefly held and released).
 *    0  - CID is in use (EADDRINUSE) or otherwise rejected.
 *   -1  - Probe failed (e.g. /dev/vhost-vsock not present, no
 *         permission). Caller should fall back to assuming "available"
 *         rather than block startup; QEMU will fail loudly if wrong.
 */
int corvus_vsock_cid_available(unsigned long long cid);

#endif /* CORVUS_VSOCK_PROBE_H */
