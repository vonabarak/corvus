/*
 * Probe whether a vhost-vsock guest CID is currently unused on the host.
 * See vsock_probe.h for rationale.
 */

#include <fcntl.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <linux/vhost.h>

#include "vsock_probe.h"

#ifndef VHOST_VSOCK_SET_GUEST_CID
/* Older headers may not define this; reproduce from <linux/vhost_types.h>. */
#define VHOST_VSOCK_SET_GUEST_CID _IOW(0xAF, 0x60, unsigned long long)
#endif

int corvus_vsock_cid_available(unsigned long long cid)
{
    int fd = open("/dev/vhost-vsock", O_RDWR);
    if (fd < 0) {
        return -1;
    }

    int rc = ioctl(fd, VHOST_VSOCK_SET_GUEST_CID, &cid);
    int saved = rc < 0 ? 1 : 0; /* 1 = error, 0 = success */
    close(fd);

    if (saved == 0) {
        return 1; /* claimed and released */
    }
    return 0; /* in use or rejected */
}
