#ifndef CORVUS_VDENS_H
#define CORVUS_VDENS_H

#include <sys/types.h>

/*
 * corvus_vdens_start: Fork a child process that creates a network namespace
 * with a VDE-connected TAP interface and runs dnsmasq inside it.
 *
 * The function forks internally (no forkProcess needed from Haskell).
 * The child process is single-threaded and safe for unshare(CLONE_NEWUSER).
 *
 * Parameters:
 *   vde_sock    - Path to VDE switch socket
 *   tap_name    - TAP interface name inside the namespace (e.g. "vde0")
 *   gw_addr     - Gateway IP address to assign to TAP (e.g. "10.0.1.1")
 *   prefix_len  - Subnet prefix length (e.g. 24)
 *   dhcp_start  - DHCP range start address
 *   dhcp_end    - DHCP range end address
 *   subnet_mask - Subnet mask string for dnsmasq (e.g. "255.255.255.0")
 *   dnsmasq_bin - Path to dnsmasq binary
 *   lease_file  - Path to DHCP lease file
 *   out_pid     - Output: PID of the namespace manager child process
 *
 * Returns: 0 on success (child is running, PID written to *out_pid).
 *         -1 on error.
 */
int corvus_vdens_start(
    const char *vde_sock,
    const char *tap_name,
    const char *gw_addr,
    int prefix_len,
    const char *dhcp_start,
    const char *dhcp_end,
    const char *subnet_mask,
    const char *dnsmasq_bin,
    const char *lease_file,
    pid_t *out_pid
);

#endif /* CORVUS_VDENS_H */
