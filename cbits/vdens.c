/*
 * Corvus namespace manager — derived from vdens by Renzo Davoli & Davide Berardi.
 *
 * Creates an unprivileged user+network+UTS namespace, bridges a VDE switch
 * to a TAP interface inside the namespace, and exec's dnsmasq.
 *
 * Original vdens: Copyright (C) 2016 Renzo Davoli, Davide Berardi — GPLv2+
 * Modifications for Corvus: simplified to single-network dnsmasq use-case,
 * replaced libcap with direct capget/capset syscalls.
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <sched.h>
#include <signal.h>
#include <poll.h>
#include <limits.h>
#include <sys/wait.h>
#include <sys/prctl.h>
#include <sys/ioctl.h>
#include <sys/signalfd.h>
#include <sys/syscall.h>
#include <net/if.h>
#include <linux/if_tun.h>
#include <linux/capability.h>
#include <libvdeplug.h>

#include "vdens.h"

/* --------------------------------------------------------------------------
 * Constants
 * -------------------------------------------------------------------------- */

#ifndef VDE_ETHBUFSIZE
#define VDE_ETHBUFSIZE (9216 + 14 + 4)
#endif

#define POLLTERM (POLLHUP | POLLERR | POLLNVAL)

/* Capability constants */
#define CAP_NET_BIND_SERVICE 10
#define CAP_NET_BROADCAST    11
#define CAP_NET_ADMIN        12
#define CAP_NET_RAW          13

#define PR_CAP_AMBIENT       47
#define PR_CAP_AMBIENT_RAISE  2

/* _LINUX_CAPABILITY_VERSION_3 for capget/capset syscalls */
#define LINUX_CAPABILITY_VERSION_3 0x20080522

/* --------------------------------------------------------------------------
 * UID/GID mapping
 * -------------------------------------------------------------------------- */

static void uid_gid_map(pid_t pid)
{
    char path[PATH_MAX];
    FILE *f;
    uid_t euid = geteuid();
    gid_t egid = getegid();

    snprintf(path, sizeof(path), "/proc/%d/uid_map", pid);
    f = fopen(path, "w");
    if (f) {
        fprintf(f, "%d %d 1\n", euid, euid);
        fclose(f);
    }

    snprintf(path, sizeof(path), "/proc/%d/setgroups", pid);
    f = fopen(path, "w");
    if (f) {
        fprintf(f, "deny\n");
        fclose(f);
    }

    snprintf(path, sizeof(path), "/proc/%d/gid_map", pid);
    f = fopen(path, "w");
    if (f) {
        fprintf(f, "%d %d 1\n", egid, egid);
        fclose(f);
    }
}

/* --------------------------------------------------------------------------
 * Capabilities (without libcap)
 * -------------------------------------------------------------------------- */

struct cap_header {
    __u32 version;
    int pid;
};

struct cap_data {
    __u32 effective;
    __u32 permitted;
    __u32 inheritable;
};

static int set_ambient_caps(void)
{
    int caps[] = {CAP_NET_ADMIN, CAP_NET_RAW, CAP_NET_BIND_SERVICE, CAP_NET_BROADCAST};
    int ncaps = sizeof(caps) / sizeof(caps[0]);
    struct cap_header hdr;
    /* capget/capset use two cap_data structs for 64-bit capability sets */
    struct cap_data data[2];

    memset(&hdr, 0, sizeof(hdr));
    memset(data, 0, sizeof(data));
    hdr.version = LINUX_CAPABILITY_VERSION_3;
    hdr.pid = 0; /* current process */

    if (syscall(SYS_capget, &hdr, data) < 0)
        return -1;

    /* Set the inheritable bits for our capabilities */
    for (int i = 0; i < ncaps; i++) {
        int idx = caps[i] >> 5;   /* which __u32 */
        int bit = caps[i] & 0x1f; /* which bit */
        data[idx].inheritable |= (1u << bit);
    }

    if (syscall(SYS_capset, &hdr, data) < 0)
        return -1;

    /* Raise ambient capabilities */
    for (int i = 0; i < ncaps; i++) {
        if (prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_RAISE, caps[i], 0, 0) < 0)
            return -1;
    }

    return 0;
}

/* --------------------------------------------------------------------------
 * TAP interface
 * -------------------------------------------------------------------------- */

static int open_tap(const char *name)
{
    struct ifreq ifr;
    int fd;

    fd = open("/dev/net/tun", O_RDWR | O_CLOEXEC);
    if (fd < 0)
        return -1;

    memset(&ifr, 0, sizeof(ifr));
    ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
    snprintf(ifr.ifr_name, IFNAMSIZ, "%s", name);

    if (ioctl(fd, TUNSETIFF, &ifr) < 0) {
        close(fd);
        return -1;
    }

    return fd;
}

/* --------------------------------------------------------------------------
 * Network configuration inside namespace
 * -------------------------------------------------------------------------- */

static int configure_network(const char *tap_name, const char *gw_addr, int prefix_len)
{
    char cmd[512];
    int ret;

    /* Bring up loopback */
    ret = system("ip link set lo up");
    if (ret != 0)
        return -1;

    /* Assign IP to TAP */
    snprintf(cmd, sizeof(cmd), "ip addr add %s/%d dev %s", gw_addr, prefix_len, tap_name);
    ret = system(cmd);
    if (ret != 0)
        return -1;

    /* Bring up TAP */
    snprintf(cmd, sizeof(cmd), "ip link set %s up", tap_name);
    ret = system(cmd);
    if (ret != 0)
        return -1;

    return 0;
}

/* --------------------------------------------------------------------------
 * VDE <-> TAP bridge loop
 * -------------------------------------------------------------------------- */

static void plug2tap(VDECONN *conn, int tapfd, int sigfd)
{
    char buf[VDE_ETHBUFSIZE];
    ssize_t n;
    struct pollfd pfd[3];

    pfd[0].fd = vde_datafd(conn);
    pfd[0].events = POLLIN;
    pfd[1].fd = tapfd;
    pfd[1].events = POLLIN;
    pfd[2].fd = sigfd;
    pfd[2].events = POLLIN;

    while (poll(pfd, 3, -1) >= 0) {
        /* VDE -> TAP */
        if (pfd[0].revents & POLLIN) {
            n = vde_recv(conn, buf, VDE_ETHBUFSIZE, 0);
            if (n <= 0)
                break;
            if (write(tapfd, buf, n) < 0)
                break;
        }

        /* TAP -> VDE */
        if (pfd[1].revents & POLLIN) {
            n = read(tapfd, buf, VDE_ETHBUFSIZE);
            if (n <= 0)
                break;
            vde_send(conn, buf, n, 0);
        }

        /* Error on either fd */
        if ((pfd[0].revents & POLLTERM) || (pfd[1].revents & POLLTERM))
            break;

        /* SIGCHLD (dnsmasq exited) or SIGTERM (stop requested) */
        if (pfd[2].revents & POLLIN) {
            struct signalfd_siginfo si;
            ssize_t r = read(sigfd, &si, sizeof(si));
            (void)r;
            break;
        }
    }

    vde_close(conn);
    close(tapfd);
}

/* --------------------------------------------------------------------------
 * Start dnsmasq
 * -------------------------------------------------------------------------- */

static pid_t start_dnsmasq(
    const char *dnsmasq_bin,
    const char *gw_addr,
    const char *dhcp_start,
    const char *dhcp_end,
    const char *subnet_mask,
    const char *lease_file)
{
    char listen_arg[256];
    char range_arg[512];
    char lease_arg[PATH_MAX];

    snprintf(listen_arg, sizeof(listen_arg), "--listen-address=%s", gw_addr);
    snprintf(range_arg, sizeof(range_arg), "--dhcp-range=%s,%s,%s,12h",
             dhcp_start, dhcp_end, subnet_mask);
    snprintf(lease_arg, sizeof(lease_arg), "--dhcp-leasefile=%s", lease_file);

    pid_t pid = fork();
    if (pid < 0)
        return -1;

    if (pid == 0) {
        /* Child: restore default signal handling before exec.
         * The parent set SIG_IGN for SIGTERM (for signalfd), but
         * dnsmasq needs SIGTERM to shut down cleanly. */
        signal(SIGTERM, SIG_DFL);
        signal(SIGINT, SIG_DFL);
        sigset_t empty;
        sigemptyset(&empty);
        sigprocmask(SIG_SETMASK, &empty, NULL);
        execlp(dnsmasq_bin, dnsmasq_bin,
               "--conf-file=/dev/null",
               "--bind-interfaces",
               listen_arg,
               "--except-interface=lo",
               range_arg,
               "--keep-in-foreground",
               "--no-hosts",
               lease_arg,
               (char *)NULL);
        _exit(127); /* exec failed */
    }

    return pid;
}

/* --------------------------------------------------------------------------
 * Namespace manager child process (runs after fork, single-threaded)
 * -------------------------------------------------------------------------- */

static void namespace_child(
    const char *vde_sock,
    const char *tap_name,
    const char *gw_addr,
    int prefix_len,
    const char *dhcp_start,
    const char *dhcp_end,
    const char *subnet_mask,
    const char *dnsmasq_bin,
    const char *lease_file,
    int ready_fd)
{
    int ns_pipe[2];
    pid_t helper_pid;
    int unshare_rv;
    ssize_t rwlen;

    /* --- Step 1: Create namespace via unshare with helper child --- */

    if (pipe2(ns_pipe, O_CLOEXEC) < 0) {
        fprintf(stderr, "corvus_vdens: pipe2 failed: %s\n", strerror(errno));
        _exit(1);
    }

    helper_pid = fork();
    if (helper_pid < 0) {
        fprintf(stderr, "corvus_vdens: fork helper failed: %s\n", strerror(errno));
        _exit(1);
    }

    if (helper_pid == 0) {
        /* Helper child: wait for unshare result, then write uid/gid maps */
        close(ns_pipe[1]);
        rwlen = read(ns_pipe[0], &unshare_rv, sizeof(unshare_rv));
        if (rwlen == sizeof(unshare_rv) && unshare_rv == 0)
            uid_gid_map(getppid());
        _exit(0);
    }

    /* Parent: perform unshare */
    close(ns_pipe[0]);
    unshare_rv = unshare(CLONE_NEWUSER | CLONE_NEWNET | CLONE_NEWUTS);
    rwlen = write(ns_pipe[1], &unshare_rv, sizeof(unshare_rv));
    close(ns_pipe[1]);

    if (rwlen != (ssize_t)sizeof(unshare_rv) || unshare_rv != 0) {
        fprintf(stderr, "corvus_vdens: unshare failed: %s (rv=%d)\n", strerror(errno), unshare_rv);
        waitpid(helper_pid, NULL, 0);
        _exit(1);
    }

    /* Wait for helper to finish writing maps */
    waitpid(helper_pid, NULL, 0);

    /* --- Step 2: Set capabilities --- */

    if (set_ambient_caps() < 0) {
        fprintf(stderr, "corvus_vdens: set_ambient_caps failed: %s\n", strerror(errno));
        _exit(1);
    }

    /* --- Step 3: Open TAP interface --- */

    int tapfd = open_tap(tap_name);
    if (tapfd < 0) {
        fprintf(stderr, "corvus_vdens: open_tap(%s) failed: %s\n", tap_name, strerror(errno));
        _exit(1);
    }

    /* --- Step 4: Connect to VDE switch --- */

    VDECONN *conn = vde_open((char *)vde_sock, "corvus", NULL);
    if (conn == NULL) {
        fprintf(stderr, "corvus_vdens: vde_open(%s) failed: %s\n", vde_sock, strerror(errno));
        close(tapfd);
        _exit(1);
    }

    /* --- Step 5: Configure network inside namespace --- */

    if (configure_network(tap_name, gw_addr, prefix_len) < 0) {
        fprintf(stderr, "corvus_vdens: configure_network failed\n");
        vde_close(conn);
        close(tapfd);
        _exit(1);
    }

    /* --- Step 6: Set up signal handling (SIGCHLD + SIGTERM) --- */

    sigset_t sigmask;
    sigemptyset(&sigmask);
    sigaddset(&sigmask, SIGCHLD);
    sigaddset(&sigmask, SIGTERM);
    sigprocmask(SIG_BLOCK, &sigmask, NULL);
    int sigfd = signalfd(-1, &sigmask, SFD_CLOEXEC);
    if (sigfd < 0) {
        fprintf(stderr, "corvus_vdens: signalfd failed: %s\n", strerror(errno));
        vde_close(conn);
        close(tapfd);
        _exit(1);
    }

    /* --- Step 7: Start dnsmasq --- */

    pid_t dnsmasq_pid = start_dnsmasq(dnsmasq_bin, gw_addr,
                                       dhcp_start, dhcp_end,
                                       subnet_mask, lease_file);
    if (dnsmasq_pid < 0) {
        fprintf(stderr, "corvus_vdens: start_dnsmasq failed: %s\n", strerror(errno));
        close(sigfd);
        vde_close(conn);
        close(tapfd);
        _exit(1);
    }

    /* --- Step 8: Signal readiness to Haskell parent --- */

    if (ready_fd >= 0) {
        char ready = 1;
        ssize_t w = write(ready_fd, &ready, 1);
        (void)w;
        close(ready_fd);
    }

    /* --- Step 9: Run bridge loop until dnsmasq exits --- */

    plug2tap(conn, tapfd, sigfd);

    /* --- Step 10: Clean up --- */

    kill(dnsmasq_pid, SIGTERM);
    waitpid(dnsmasq_pid, NULL, 0);
    close(sigfd);
    _exit(0);
}

/* --------------------------------------------------------------------------
 * Main entry point — called from Haskell
 * -------------------------------------------------------------------------- */

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
    pid_t *out_pid)
{
    int ready_pipe[2];

    if (pipe2(ready_pipe, O_CLOEXEC) < 0)
        return -1;

    pid_t child = fork();
    if (child < 0) {
        close(ready_pipe[0]);
        close(ready_pipe[1]);
        return -1;
    }

    if (child == 0) {
        /* Child: single-threaded, safe for unshare.
         * Replace GHC's signal handlers — GHC's RTS installs C-level
         * handlers that write to a pipe shared with the parent process.
         * Without this, sending SIGTERM to the child would trigger the
         * parent daemon's shutdown handler via the shared pipe.
         *
         * We use SIG_IGN (not SIG_DFL) for SIGTERM/SIGINT because signalfd
         * cannot deliver signals with SIG_DFL disposition. The signals are
         * later blocked via sigprocmask and consumed via signalfd in the
         * bridge loop. SIG_IGN signals ARE queued and delivered by signalfd
         * when also blocked via sigprocmask. */
        signal(SIGTERM, SIG_IGN);
        signal(SIGINT, SIG_IGN);
        signal(SIGCHLD, SIG_DFL);
        close(ready_pipe[0]);
        namespace_child(vde_sock, tap_name, gw_addr, prefix_len,
                        dhcp_start, dhcp_end, subnet_mask,
                        dnsmasq_bin, lease_file, ready_pipe[1]);
        /* namespace_child calls _exit, never returns */
        _exit(1);
    }

    /* Parent: close write end, wait for ready signal from child */
    close(ready_pipe[1]);

    char buf;
    ssize_t n = read(ready_pipe[0], &buf, 1);
    close(ready_pipe[0]);

    if (n != 1) {
        /* Child failed before signaling readiness — reap it */
        waitpid(child, NULL, 0);
        return -1;
    }

    *out_pid = child;
    return 0;
}
