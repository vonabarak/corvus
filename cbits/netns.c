/*
 * Corvus namespace manager — creates an unprivileged user+network+UTS namespace
 * and keeps it alive for the daemon to manage bridges, dnsmasq, and QEMU
 * processes via nsenter.
 *
 * Derived from vdens by Renzo Davoli & Davide Berardi.
 * Original vdens: Copyright (C) 2016 Renzo Davoli, Davide Berardi — GPLv2+
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
#include <sys/signalfd.h>
#include <sys/syscall.h>
#include <linux/capability.h>

#include <net/if.h>
#include <linux/if_tun.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>

#include "netns.h"

/* --------------------------------------------------------------------------
 * Constants
 * -------------------------------------------------------------------------- */

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
    struct cap_data data[2];

    memset(&hdr, 0, sizeof(hdr));
    memset(data, 0, sizeof(data));
    hdr.version = LINUX_CAPABILITY_VERSION_3;
    hdr.pid = 0;

    if (syscall(SYS_capget, &hdr, data) < 0)
        return -1;

    for (int i = 0; i < ncaps; i++) {
        int idx = caps[i] >> 5;
        int bit = caps[i] & 0x1f;
        data[idx].inheritable |= (1u << bit);
    }

    if (syscall(SYS_capset, &hdr, data) < 0)
        return -1;

    for (int i = 0; i < ncaps; i++) {
        if (prctl(PR_CAP_AMBIENT, PR_CAP_AMBIENT_RAISE, caps[i], 0, 0) < 0)
            return -1;
    }

    return 0;
}

/* --------------------------------------------------------------------------
 * Namespace child process (runs after fork, single-threaded)
 * -------------------------------------------------------------------------- */

static void namespace_child(int ready_fd)
{
    int ns_pipe[2];
    pid_t helper_pid;
    int unshare_rv;
    ssize_t rwlen;

    /* --- Step 1: Create namespace via unshare with helper child --- */

    if (pipe2(ns_pipe, O_CLOEXEC) < 0) {
        fprintf(stderr, "corvus_netns: pipe2 failed: %s\n", strerror(errno));
        _exit(1);
    }

    helper_pid = fork();
    if (helper_pid < 0) {
        fprintf(stderr, "corvus_netns: fork helper failed: %s\n", strerror(errno));
        _exit(1);
    }

    if (helper_pid == 0) {
        close(ns_pipe[1]);
        rwlen = read(ns_pipe[0], &unshare_rv, sizeof(unshare_rv));
        if (rwlen == sizeof(unshare_rv) && unshare_rv == 0)
            uid_gid_map(getppid());
        _exit(0);
    }

    close(ns_pipe[0]);
    unshare_rv = unshare(CLONE_NEWUSER | CLONE_NEWNET | CLONE_NEWUTS);
    rwlen = write(ns_pipe[1], &unshare_rv, sizeof(unshare_rv));
    close(ns_pipe[1]);

    if (rwlen != (ssize_t)sizeof(unshare_rv) || unshare_rv != 0) {
        fprintf(stderr, "corvus_netns: unshare failed: %s (rv=%d)\n", strerror(errno), unshare_rv);
        waitpid(helper_pid, NULL, 0);
        _exit(1);
    }

    waitpid(helper_pid, NULL, 0);

    /* --- Step 2: Set capabilities --- */

    if (set_ambient_caps() < 0) {
        fprintf(stderr, "corvus_netns: set_ambient_caps failed: %s\n", strerror(errno));
        _exit(1);
    }

    /* --- Step 3: Bring up loopback --- */

    if (system("ip link set lo up") != 0) {
        fprintf(stderr, "corvus_netns: failed to bring up loopback\n");
        _exit(1);
    }

    /* --- Step 4: Set up signal handling --- */

    sigset_t sigmask;
    sigemptyset(&sigmask);
    sigaddset(&sigmask, SIGCHLD);
    sigaddset(&sigmask, SIGTERM);
    sigprocmask(SIG_BLOCK, &sigmask, NULL);
    int sigfd = signalfd(-1, &sigmask, SFD_CLOEXEC);
    if (sigfd < 0) {
        fprintf(stderr, "corvus_netns: signalfd failed: %s\n", strerror(errno));
        _exit(1);
    }

    /* --- Step 5: Signal readiness to Haskell parent --- */

    if (ready_fd >= 0) {
        char ready = 1;
        ssize_t w = write(ready_fd, &ready, 1);
        (void)w;
        close(ready_fd);
    }

    /* --- Step 6: Wait loop — reap children on SIGCHLD, exit on SIGTERM --- */

    struct pollfd pfd;
    pfd.fd = sigfd;
    pfd.events = POLLIN;

    while (poll(&pfd, 1, -1) >= 0) {
        if (pfd.revents & POLLIN) {
            struct signalfd_siginfo si;
            ssize_t r = read(sigfd, &si, sizeof(si));
            if (r != (ssize_t)sizeof(si))
                break;

            if (si.ssi_signo == SIGCHLD) {
                /* Reap all terminated children */
                while (waitpid(-1, NULL, WNOHANG) > 0)
                    ;
            } else if (si.ssi_signo == SIGTERM) {
                break;
            }
        }
    }

    close(sigfd);
    _exit(0);
}

/* --------------------------------------------------------------------------
 * Main entry point — called from Haskell
 * -------------------------------------------------------------------------- */

int corvus_netns_start(pid_t *out_pid)
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
         * cannot deliver signals with SIG_DFL disposition. */
        signal(SIGTERM, SIG_IGN);
        signal(SIGINT, SIG_IGN);
        signal(SIGCHLD, SIG_DFL);
        close(ready_pipe[0]);
        namespace_child(ready_pipe[1]);
        _exit(1);
    }

    /* Parent: close write end, wait for ready signal from child */
    close(ready_pipe[1]);

    char buf;
    ssize_t n = read(ready_pipe[0], &buf, 1);
    close(ready_pipe[0]);

    if (n != 1) {
        waitpid(child, NULL, 0);
        return -1;
    }

    *out_pid = child;
    return 0;
}

/* --------------------------------------------------------------------------
 * Execute a command inside the namespace with capabilities
 * -------------------------------------------------------------------------- */

static int enter_namespace(pid_t ns_pid)
{
    char path[PATH_MAX];
    int fd;

    /* Enter user namespace first (grants capabilities) */
    snprintf(path, sizeof(path), "/proc/%d/ns/user", ns_pid);
    fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0)
        return -1;
    if (setns(fd, CLONE_NEWUSER) < 0) {
        close(fd);
        return -1;
    }
    close(fd);

    /* Enter network namespace */
    snprintf(path, sizeof(path), "/proc/%d/ns/net", ns_pid);
    fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0)
        return -1;
    if (setns(fd, CLONE_NEWNET) < 0) {
        close(fd);
        return -1;
    }
    close(fd);

    return 0;
}

int corvus_netns_exec(pid_t ns_pid, char *const argv[])
{
    pid_t child = fork();
    if (child < 0)
        return -1;

    if (child == 0) {
        /* Child: enter namespace, set caps, exec */
        signal(SIGTERM, SIG_DFL);
        signal(SIGINT, SIG_DFL);
        signal(SIGCHLD, SIG_DFL);

        if (enter_namespace(ns_pid) < 0) {
            fprintf(stderr, "corvus_netns_exec: enter_namespace failed: %s\n", strerror(errno));
            _exit(126);
        }

        if (set_ambient_caps() < 0) {
            fprintf(stderr, "corvus_netns_exec: set_ambient_caps failed: %s\n", strerror(errno));
            _exit(126);
        }

        execvp(argv[0], argv);
        fprintf(stderr, "corvus_netns_exec: execvp(%s) failed: %s\n", argv[0], strerror(errno));
        _exit(127);
    }

    /* Parent: wait for child */
    int status;
    if (waitpid(child, &status, 0) < 0)
        return -1;

    if (WIFEXITED(status))
        return WEXITSTATUS(status);

    return -1;
}

/* --------------------------------------------------------------------------
 * Spawn a long-running process inside the namespace (don't wait)
 * -------------------------------------------------------------------------- */

int corvus_netns_spawn(pid_t ns_pid, char *const argv[], pid_t *out_pid)
{
    pid_t child = fork();
    if (child < 0)
        return -1;

    if (child == 0) {
        /* Child: enter namespace, set caps, exec */
        signal(SIGTERM, SIG_DFL);
        signal(SIGINT, SIG_DFL);
        signal(SIGCHLD, SIG_DFL);

        if (enter_namespace(ns_pid) < 0) {
            fprintf(stderr, "corvus_netns_spawn: enter_namespace failed: %s\n", strerror(errno));
            _exit(126);
        }

        if (set_ambient_caps() < 0) {
            fprintf(stderr, "corvus_netns_spawn: set_ambient_caps failed: %s\n", strerror(errno));
            _exit(126);
        }

        execvp(argv[0], argv);
        fprintf(stderr, "corvus_netns_spawn: execvp(%s) failed: %s\n", argv[0], strerror(errno));
        _exit(127);
    }

    /* Parent: return child PID without waiting */
    *out_pid = child;
    return 0;
}

/* --------------------------------------------------------------------------
 * Create a TAP device inside the namespace and add it to a bridge.
 * Returns the TAP fd to the caller (works across namespace boundaries).
 *
 * Uses a socketpair to pass the fd from a child (in the namespace)
 * back to the parent (in the host namespace) via SCM_RIGHTS.
 * -------------------------------------------------------------------------- */

static int send_fd(int sock, int fd)
{
    char buf = 0;
    struct iovec iov = { .iov_base = &buf, .iov_len = 1 };
    char cmsg_buf[CMSG_SPACE(sizeof(int))];
    struct msghdr msg = {
        .msg_iov = &iov,
        .msg_iovlen = 1,
        .msg_control = cmsg_buf,
        .msg_controllen = sizeof(cmsg_buf),
    };
    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    cmsg->cmsg_level = SOL_SOCKET;
    cmsg->cmsg_type = SCM_RIGHTS;
    cmsg->cmsg_len = CMSG_LEN(sizeof(int));
    memcpy(CMSG_DATA(cmsg), &fd, sizeof(int));
    return sendmsg(sock, &msg, 0) < 0 ? -1 : 0;
}

static int recv_fd(int sock)
{
    char buf;
    struct iovec iov = { .iov_base = &buf, .iov_len = 1 };
    char cmsg_buf[CMSG_SPACE(sizeof(int))];
    struct msghdr msg = {
        .msg_iov = &iov,
        .msg_iovlen = 1,
        .msg_control = cmsg_buf,
        .msg_controllen = sizeof(cmsg_buf),
    };
    if (recvmsg(sock, &msg, 0) < 0)
        return -1;
    struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
    if (cmsg == NULL || cmsg->cmsg_level != SOL_SOCKET || cmsg->cmsg_type != SCM_RIGHTS)
        return -1;
    int fd;
    memcpy(&fd, CMSG_DATA(cmsg), sizeof(int));
    return fd;
}

int corvus_netns_create_tap(pid_t ns_pid, const char *bridge_name, int *out_fd)
{
    int sv[2];
    if (socketpair(AF_UNIX, SOCK_STREAM | SOCK_CLOEXEC, 0, sv) < 0)
        return -1;

    pid_t child = fork();
    if (child < 0) {
        close(sv[0]);
        close(sv[1]);
        return -1;
    }

    if (child == 0) {
        /* Child: enter namespace, create TAP, add to bridge, send fd back */
        close(sv[0]);
        signal(SIGTERM, SIG_DFL);
        signal(SIGINT, SIG_DFL);

        if (enter_namespace(ns_pid) < 0) {
            fprintf(stderr, "corvus_netns_create_tap: enter_namespace failed: %s\n", strerror(errno));
            _exit(1);
        }

        if (set_ambient_caps() < 0) {
            fprintf(stderr, "corvus_netns_create_tap: set_ambient_caps failed: %s\n", strerror(errno));
            _exit(1);
        }

        /* Create TAP device (auto-named) */
        int tapfd = open("/dev/net/tun", O_RDWR);
        if (tapfd < 0) {
            fprintf(stderr, "corvus_netns_create_tap: open /dev/net/tun failed: %s\n", strerror(errno));
            _exit(1);
        }

        struct ifreq ifr;
        memset(&ifr, 0, sizeof(ifr));
        ifr.ifr_flags = IFF_TAP | IFF_NO_PI;
        /* Leave ifr_name empty for auto-naming (tap0, tap1, ...) */

        if (ioctl(tapfd, TUNSETIFF, &ifr) < 0) {
            fprintf(stderr, "corvus_netns_create_tap: TUNSETIFF failed: %s\n", strerror(errno));
            close(tapfd);
            _exit(1);
        }

        /* Add TAP to bridge */
        char cmd[256];
        snprintf(cmd, sizeof(cmd), "ip link set %s master %s", ifr.ifr_name, bridge_name);
        if (system(cmd) != 0) {
            fprintf(stderr, "corvus_netns_create_tap: failed to add TAP to bridge\n");
            close(tapfd);
            _exit(1);
        }

        /* Bring TAP up */
        snprintf(cmd, sizeof(cmd), "ip link set %s up", ifr.ifr_name);
        if (system(cmd) != 0) {
            fprintf(stderr, "corvus_netns_create_tap: failed to bring TAP up\n");
            close(tapfd);
            _exit(1);
        }

        /* Clear CLOEXEC on tapfd so it can be sent */
        int flags = fcntl(tapfd, F_GETFD);
        fcntl(tapfd, F_SETFD, flags & ~FD_CLOEXEC);

        /* Send tapfd to parent */
        if (send_fd(sv[1], tapfd) < 0) {
            fprintf(stderr, "corvus_netns_create_tap: send_fd failed: %s\n", strerror(errno));
            close(tapfd);
            _exit(1);
        }

        close(tapfd);
        close(sv[1]);
        _exit(0);
    }

    /* Parent: receive TAP fd from child */
    close(sv[1]);

    int tapfd = recv_fd(sv[0]);
    close(sv[0]);

    /* Wait for child to exit */
    int status;
    waitpid(child, &status, 0);

    if (tapfd < 0 || !WIFEXITED(status) || WEXITSTATUS(status) != 0) {
        if (tapfd >= 0)
            close(tapfd);
        return -1;
    }

    *out_fd = tapfd;
    return 0;
}
