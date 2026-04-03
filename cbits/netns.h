#ifndef CORVUS_NETNS_H
#define CORVUS_NETNS_H

#include <sys/types.h>

/*
 * corvus_netns_start: Fork a child process that creates an unprivileged
 * user+network+UTS namespace and keeps it alive.
 *
 * The function forks internally (no forkProcess needed from Haskell).
 * The child process is single-threaded and safe for unshare(CLONE_NEWUSER).
 *
 * The namespace stays alive as long as the child process is running.
 * Send SIGTERM to the child PID to tear down the namespace.
 *
 * Parameters:
 *   out_pid - Output: PID of the namespace manager child process
 *
 * Returns: 0 on success (child is running, PID written to *out_pid).
 *         -1 on error.
 */
int corvus_netns_start(pid_t *out_pid);

/*
 * corvus_netns_exec: Execute a command inside the namespace with
 * network capabilities (CAP_NET_ADMIN, etc.).
 *
 * Forks a child that enters the namespace via setns(), sets ambient
 * capabilities so they survive exec(), then execvp() the command.
 * The parent waits for the child and returns its exit status.
 *
 * Parameters:
 *   ns_pid  - PID of the namespace manager (for /proc/<pid>/ns/*)
 *   argv    - NULL-terminated argument array (argv[0] is the binary)
 *
 * Returns: exit status of the command (0 on success), or -1 on error.
 */
int corvus_netns_exec(pid_t ns_pid, char *const argv[]);

/*
 * corvus_netns_spawn: Like corvus_netns_exec, but does not wait for
 * the child. Returns the child PID via out_pid.
 *
 * Used for long-running processes (dnsmasq, QEMU) that need to run
 * inside the namespace with capabilities.
 *
 * Parameters:
 *   ns_pid  - PID of the namespace manager (for /proc/<pid>/ns/*)
 *   argv    - NULL-terminated argument array (argv[0] is the binary)
 *   out_pid - Output: PID of the spawned child
 *
 * Returns: 0 on success, -1 on error.
 */
int corvus_netns_spawn(pid_t ns_pid, char *const argv[], pid_t *out_pid);

/*
 * corvus_netns_create_tap: Create a TAP device inside the namespace
 * and add it to a bridge. Returns the TAP file descriptor.
 *
 * The TAP fd works across namespace boundaries — QEMU can use it
 * from the host namespace with -netdev tap,fd=<N>.
 *
 * Parameters:
 *   ns_pid      - PID of the namespace manager
 *   bridge_name - Name of the bridge to add the TAP to
 *   out_fd      - Output: TAP file descriptor (caller must close)
 *
 * Returns: 0 on success, -1 on error.
 */
int corvus_netns_create_tap(pid_t ns_pid, const char *bridge_name, int *out_fd);

#endif /* CORVUS_NETNS_H */
