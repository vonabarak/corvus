@0xbc08f7ad74ba3f78;

# Cap'n Proto wire schema for `corvus-nodeagent`, the per-host
# privileged agent that owns local-machine state: QEMU + virtiofsd
# process supervision, qemu-img and cloud-init ISO generation, the
# console ring buffer, and the guest-agent poller.
#
# Phase 1 ships the bootstrap surface only — `ping`, `version`,
# `session`. Disk / VM / console methods land in later phases.
#
# Symmetric with `netagent.capnp`: stateless agent, declarative
# `applyX` / `deleteX` / `listX` RPC, daemon-side reconnect-and-
# re-apply. The two agents are fully independent of each other;
# the daemon dials both at startup.
#
# Naming convention: every agent-managed resource on the host has a
# fixed prefix the agent uses for startup / shutdown cleanup:
#
#   * QEMU processes:     `corvus-qemu/*` (argv match)
#   * virtiofsd processes: `virtiofsd ... --shared-dir=*corvus*`
#   * runtime sockets:    `$XDG_RUNTIME_DIR/corvus/<vm>/`
#
# Authentication: out of scope for v1. The agent binds to
# 127.0.0.1:9878 and trusts every connection.

# ---------------------------------------------------------------------
# Bootstrap interface
# ---------------------------------------------------------------------

interface NodeAgent {
  # Open a new session. `owner` is a free-form tag (typically the
  # caller's numeric uid as a string); reserved for future per-owner
  # ledger partitioning. Not authenticated.
  session @0 (owner :Text) -> (session :Session);

  # Liveness + capability probe.
  ping    @1 () -> ();
  version @2 () -> (info :AgentInfo);
}

struct AgentInfo {
  semver       @0 :Text;       # "0.1.0"
  capabilities @1 :List(Text); # phase-by-phase: "disk", "vm", "console", ...
}

# ---------------------------------------------------------------------
# Session — scoped to one owner tag.
#
# Phase 1: liveness only. Later phases extend with:
#   * applyDisk / deleteDisk / snapshotDisk / rebaseDisk / importDisk
#   * applyCloudInit / deleteCloudInit
#   * applyVm / deleteVm / listVms / getVm / controlVm
#   * setSpiceTicket / guestExec
#   * openSerialConsole / openHmpMonitor
#   * subscribeStatus
# ---------------------------------------------------------------------

interface Session {
  # End-to-end check that the session is wired up. Phase 1 calls this
  # from the daemon after `session(owner)` succeeds, to prove the
  # session cap is live before declaring the connection healthy.
  ping @0 () -> ();
}
