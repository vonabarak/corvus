@0xa40f88677f695703;

# Node-management surface. A `Node` row models one host the daemon
# orchestrates: agent endpoints (corvus-nodeagent and corvus-netd
# ports), administrative state, and the latest agent-pushed
# health/capacity snapshot (RAM / CPU / disk / load).
#
# Multi-node Phase 1 introduces only the data shape + the read
# surface. The scheduler (Phase 2), per-resource node FKs in
# disk/network attach paths (Phase 3-4), and the `NodeStats`
# push from the agent (Phase 5) build on top.

using Common = import "common.capnp";
using Enums = import "enums.capnp";

# ---------------------------------------------------------------------
# Info structs
# ---------------------------------------------------------------------

# Short summary used in the `list` cap and rendered as table rows by
# `crv node list`. Live counters (RAM/disk/load) are surfaced as 0
# when the agent hasn't pushed yet — clients render that as `--`.
struct NodeInfo {
  id                   @0  :Int64;
  name                 @1  :Text;
  host                 @2  :Text;
  nodeAgentPort        @3  :Int32;
  netAgentPort         @4  :Int32;
  adminState           @5  :Enums.NodeAdminState;
  createdAt            @6  :Int64;  # POSIX nanoseconds
  # Latest agent-pushed counters (0 == never pushed).
  cpuCount             @7  :Int32;
  ramMbTotal           @8  :Int32;
  ramMbFree            @9  :Int32;
  storageBytesTotal    @10 :Int64;
  storageBytesFree     @11 :Int64;
  loadAvg1             @12 :Float64;
  lastNodeAgentPushAt  @13 :Int64;  # POSIX nanoseconds; 0 == never
  lastNetAgentPushAt   @14 :Int64;
}

# Full per-node detail surfaced by `crv node show`.
struct NodeDetails {
  id                   @0  :Int64;
  name                 @1  :Text;
  host                 @2  :Text;
  nodeAgentPort        @3  :Int32;
  netAgentPort         @4  :Int32;
  basePath             @5  :Text;
  description          @6  :Text;   # empty == none
  adminState           @7  :Enums.NodeAdminState;
  createdAt            @8  :Int64;
  cpuCount             @9  :Int32;
  ramMbTotal           @10 :Int32;
  ramMbFree            @11 :Int32;
  storageBytesTotal    @12 :Int64;
  storageBytesFree     @13 :Int64;
  loadAvg1             @14 :Float64;
  loadAvg5             @15 :Float64;
  loadAvg15            @16 :Float64;
  kernelRelease        @17 :Text;   # empty == none
  agentVersion         @18 :Text;   # empty == none
  lastNodeAgentPushAt  @19 :Int64;
  lastNetAgentPushAt   @20 :Int64;
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

# Inline-param defaults mirror `crv node add`. Only `name` and
# `host` are mandatory; ports default to the netd/nodeagent
# convention (9877/9878) and basePath to `/home/corvus/VMs`
# (matching the existing service unit's working dir).
struct NodeAddParams {
  name           @0 :Text;
  host           @1 :Text;
  nodeAgentPort  @2 :Int32 = 9878;
  netAgentPort   @3 :Int32 = 9877;
  basePath       @4 :Text  = "/home/corvus/VMs";
  description    @5 :Text;          # empty == none
  adminState     @6 :Enums.NodeAdminState = online;
}

# Optional-by-presence-flag edits, mirroring the existing
# 'VmEditParams' / 'NetworkEditParams' pattern.
struct NodeEditParams {
  hasName          @0  :Bool;
  name             @1  :Text;
  hasHost          @2  :Bool;
  host             @3  :Text;
  hasNodeAgentPort @4  :Bool;
  nodeAgentPort    @5  :Int32;
  hasNetAgentPort  @6  :Bool;
  netAgentPort     @7  :Int32;
  hasBasePath      @8  :Bool;
  basePath         @9  :Text;
  hasDescription   @10 :Bool;
  description      @11 :Text;
  hasAdminState    @12 :Bool;
  adminState       @13 :Enums.NodeAdminState;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface NodeManager {
  list    @0 () -> (nodes :List(NodeInfo));
  get     @1 (ref :Common.EntityRef) -> (node :Node);
  create  @2 (params :NodeAddParams) -> (node :Node);
}

interface Node {
  show   @0 () -> (details :NodeDetails);
  edit   @1 (params :NodeEditParams) -> ();
  # Operator shortcut for `edit { adminState = draining }`; the
  # scheduler skips draining nodes when picking placement.
  drain  @2 () -> ();
  # Refuses while any VMs / disks / networks reference this node.
  # Cascade-with-confirm is a Phase-2 ergonomic.
  delete @3 () -> ();
}
