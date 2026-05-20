# Multi-node Corvus

Corvus runs as one unprivileged daemon (`corvus`) talking to N
privileged agent pairs — one `corvus-nodeagent` + one
`corvus-netd` per host. Each host the daemon orchestrates is
recorded in the database as a **Node** row; every VM, network,
and disk-image placement carries a `node_id` FK pointing at a
specific node.

This page covers:

  * The `crv node …` CRUD surface.
  * The scheduler that picks a node when `--node` is omitted.
  * The same-node invariant for disk-attach / netif-add.
  * The per-node reconnect-loop topology, plus how to recover
    from a temporary nodeagent / netd outage.
  * The per-node VSOCK CID and SPICE port allocator scoping.

The default after a fresh install is single-node: register one
node and never pass `--node` again. The CLI / Python client
both treat `--node` / `node=` as optional everywhere; the
scheduler picks the one available node.

## Node CRUD

### `crv node add`

```
crv node add <NAME> --host <IP> [--node-agent-port 9878] \
                                [--net-agent-port 9877] \
                                [--base-path /home/corvus/VMs] \
                                [--description TEXT] \
                                [--admin-state online|draining|maintenance]
```

  * **NAME** — short identifier (unique across the cluster).
    Used everywhere the operator types `--node <NAME>`.
  * **--host** — IPv4 / IPv6 / hostname the daemon will dial.
    Defaults assume the daemon and the agents share `127.0.0.1`;
    for a remote node, this is the address of that node's
    nodeagent + netd listeners.
  * **--node-agent-port / --net-agent-port** — TCP ports the
    agents are listening on. Defaults match the upstream
    systemd units.
  * **--base-path** — daemon basePath on the node (where VM
    images live). Disk file paths the daemon records in the
    `disk_image_node` join are resolved against this base.
  * **--admin-state** — `online` (default), `maintenance`
    (refuse new VM placements), or `draining` (same as
    `maintenance`, but also a strong signal that ops is about
    to drain workloads off the node).

The daemon spawns the per-node reconnect supervisor as soon as
the row insert succeeds — the new node is live without a
daemon restart.

### `crv node list`

```
ID  NAME      HOST           STATE   CPUS  RAM_FREE  DISK_FREE_GB  LOAD1
1   alpha     192.168.1.10   online    8     14336           400   0.42
2   beta      192.168.1.11   online   16     30720           800   0.18
```

Capacity columns show `--` until the node's nodeagent has
pushed its first `NodeStats` snapshot (~10 s after first
connect).

### `crv node show <NODE>`

Full record including `description`, `basePath`, kernel
release, agent version, and per-agent healthcheck timestamps.

### `crv node edit <NODE>`

```
crv node edit alpha --host 192.168.1.20
crv node edit alpha --admin-state draining
crv node edit alpha --description "decom'd 2026-08-01"
```

Changing `--host`, `--node-agent-port`, or `--net-agent-port`
triggers a daemon-side supervisor respawn against the new
endpoint — no daemon restart needed. Other fields leave the
existing connection alone.

### `crv node drain <NODE>`

Shortcut for `crv node edit <NODE> --admin-state draining`.
The scheduler skips draining nodes when picking placement,
but existing VMs / networks / disks on the node keep running.

### `crv node delete <NODE>`

Refuses while any VM, network, or `disk_image_node` row still
references the node. Move the workloads off (`crv vm
delete --delete-disks` for one-shot cleanup; `crv vm edit
--node <NEW>` is a future ergonomic) before retrying.

## Scheduler

When you omit `--node` on `crv vm create`, `crv network
create`, or `crv template instantiate`, the daemon's scheduler
picks one. Same for `node:` on apply YAML.

Algorithm:

  1. Filter to nodes with `admin_state = online`.
  2. For VMs: require `ram_mb_free >= request.ram_mb + 512`
     (the safety margin matches `Corvus.Handlers.Scheduler.ramSafetyMb`).
     Stats are `NULL` until the first agent push, so freshly
     added nodes always pass this filter.
  3. Score by `(ram_mb_free - request.ram_mb) +
     (storage_bytes_free / 1 GiB) - 100 * load_avg1`. The
     load penalty is small so equally-sized nodes break ties
     on a quiescent host.
  4. Tie-break by node name (stable, predictable in tests).
  5. Subtract any in-memory **reservation** for that node
     before scoring. The scheduler bumps a per-node
     reservation as soon as it picks a node; the reservation
     clears when the agent's next stats push arrives. This
     stops back-to-back creates within the same daemon from
     piling onto the same node before the agent's reported
     `ram_mb_free` reflects the new VM.

For disks and networks the algorithm is simpler: pick the
lowest-id online node. The disk case is a Phase-3 placeholder —
a richer disk-aware policy may land later (e.g. prefer nodes
that already host the backing image).

## Same-node invariants

Two daemon-side checks enforce the rule that resources qemu
needs to open simultaneously must live on the same kernel:

  * **Disk attach** (`crv vm attach-disk` / drive entries in
    apply YAML). The disk's `disk_image_node` row for the VM's
    node must exist. Without it qemu on the VM's host has no
    file to open. The daemon refuses the attach with a clear
    error naming both sides — the operator's fix is to rsync
    the image over and run `crv disk register --node <vm-node>
    <name> <path>`.
  * **Network interface add** with a managed network
    (`network: <name>` on a netif). The network's node and the
    VM's node must match. A managed-NIC TAP can only join a
    bridge that lives on the same kernel as qemu.

## Per-node connections

Each Node row gets one supervisor 'Async' that holds open both
its `corvus-nodeagent` and `corvus-netd` connections. On
connect / reconnect the supervisor:

  * Subscribes to the nodeagent's VM-status push stream
    (`subscribeVmStatus`), so the daemon picks up VM state
    transitions and `NodeStats` once per tick.
  * Re-applies every running network row whose `node_id`
    matches this node (the netd agent is stateless).
  * Re-attaches monitor threads for every VM the agent already
    runs (so a daemon restart doesn't lose track of QEMU exits
    for VMs that survived the disconnect).

If a node's nodeagent / netd is unreachable, calls that need
that agent return `nodeagent for node X unavailable` / `netd
for node X unavailable`. Calls scoped to other nodes are
unaffected.

## Per-node allocators

  * **VSOCK CID**. The daemon asks the target node's agent
    (`Session.probeVsockCid`) whether a candidate CID is free
    on that node's kernel; the daemon doesn't open
    `/dev/vhost-vsock` itself. CIDs are unique per kernel, not
    cluster-wide, so two VMs on different nodes may legally
    share a CID.
  * **SPICE port**. Per-node SPICE allocation against a per-node
    free-port range. Two VMs on different nodes can listen on
    the same port; clients route through the per-node
    `--spice-bind` address.

## Wire surface

Cap'n Proto schema additions for multi-node:

  * `schema/node.capnp` — `Node` resource + `NodeManager`
    interface; `NodeInfo` / `NodeDetails` DTOs.
  * `node :Common.EntityRef` on `VmCreateParams`,
    `NetworkCreateParams`, `Template.instantiate`. Optional
    (empty / unset = "let the scheduler choose").
  * `schema/disk.capnp` `DiskImageInfo.placements
    :List(DiskImagePlacement)` — replaces the legacy single
    `filePath` field. Each placement carries `nodeId`,
    `nodeName`, `filePath`.
  * `schema/nodeagent.capnp` `VmStatusSnapshot.nodeStats
    :NodeStats` — CPU / RAM / storage / load / kernel /
    version, pushed every 10 s.
  * `schema/nodeagent.capnp` `Session.probeVsockCid` — per-node
    kernel-uniqueness probe replacing the daemon's old local
    `/dev/vhost-vsock` ioctl.

## Operational tips

  * **Recovering from "no online node available"**. A fresh
    daemon DB has zero Node rows; every create RPC will fail
    until you run `crv node add`. The integration-test harness
    bootstraps a self-pointing node automatically; manual
    installs need at least one `crv node add` before the first
    `crv vm create`.

  * **Recovering from "node X unavailable"**. Either the
    nodeagent / netd on node X is down, or the network between
    daemon and node X is broken. The supervisor retries every
    5 s; `crv node show <X>` displays the last successful push
    timestamp so you can tell how stale the registry view is.

  * **Moving a node**. Use `crv node edit <NAME> --host
    <NEW-IP>` (and / or `--node-agent-port` / `--net-agent-port`).
    The daemon cancels the existing supervisor and respawns
    against the new endpoint; in-flight RPCs see "node X
    unavailable" for the few seconds it takes to reconnect.

  * **Per-node maintenance**. `crv node drain <NAME>` marks
    the node as draining so the scheduler skips it. Existing
    VMs keep running until you explicitly stop / delete them
    or move them off (the latter is a future ergonomic —
    Phase-3+).

## Limitations (current Phase 1 shape)

  * **Auth / TLS** on daemon↔agent links is **not yet in
    place**. Agents bind 127.0.0.1; cross-host nodes assume a
    trusted L2 or a WireGuard / SSH tunnel between the daemon
    host and each node. A certificate-based auth scheme is on
    the roadmap.

  * **No live migration** of a running VM between nodes.
    Stop, delete-without-`--delete-disks`, rsync the disk,
    re-register on the new node, create a fresh VM, attach.

  * **No automatic disk replication**. The
    `disk_image_node` rows are populated as the operator
    explicitly registers / creates images per node. A `crv
    disk replicate <name> --to <node>` is a Phase-7 candidate.

  * **Per-node networks only**. Networks have a `node_id`
    FK — overlays / cross-node VXLAN are out of scope.

  * **No quotas** beyond the basic free-RAM / free-disk filter
    in the scheduler. `admin_state = draining` is the only
    coarse knob.

  * **Heterogeneous agent versions**. `Node.agent_version` is
    recorded but the daemon doesn't yet refuse a mismatched
    agent.
