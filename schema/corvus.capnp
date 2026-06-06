@0xc6b684f81a734396;

# Top-level Cap'n Proto schema for the Corvus daemon RPC.
#
# The `Daemon` interface is the bootstrap capability — every client
# session starts by retrieving it. From there, subsystem managers are
# obtained via member methods (e.g. `vms()` returns a `VmManager`),
# and resource capabilities (`Vm`, `Disk`, ...) are obtained by name
# or id on the managers.
#
# Leaf types shared with the subsystem schemas (`EntityRef`,
# `StatusInfo`, `ViewGrant`) live in common.capnp so that subsystem
# files don't have to import corvus.capnp — which would create a
# module cycle with corvus.capnp's imports of each subsystem.

using Common = import "common.capnp";
using Vm = import "vm.capnp";
using Disk = import "disk.capnp";
using Network = import "network.capnp";
using Node = import "node.capnp";
using SshKey = import "sshkey.capnp";
using Template = import "template.capnp";
using Task = import "task.capnp";
using CloudInit = import "cloudinit.capnp";
using Streams = import "streams.capnp";

# ---------------------------------------------------------------------
# Daemon bootstrap interface
# ---------------------------------------------------------------------

interface Daemon {
  ping     @0 () -> ();
  status   @1 () -> (info :Common.StatusInfo);
  shutdown @2 () -> ();

  vms       @3 () -> (mgr :Vm.VmManager);
  disks     @4 () -> (mgr :Disk.DiskManager);
  networks  @5 () -> (mgr :Network.NetworkManager);
  sshKeys   @6 () -> (mgr :SshKey.SshKeyManager);
  templates @7 () -> (mgr :Template.TemplateManager);
  tasks     @8 () -> (mgr :Task.TaskManager);
  cloudInit @9 () -> (mgr :CloudInit.CloudInitManager);
  nodes     @12 () -> (mgr :Node.NodeManager);

  # Declarative environment application.
  #
  # When `sink` is supplied (non-null), the daemon streams progress
  # through it (phase boundaries, per-entity start/end, download
  # bytes), returns `taskId` immediately, and `result` is empty —
  # the caller learns the outcome from `applyEnd` on the sink
  # followed by `end()`. The `wait` flag is ignored in this mode.
  #
  # When `sink` is null, the legacy behaviour applies: `wait=true`
  # blocks until completion and returns a populated `result`;
  # `wait=false` kicks off an async task and returns `taskId` with
  # an empty `result`. `skipExisting` skips entities whose names
  # already exist instead of erroring.
  apply @10 (yaml         :Text,
             skipExisting :Bool                     = false,
             wait         :Bool                     = false,
             sink         :Streams.ApplyEventSink)
           -> (result :ApplyResult, taskId :Int64);

  # Build pipeline. The client passes a `BuildEventSink`; the daemon
  # pushes events as the build progresses.
  #
  # `useCache` and `buildCache` are independent flags. With neither
  # set the daemon behaves as it did before the cache feature shipped:
  # every step runs from scratch and no cache rows are written. With
  # `useCache` set, the daemon reuses any cached bake VM whose chain
  # matches a prefix of the build's steps. With `buildCache` set, the
  # daemon snapshots the bake VM's writable disks after each successful
  # step and keeps the bake VM alive across builds. `rebuildFrom`
  # (1-based step index) caps the matched prefix at one step before
  # the named step and is only meaningful when `useCache` is also set.
  build @11 (yaml         :Text,
             sink         :Streams.BuildEventSink,
             useCache     :Bool  = false,
             buildCache   :Bool  = false,
             rebuildFrom  :Int32 = 0)
         -> (taskId :Int64);
}

# ---------------------------------------------------------------------
# Apply result (Daemon-specific; not shared)
# ---------------------------------------------------------------------

struct ApplyCreated {
  name @0 :Text;
  id   @1 :Int64;
}

struct ApplyResult {
  sshKeys   @0 :List(ApplyCreated);
  disks     @1 :List(ApplyCreated);
  networks  @2 :List(ApplyCreated);
  vms       @3 :List(ApplyCreated);
  templates @4 :List(ApplyCreated);
}
