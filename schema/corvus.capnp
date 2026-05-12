@0xc6b684f81a734396;

# Top-level Cap'n Proto schema for the Corvus daemon RPC.
#
# The `Daemon` interface is the bootstrap capability — every client
# session starts by retrieving it. From there, subsystem managers are
# obtained via member methods (e.g. `vms()` returns a `VmManager`),
# and resource capabilities (`Vm`, `Disk`, ...) are obtained by name
# or id on the managers.
#
# All entity-id arguments use the `EntityRef` union so the client
# states unambiguously whether it's referring by numeric id or
# symbolic name. The CLI translates bare tokens locally (digits → id,
# otherwise → name); library users select the branch explicitly.

using Vm = import "vm.capnp";
using Disk = import "disk.capnp";
using Network = import "network.capnp";
using SshKey = import "sshkey.capnp";
using Template = import "template.capnp";
using Task = import "task.capnp";
using CloudInit = import "cloudinit.capnp";
using Streams = import "streams.capnp";

# ---------------------------------------------------------------------
# Common types
# ---------------------------------------------------------------------

# Reference to an entity by either numeric id or symbolic name.
# Exactly one branch is set; the union enforces it at the schema level.
struct EntityRef {
  union {
    id   @0 :Int64;
    name @1 :Text;
  }
}

# Daemon-wide status.
struct StatusInfo {
  uptimeSeconds   @0 :Int64;
  connections     @1 :Int32;
  version         @2 :Text;
  protocolVersion @3 :UInt32;
  namespacePid    @4 :Int64;  # 0 when no shared namespace is active
}

# Returned by `Vm.viewGrant` — short-lived SPICE access credentials.
struct ViewGrant {
  host       @0 :Text;
  port       @1 :Int32;
  password   @2 :Text;
  ttlSeconds @3 :Int32;
}

# ---------------------------------------------------------------------
# Daemon bootstrap interface
# ---------------------------------------------------------------------

interface Daemon {
  ping     @0 () -> ();
  status   @1 () -> (info :StatusInfo);
  shutdown @2 () -> ();

  vms       @3 () -> (mgr :Vm.VmManager);
  disks     @4 () -> (mgr :Disk.DiskManager);
  networks  @5 () -> (mgr :Network.NetworkManager);
  sshKeys   @6 () -> (mgr :SshKey.SshKeyManager);
  templates @7 () -> (mgr :Template.TemplateManager);
  tasks     @8 () -> (mgr :Task.TaskManager);
  cloudInit @9 () -> (mgr :CloudInit.CloudInitManager);

  # Declarative environment application. `wait` returns once the
  # synchronous portion of apply is done; long-running steps are
  # tracked via the returned task id.
  apply @10 (yaml :Text, skipExisting :Bool, wait :Bool)
           -> (result :ApplyResult, taskId :Int64);

  # Build pipeline. The client passes a `BuildEventSink`; the daemon
  # pushes events as the build progresses.
  build @11 (yaml :Text, sink :Streams.BuildEventSink) -> (taskId :Int64);
}

# ---------------------------------------------------------------------
# Apply result
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
