@0xa6341bd086aa89f6;

using Corvus = import "corvus.capnp";

# ---------------------------------------------------------------------
# Info structs
# ---------------------------------------------------------------------

struct SshKeyInfo {
  id            @0 :Int64;
  name          @1 :Text;
  publicKey     @2 :Text;
  createdAt     @3 :Int64;   # POSIX nanoseconds
  attachedVms   @4 :List(VmAttachment);
}

struct VmAttachment {
  vmId   @0 :Int64;
  vmName @1 :Text;
}

# ---------------------------------------------------------------------
# Parameter structs
# ---------------------------------------------------------------------

struct SshKeyCreateParams {
  name      @0 :Text;
  publicKey @1 :Text;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface SshKeyManager {
  list   @0 () -> (keys :List(SshKeyInfo));
  get    @1 (ref :Corvus.EntityRef) -> (key :SshKey);
  create @2 (params :SshKeyCreateParams) -> (key :SshKey);
}

interface SshKey {
  show   @0 () -> (info :SshKeyInfo);
  delete @1 () -> ();
}
