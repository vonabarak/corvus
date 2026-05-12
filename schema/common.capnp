@0x9b1373e2334a09e9;

# Leaf types shared across the Corvus schema. This file imports
# nothing, which lets every subsystem file (vm.capnp, disk.capnp,
# network.capnp, ...) import it without creating a cycle through
# corvus.capnp (which itself imports every subsystem for the
# Daemon's manager methods).

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
