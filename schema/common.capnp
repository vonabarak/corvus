@0x9b1373e2334a09e9;

# Leaf types shared across the Corvus schema. This file imports
# nothing, which lets every subsystem file (vm.capnp, disk.capnp,
# network.capnp, ...) import it without creating a cycle through
# corvus.capnp (which itself imports every subsystem for the
# Daemon's manager methods).

# Reference to an entity by either numeric id or symbolic name.
# Used as an RPC INPUT — the caller picks the branch. The schema-level
# union enforces "exactly one" so handlers don't have to reconcile both.
struct EntityRef {
  union {
    id   @0 :Int64;
    name @1 :Text;
  }
}

# An entity's id paired with its display name. Used as an RPC OUTPUT
# wherever one entity references another (a drive's disk image, a VM's
# node, a network interface's network, etc.) — both halves travel
# together so the JSON / WebUI can render a link + label without a
# second round-trip.
#
# Optional references encode "absent" with the sentinel `id == 0`;
# language-level converters (Haskell, Python, TypeScript) lift that
# to a proper Maybe / None / null at the boundary.
#
# See AGENTS.md `## Project Rules / Cross-entity references` for the
# convention.
struct NamedRef {
  id   @0 :Int64;
  name @1 :Text;
}

# Daemon-wide status.
struct StatusInfo {
  uptimeSeconds   @0 :Int64;
  connections     @1 :Int32;
  version         @2 :Text;
  protocolVersion @3 :UInt32;
  databaseBackend @4 :Text;
  databaseVersion @5 :Text;
}

# Returned by `Vm.viewGrant` — short-lived SPICE access credentials.
struct ViewGrant {
  host       @0 :Text;
  port       @1 :Int32;
  password   @2 :Text;
  ttlSeconds @3 :Int32;
}
