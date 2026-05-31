@0xd6cec0faa39e02dc;

using Common = import "common.capnp";
using Enums = import "enums.capnp";
using Vm = import "vm.capnp";
using CloudInit = import "cloudinit.capnp";

# ---------------------------------------------------------------------
# Info structs
# ---------------------------------------------------------------------

struct TemplateVmInfo {
  id          @0 :Int64;
  name        @1 :Text;
  cpuCount    @2 :Int32;
  ramMb       @3 :Int32;
  description @4 :Text;   # empty == none
  headless    @5 :Bool;
  guestAgent  @6 :Bool;
  autostart   @7 :Bool;
  rebootQuirk @8 :Bool;
}

struct TemplateDriveInfo {
  # Source disk image. `id == 0` => not bound to a specific image
  # (e.g. clone/overlay strategies that create a new disk).
  diskImage      @0  :Common.NamedRef;
  interface      @1  :Enums.DriveInterface;
  hasMedia       @2  :Bool;
  media          @3  :Enums.DriveMedia;
  readOnly       @4  :Bool;
  cacheType      @5  :Enums.CacheType;
  discard        @6  :Bool;
  cloneStrategy  @7  :Enums.TemplateCloneStrategy;
  sizeMb         @8  :Int64;   # 0 == not specified
  hasFormat      @9  :Bool;
  format         @10 :Enums.DriveFormat;
  # Per-drive override for the ephemeral flag on disks created during
  # instantiation. `hasEphemeral` distinguishes "user did not specify"
  # (use strategy-driven default) from "user explicitly set to false".
  hasEphemeral   @11 :Bool;
  ephemeral      @12 :Bool;
}

struct TemplateNetIfInfo {
  type       @0 :Enums.NetInterfaceType;
  hostDevice @1 :Text;   # empty == auto
  # Managed-network NICs reference the network by name. Resolved
  # to a NetworkId at template-instantiation time. Empty for
  # non-managed types.
  network    @2 :Text;
}

struct TemplateSshKeyInfo {
  id   @0 :Int64;
  name @1 :Text;
}

struct TemplateDetails {
  id              @0  :Int64;
  name            @1  :Text;
  cpuCount        @2  :Int32;
  ramMb           @3  :Int32;
  description     @4  :Text;
  headless        @5  :Bool;
  cloudInit       @6  :Bool;
  guestAgent      @7  :Bool;
  autostart       @8  :Bool;
  cloudInitConfig @9  :CloudInit.CloudInitInfo;
  createdAt       @10 :Int64;
  drives          @11 :List(TemplateDriveInfo);
  netIfs          @12 :List(TemplateNetIfInfo);
  sshKeys         @13 :List(TemplateSshKeyInfo);
  rebootQuirk     @14 :Bool;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface TemplateManager {
  list   @0 () -> (templates :List(TemplateVmInfo));
  get    @1 (ref :Common.EntityRef) -> (template :Template);
  create @2 (yaml :Text) -> (template :Template);
}

interface Template {
  show         @0 () -> (details :TemplateDetails);
  delete       @1 () -> ();
  instantiate  @2 (name :Text, node :Common.EntityRef) -> (vm :Vm.Vm);
  update       @3 (yaml :Text) -> ();
}
