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
}

struct TemplateDriveInfo {
  diskImageId    @0  :Int64;   # 0 == not bound to a specific image
  diskImageName  @1  :Text;    # empty when no specific image
  interface      @2  :Enums.DriveInterface;
  hasMedia       @3  :Bool;
  media          @4  :Enums.DriveMedia;
  readOnly       @5  :Bool;
  cacheType      @6  :Enums.CacheType;
  discard        @7  :Bool;
  cloneStrategy  @8  :Enums.TemplateCloneStrategy;
  sizeMb         @9  :Int64;   # 0 == not specified
  hasFormat      @10 :Bool;
  format         @11 :Enums.DriveFormat;
}

struct TemplateNetIfInfo {
  type       @0 :Enums.NetInterfaceType;
  hostDevice @1 :Text;   # empty == auto
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
  instantiate  @2 (name :Text) -> (vm :Vm.Vm);
  update       @3 (yaml :Text) -> ();
}
