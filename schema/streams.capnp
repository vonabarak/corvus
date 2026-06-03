@0x9bd452a518ed3917;

# Streaming sink capabilities used across the Corvus RPC surface.
#
# Note: the Haskell `capnp` library does not implement the `stream`
# keyword's automatic flow control (it treats `-> stream` like
# `-> ()`). All sinks below use plain `-> ()` returns and rely on
# the caller to maintain a small window of in-flight `push` calls
# for backpressure. When the Haskell library gains stream-keyword
# support, the `-> ()` returns can be replaced with `-> stream`
# without any schema-incompatible change to senders or receivers.

using Enums = import "enums.capnp";

# ---------------------------------------------------------------------
# Generic byte-pipe sink
# ---------------------------------------------------------------------

# Used for serial console output, HMP monitor output, and any other
# unstructured byte stream from daemon to client (or, for input,
# client to daemon).
interface ByteSink {
  write @0 (chunk :Data) -> ();
  end   @1 () -> ();
}

# ---------------------------------------------------------------------
# Build pipeline events
# ---------------------------------------------------------------------

struct BuildEvent {
  union {
    logLine @0 :Text;

    stepStart :group {
      stepIndex @1 :Int32;
      name      @2 :Text;
      command   @3 :Text;
    }

    stepOutput :group {
      stepIndex @4 :Int32;
      line      @5 :Text;
    }

    stepEnd :group {
      stepIndex @6 :Int32;
      result    @7 :Enums.TaskResult;
      message   @8 :Text;  # empty == no message
    }

    buildEnd :group {
      # Exactly one of `errorMessage` / `artifactDiskId` is meaningful;
      # `success` discriminates.
      success         @9  :Bool;
      errorMessage    @10 :Text;
      artifactDiskId  @11 :Int64;
    }

    pipelineEnd :group {
      builds @12 :List(BuildOneResult);
    }
  }
}

struct BuildOneResult {
  name             @0 :Text;
  artifactDiskId   @1 :Int64;   # 0 when this build errored
  errorMessage     @2 :Text;    # empty on success
}

interface BuildEventSink {
  push @0 (event :BuildEvent) -> ();
  end  @1 () -> ();
}

# ---------------------------------------------------------------------
# Apply (declarative environment) events
# ---------------------------------------------------------------------
#
# Streamed when a client calls `Daemon.apply` and supplies an
# `ApplyEventSink` cap. Each phase of `executeApply` emits a
# `phaseStart` followed by per-entity `entityStart` / `entityEnd`
# pairs. Disk imports that download from a URL emit
# `downloadStart` / `downloadProgress` / `downloadEnd` between the
# disk's `entityStart` and `entityEnd`. The stream is closed with
# a single `applyEnd` followed by `end()`.

struct ApplyEvent {
  union {
    logLine @0 :Text;

    phaseStart :group {
      phase @1 :Text;     # "sshKeys" | "disks" | "networks" | "vms" | "templates"
      total @2 :UInt32;
    }

    entityStart :group {
      phase @3 :Text;
      name  @4 :Text;
      kind  @5 :Text;     # e.g. "disk-import", "vm-create", "ssh-key-create"
    }

    entityEnd :group {
      phase    @6  :Text;
      name     @7  :Text;
      result   @8  :Enums.TaskResult;
      message  @9  :Text;  # empty == no message
      entityId @10 :Int64; # 0 when skipped
    }

    downloadStart :group {
      name @11 :Text;      # disk name
      url  @12 :Text;
    }

    downloadProgress :group {
      name       @13 :Text;
      downloaded @14 :Int64;
      total      @15 :Int64; # 0 == unknown (no Content-Length)
    }

    downloadEnd :group {
      name    @16 :Text;
      success @17 :Bool;
      message @18 :Text;
    }

    applyEnd :group {
      result  @19 :Enums.TaskResult;
      message @20 :Text;
      taskId  @21 :Int64;
    }
  }
}

interface ApplyEventSink {
  push @0 (event :ApplyEvent) -> ();
  end  @1 () -> ();
}

# ---------------------------------------------------------------------
# Disk download progress (node-agent → daemon)
# ---------------------------------------------------------------------
#
# Used inside `Session.diskDownload`. The daemon exports a server
# implementation, passes the cap in the call, and the agent pushes
# byte-counted progress to it during the curl/wget transfer.
# `total == 0` until Content-Length is probed (or stays 0 if the
# server didn't return one).

interface DiskDownloadSink {
  progress @0 (downloaded :Int64, total :Int64) -> ();
}

# ---------------------------------------------------------------------
# Guest agent status subscription (new; replaces today's vmShow poll)
# ---------------------------------------------------------------------

struct GuestAgentStatus {
  vmId            @0 :Int64;
  lastHealthcheck @1 :Int64;    # POSIX nanoseconds; 0 == never
  enabled         @2 :Bool;
  reachable       @3 :Bool;
  message         @4 :Text;     # empty == no transient detail
}

interface GuestAgentStatusSink {
  push @0 (status :GuestAgentStatus) -> ();
}

# ---------------------------------------------------------------------
# Task progress subscription (new; complements the Task table)
# ---------------------------------------------------------------------

struct TaskProgressEvent {
  taskId @0 :Int64;
  union {
    started :group {
      command   @1 :Text;
      subsystem @2 :Enums.TaskSubsystem;
    }
    progress :group {
      # Free-form progress: completedUnits/totalUnits + optional label.
      completed @3 :Int64;
      total     @4 :Int64;   # 0 when total is unknown
      label     @5 :Text;    # empty when no label
    }
    finished :group {
      result  @6 :Enums.TaskResult;
      message @7 :Text;
    }
  }
}

interface TaskProgressSink {
  push @0 (event :TaskProgressEvent) -> ();
}

# ---------------------------------------------------------------------
# Subscription handle
# ---------------------------------------------------------------------

# Returned by subscribe-style methods. The server holds a weak
# reference to its subscriber list keyed by the handle's identity;
# when the client drops it, the server prunes.
interface Handle {}
