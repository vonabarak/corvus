@0xd3867de7dfe678c7;

using Corvus = import "corvus.capnp";
using Enums = import "enums.capnp";
using Streams = import "streams.capnp";

# ---------------------------------------------------------------------
# Info struct
# ---------------------------------------------------------------------

struct TaskInfo {
  id          @0  :Int64;
  parentId    @1  :Int64;   # 0 == no parent
  startedAt   @2  :Int64;   # POSIX nanoseconds
  finishedAt  @3  :Int64;   # 0 == still running
  subsystem   @4  :Enums.TaskSubsystem;
  entityId    @5  :Int64;   # 0 == none
  entityName  @6  :Text;    # empty == none
  command     @7  :Text;
  result      @8  :Enums.TaskResult;
  message     @9  :Text;    # empty == no message
}

struct TaskListParams {
  # Optional filters; default values mean "no filter".
  limit       @0 :Int32;        # 0 == no limit
  subsystem   @1 :Enums.TaskSubsystem;
  hasSubsystem @2 :Bool;
  entityId    @3 :Int64;        # 0 == any
  result      @4 :Enums.TaskResult;
  hasResult   @5 :Bool;
}

# ---------------------------------------------------------------------
# Manager + resource capabilities
# ---------------------------------------------------------------------

interface TaskManager {
  list         @0 (params :TaskListParams) -> (tasks :List(TaskInfo));
  get          @1 (taskId :Int64) -> (task :Task);
  listChildren @2 (parentId :Int64) -> (tasks :List(TaskInfo));

  # Subscribe to live progress events for the given task. The
  # returned `handle` keeps the subscription alive; drop it to
  # unsubscribe.
  subscribe @3 (taskId :Int64, sink :Streams.TaskProgressSink)
              -> (handle :Streams.Handle);
}

interface Task {
  show @0 () -> (info :TaskInfo);
}
