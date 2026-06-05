@0xd3867de7dfe678c7;

using Common = import "common.capnp";
using Enums = import "enums.capnp";
using Streams = import "streams.capnp";

# ---------------------------------------------------------------------
# Info struct
# ---------------------------------------------------------------------

struct TaskInfo {
  id          @0  :Int64;
  # Parent task id, or 0 if this is a top-level task. Stays flat
  # (no nested NamedRef) because tasks don't have a human-readable
  # name field — there's nothing to nest. Operators chase the
  # parent by id alone.
  parentId    @1  :Int64;   # 0 == no parent
  startedAt   @2  :Int64;   # POSIX nanoseconds
  finishedAt  @3  :Int64;   # 0 == still running
  subsystem   @4  :Enums.TaskSubsystem;
  # Subject of the task (the VM / disk / etc. being acted on).
  # `id == 0` => the task isn't about a specific entity (e.g.
  # daemon-level startup/shutdown).
  entity      @5  :Common.NamedRef;
  command     @6  :Text;
  result      @7  :Enums.TaskResult;
  message     @8  :Text;    # empty == no message
  clientName  @9  :Text;    # "local", "system", or the client cert CN suffix
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

  # Request cancellation of a running task. Best-effort: flips the
  # task's cooperative cancel flag (honoured at subtask boundaries
  # and long-loop checkpoints) and cancels the task's worker thread
  # for asynchronously-dispatched actions. A no-op for tasks that
  # have already finished. Returns once the request is recorded, not
  # once the task actually stops.
  cancel @4 (taskId :Int64) -> ();
}

interface Task {
  show @0 () -> (info :TaskInfo);
}
