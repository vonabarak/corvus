# Task History

Every mutating operation in Corvus (creating a VM, importing a disk, applying a config, etc.) is recorded as a **task** in the database. Tasks track when an operation started, when it finished, whether it succeeded or failed, and any error messages.

Async operations (e.g., `crv disk import` without `--wait`, `crv apply` without `--wait`) return a task ID immediately. Use `crv task wait` to block until completion.

## Commands

```bash
crv task list [options]     # List recent tasks
crv task show <id>          # Show task details
crv task wait <id> [--timeout <seconds>]   # Wait for a task to complete
```

## Listing Tasks

```bash
crv task list                              # Last 10 tasks
crv task list -n 50                        # Last 50 tasks
crv task list --subsystem vm               # Only VM operations
crv task list --result error               # Only failed tasks
crv task list --subsystem disk --result success  # Successful disk ops
crv task list --all                        # Include subtasks
```

| Option | Description |
|--------|-------------|
| `-n`, `--last` | Number of tasks to show (default: 10) |
| `--subsystem` | Filter by subsystem (see below) |
| `--result` | Filter by result (see below) |
| `-a`, `--all` | Include subtasks in the list |

### Subsystems

| Value | Operations |
|-------|------------|
| `vm` | create, start, stop, pause, reset, delete, edit, cloud-init |
| `disk` | create, register, import, overlay, clone, rebase, resize, delete, attach, detach, refresh |
| `network` | create, delete, start, stop, edit |
| `ssh-key` | create, delete, attach, detach |
| `template` | create, update, delete, instantiate |
| `shared-dir` | add, remove |
| `snapshot` | create, delete, rollback, merge |
| `system` | startup, shutdown |
| `apply` | apply |

### Results

| Value | Meaning |
|-------|---------|
| `success` | Completed successfully |
| `error` | Failed with an error |
| `running` | Still in progress |
| `not_started` | Queued but not yet started (subtask) |
| `cancelled` | Cancelled because a sibling subtask failed |

## Showing Task Details

```bash
crv task show 42
```

Displays the task's ID, parent task (if a subtask), timestamps, subsystem, command, entity (VM/disk/etc.), result, and error message (if any).

## Waiting for Completion

```bash
crv task wait 42              # Wait indefinitely
crv task wait 42 --timeout 60 # Wait up to 60 seconds
```

Blocks until the task finishes (success or error). Returns exit code 0 on success, 1 on error or timeout.

Common use with async operations:

```bash
# Import a disk asynchronously, then wait
crv disk import large-image https://example.com/big.qcow2
# Output: Import started (task ID: 42)
crv task wait 42
```

## Subtasks

Composite operations (like `crv apply` or `crv template instantiate`) create a parent task with multiple subtasks. Each subtask tracks an individual step (create SSH key, create disk, create VM, etc.).

```bash
crv task list --all            # Show parent + subtasks
crv task show 42               # Parent task shows overall result
```

If any subtask fails, remaining sibling subtasks are cancelled and the parent task is marked as failed.
