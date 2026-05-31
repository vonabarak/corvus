import { useEffect, useState } from "react";
import { useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useParams } from "react-router-dom";
import { AlertCircle, ArrowLeft, Radio } from "lucide-react";
import { getTask, listTaskChildren, type TaskInfo } from "@/api/tasks";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Button } from "@/components/ui/button";
import { TaskResultBadge } from "@/components/TaskResultBadge";
import { subsystemEntityRoute } from "@/lib/entityLink";
import { useWebSocketJson } from "@/hooks/useWebSocketJson";

// Wire-format mirror of python/corvus_web/routes/tasks.py
// _task_progress_event_to_dict — one frame per daemon push.
type ProgressFrame =
  | { type: "started"; task_id: number; command: string; subsystem: string }
  | {
      type: "progress";
      task_id: number;
      completed: number;
      total: number | null;
      label: string | null;
    }
  | { type: "finished"; task_id: number; result: string; message: string | null }
  | { type: "unknown"; payload: string };

interface FieldProps {
  label: string;
  value: React.ReactNode;
}

function Field({ label, value }: FieldProps) {
  return (
    <div className="space-y-1">
      <div className="text-xs uppercase tracking-wide text-muted-foreground">{label}</div>
      <div className="text-sm">{value}</div>
    </div>
  );
}

function duration(start: string, finish: string | null): string {
  if (!finish) return "still running…";
  const ms = Date.parse(finish) - Date.parse(start);
  if (ms < 1000) return `${ms} ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(2)} s`;
  const m = Math.floor(s / 60);
  return `${m}m ${(s - m * 60).toFixed(1)}s`;
}

export default function TaskDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);
  const queryClient = useQueryClient();

  const {
    data: task,
    error,
    isLoading,
  } = useQuery<TaskInfo>({
    queryKey: ["task", id],
    queryFn: ({ signal }) => getTask(id, signal),
    enabled: Number.isFinite(id),
  });

  // Live progress events from the daemon's TaskProgressSink. The
  // backend closes the WS after the `finished` frame; once that
  // happens we invalidate the task query so finished_at + message
  // + final result land on screen.
  const wsEnabled = Number.isFinite(id) && !!task && task.result === "running";
  const { last: progress, state: wsState } = useWebSocketJson<ProgressFrame>(
    `/api/tasks/${id}/ws`,
    wsEnabled,
  );

  // Cache the most recent progress frame so the panel keeps showing
  // it even after the WS closes (TanStack Query unmount semantics
  // would otherwise drop the last value on transition).
  const [latestProgress, setLatestProgress] = useState<ProgressFrame | null>(null);
  useEffect(() => {
    if (progress) setLatestProgress(progress);
  }, [progress]);

  // When the daemon emits `finished`, refresh the task + children so
  // the page shows the final result/finished_at/message.
  useEffect(() => {
    if (progress?.type === "finished") {
      queryClient.invalidateQueries({ queryKey: ["task", id] });
      queryClient.invalidateQueries({ queryKey: ["task-children", id] });
    }
  }, [progress, id, queryClient]);

  // Children: poll only while the parent task is running (the set may
  // grow). Once terminal, refresh on the same invalidate that runs
  // after `finished`.
  const { data: children } = useQuery<TaskInfo[]>({
    queryKey: ["task-children", id],
    queryFn: ({ signal }) => listTaskChildren(id, signal),
    refetchInterval: task && task.result === "running" ? 2000 : false,
    enabled: Number.isFinite(id) && !!task,
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid task id.</p>;
  }
  if (isLoading) {
    return <p className="text-muted-foreground">Loading…</p>;
  }
  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <AlertCircle className="h-5 w-5 text-destructive" />
            Failed to load task
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!task) return null;

  const entityRoute = subsystemEntityRoute(task.subsystem, task.entity?.id ?? null);

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/tasks">
            <ArrowLeft className="h-4 w-4" />
            Tasks
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">Task #{task.id}</h1>
          <TaskResultBadge result={task.result} />
        </div>
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="Subsystem" value={<span className="font-mono">{task.subsystem}</span>} />
          <Field label="Command" value={<span className="font-mono">{task.command}</span>} />
          <Field
            label="Entity"
            value={
              task.entity ? (
                entityRoute ? (
                  <Link to={entityRoute} className="hover:underline">
                    {task.entity.name}
                  </Link>
                ) : (
                  <span>{task.entity.name}</span>
                )
              ) : (
                <span className="text-muted-foreground">—</span>
              )
            }
          />
          <Field label="Client" value={task.client_name} />
          <Field label="Started" value={new Date(task.started_at).toLocaleString()} />
          <Field
            label="Finished"
            value={
              task.finished_at ? (
                new Date(task.finished_at).toLocaleString()
              ) : (
                <span className="text-muted-foreground">—</span>
              )
            }
          />
          <Field label="Duration" value={duration(task.started_at, task.finished_at)} />
          {task.parent_id !== null && (
            <Field
              label="Parent"
              value={
                <Link to={`/tasks/${task.parent_id}`} className="hover:underline">
                  #{task.parent_id}
                </Link>
              }
            />
          )}
        </CardContent>
      </Card>

      {task.result === "running" && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-base">
              <Radio
                className={`h-4 w-4 ${wsState === "open" ? "text-emerald-400" : "text-muted-foreground"}`}
              />
              Live progress
            </CardTitle>
            <CardDescription>
              {wsState === "open"
                ? "Subscribed to the daemon's task-progress stream."
                : wsState === "connecting"
                  ? "Connecting…"
                  : "Disconnected — refresh to reconnect."}
            </CardDescription>
          </CardHeader>
          <CardContent>
            {latestProgress?.type === "progress" ? (
              <div className="space-y-2">
                <div className="flex items-center justify-between text-sm">
                  <span className="text-muted-foreground">
                    {latestProgress.label ?? "working…"}
                  </span>
                  <span className="font-mono tabular-nums">
                    {latestProgress.completed}
                    {latestProgress.total !== null ? ` / ${latestProgress.total}` : ""}
                  </span>
                </div>
                {latestProgress.total !== null && latestProgress.total > 0 && (
                  <div className="h-2 w-full overflow-hidden rounded-full bg-muted">
                    <div
                      className="h-full bg-primary transition-all"
                      style={{
                        width: `${Math.min(100, Math.round((latestProgress.completed / latestProgress.total) * 100))}%`,
                      }}
                    />
                  </div>
                )}
              </div>
            ) : (
              <p className="text-sm text-muted-foreground">Waiting for the first progress event…</p>
            )}
          </CardContent>
        </Card>
      )}

      {task.message && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">
              {task.result === "error" ? "Error message" : "Message"}
            </CardTitle>
          </CardHeader>
          <CardContent>
            <pre
              className={`overflow-auto rounded-md border border-border p-3 font-mono text-xs ${task.result === "error" ? "bg-destructive/10 text-destructive" : "bg-muted/30"}`}
            >
              {task.message}
            </pre>
          </CardContent>
        </Card>
      )}

      {children && children.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Sub-tasks</CardTitle>
            <CardDescription>
              Apply and build flows record one child task per resource they touch.
            </CardDescription>
          </CardHeader>
          <CardContent className="p-0">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>ID</TableHead>
                  <TableHead>Subsystem</TableHead>
                  <TableHead>Command</TableHead>
                  <TableHead>Entity</TableHead>
                  <TableHead>Result</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {children.map((c) => {
                  const route = subsystemEntityRoute(c.subsystem, c.entity?.id ?? null);
                  return (
                    <TableRow key={c.id}>
                      <TableCell>
                        <Link to={`/tasks/${c.id}`} className="font-mono text-xs hover:underline">
                          #{c.id}
                        </Link>
                      </TableCell>
                      <TableCell className="font-mono text-xs">{c.subsystem}</TableCell>
                      <TableCell className="font-mono text-xs">{c.command}</TableCell>
                      <TableCell>
                        {c.entity ? (
                          route ? (
                            <Link to={route} className="hover:underline">
                              {c.entity.name}
                            </Link>
                          ) : (
                            <span>{c.entity.name}</span>
                          )
                        ) : (
                          <span className="text-muted-foreground">—</span>
                        )}
                      </TableCell>
                      <TableCell>
                        <TaskResultBadge result={c.result} />
                      </TableCell>
                    </TableRow>
                  );
                })}
              </TableBody>
            </Table>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
