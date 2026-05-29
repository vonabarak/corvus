import { useQuery } from "@tanstack/react-query";
import { Link, useParams } from "react-router-dom";
import { AlertCircle, ArrowLeft } from "lucide-react";
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

  const {
    data: task,
    error,
    isLoading,
  } = useQuery<TaskInfo>({
    queryKey: ["task", id],
    queryFn: ({ signal }) => getTask(id, signal),
    // While a task is running, poll fast; once it's terminal the
    // refetchInterval auto-stops because TanStack Query honours a
    // false return from the function form.
    refetchInterval: (q) => {
      const t = q.state.data;
      return t && t.result !== "running" ? false : 1000;
    },
    enabled: Number.isFinite(id),
  });

  const { data: children } = useQuery<TaskInfo[]>({
    queryKey: ["task-children", id],
    queryFn: ({ signal }) => listTaskChildren(id, signal),
    // Poll fast while the parent task is still running (child set may
    // grow); slow down once it's terminal.
    refetchInterval: task && task.result === "running" ? 1500 : 5000,
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

  const entityRoute = subsystemEntityRoute(task.subsystem, task.entity_id);

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
              task.entity_name ? (
                entityRoute ? (
                  <Link to={entityRoute} className="hover:underline">
                    {task.entity_name}
                  </Link>
                ) : (
                  <span>{task.entity_name}</span>
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
                  const route = subsystemEntityRoute(c.subsystem, c.entity_id);
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
                        {c.entity_name ? (
                          route ? (
                            <Link to={route} className="hover:underline">
                              {c.entity_name}
                            </Link>
                          ) : (
                            <span>{c.entity_name}</span>
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
