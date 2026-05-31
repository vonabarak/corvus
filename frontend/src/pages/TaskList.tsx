import { useEffect, useState } from "react";
import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, ChevronDown, History } from "lucide-react";
import { listTasks, type TaskInfo } from "@/api/tasks";
import { Card, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
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

// Subsystem enum from src/Corvus/Model.hs TaskSubsystem. Keep in
// lockstep — the daemon enforces these values when filtering.
const SUBSYSTEMS = [
  "",
  "vm",
  "disk",
  "network",
  "ssh-key",
  "template",
  "shared-dir",
  "snapshot",
  "system",
  "apply",
] as const;

const RESULTS = ["", "running", "success", "error"] as const;

// Pagination policy.
//
// The daemon's TaskListParams only carries a `limit` (no offset, no
// cursor — see corvus_client._async.task.AsyncTaskManager.list), so a
// classic page-number UI would mean refetching the prefix every time.
// Instead we grow the limit on demand: each "Load more" click bumps it
// by PAGE_SIZE. The corvus-web /api/tasks endpoint caps `limit` at 500
// (Query(50, ge=1, le=500) in routes/tasks.py), which is also the hard
// ceiling here.
const PAGE_SIZE = 50;
const MAX_LIMIT = 500;

function FilterSelect<T extends string>({
  value,
  onChange,
  options,
  label,
}: {
  value: T;
  onChange: (v: T) => void;
  options: readonly T[];
  label: string;
}) {
  return (
    <label className="flex items-center gap-2 text-sm">
      <span className="text-muted-foreground">{label}:</span>
      <select
        value={value}
        onChange={(e) => onChange(e.target.value as T)}
        className="h-8 rounded-md border border-input bg-background px-2 text-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
      >
        {options.map((opt) => (
          <option key={opt} value={opt}>
            {opt === "" ? "all" : opt}
          </option>
        ))}
      </select>
    </label>
  );
}

function durationMs(start: string, finish: string | null): string {
  if (!finish) return "—";
  const ms = Date.parse(finish) - Date.parse(start);
  if (ms < 1000) return `${ms} ms`;
  const s = ms / 1000;
  if (s < 60) return `${s.toFixed(1)} s`;
  const m = Math.floor(s / 60);
  return `${m}m ${(s - m * 60).toFixed(0)}s`;
}

export default function TaskList() {
  const [subsystem, setSubsystem] = useState<(typeof SUBSYSTEMS)[number]>("");
  const [result, setResult] = useState<(typeof RESULTS)[number]>("");
  const [limit, setLimit] = useState<number>(PAGE_SIZE);

  // Reset to the first page when the filter changes — otherwise the
  // grown limit carries across an unrelated filter and the operator
  // sees more rows than they asked for.
  useEffect(() => {
    setLimit(PAGE_SIZE);
  }, [subsystem, result]);

  const { data, error, isLoading } = useQuery<TaskInfo[]>({
    queryKey: ["tasks", subsystem, result, limit],
    queryFn: ({ signal }) =>
      listTasks(
        {
          limit,
          ...(subsystem ? { subsystem } : {}),
          ...(result ? { result } : {}),
        },
        signal,
      ),
    refetchInterval: 2000,
  });

  if (isLoading) {
    return <p className="text-muted-foreground">Loading…</p>;
  }
  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <AlertCircle className="h-5 w-5 text-destructive" />
            Failed to load tasks
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const tasks = data ?? [];
  // If the daemon returned fewer rows than we asked for, there are no
  // older tasks to load — disable the button. Same logic at the hard
  // cap MAX_LIMIT.
  const reachedEnd = tasks.length < limit;
  const reachedCap = limit >= MAX_LIMIT;
  const canLoadMore = !reachedEnd && !reachedCap;

  return (
    <div className="space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-semibold tracking-tight">Tasks</h1>
          <p className="text-sm text-muted-foreground">
            Every mutating RPC the daemon receives is recorded here.
          </p>
        </div>
        <div className="flex items-center gap-3">
          <FilterSelect
            value={subsystem}
            onChange={setSubsystem}
            options={SUBSYSTEMS}
            label="Subsystem"
          />
          <FilterSelect value={result} onChange={setResult} options={RESULTS} label="Result" />
        </div>
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>ID</TableHead>
              <TableHead>Started</TableHead>
              <TableHead>Subsystem</TableHead>
              <TableHead>Command</TableHead>
              <TableHead>Entity</TableHead>
              <TableHead>Result</TableHead>
              <TableHead className="text-right">Duration</TableHead>
              <TableHead>Client</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {tasks.map((t) => {
              const route = subsystemEntityRoute(t.subsystem, t.entity?.id ?? null);
              return (
                <TableRow key={t.id}>
                  <TableCell>
                    <Link
                      to={`/tasks/${t.id}`}
                      className="inline-flex items-center gap-2 font-mono text-xs hover:underline"
                    >
                      <History className="h-3.5 w-3.5 text-muted-foreground" />#{t.id}
                    </Link>
                  </TableCell>
                  <TableCell className="text-xs text-muted-foreground">
                    {new Date(t.started_at).toLocaleString()}
                  </TableCell>
                  <TableCell className="font-mono text-xs">{t.subsystem}</TableCell>
                  <TableCell className="font-mono text-xs">{t.command}</TableCell>
                  <TableCell>
                    {t.entity ? (
                      route ? (
                        <Link to={route} className="hover:underline">
                          {t.entity.name}
                        </Link>
                      ) : (
                        <span>{t.entity.name}</span>
                      )
                    ) : (
                      <span className="text-muted-foreground">—</span>
                    )}
                  </TableCell>
                  <TableCell>
                    <TaskResultBadge result={t.result} />
                  </TableCell>
                  <TableCell className="text-right tabular-nums">
                    {durationMs(t.started_at, t.finished_at)}
                  </TableCell>
                  <TableCell className="text-muted-foreground">{t.client_name}</TableCell>
                </TableRow>
              );
            })}
          </TableBody>
        </Table>
      </Card>
      <div className="flex items-center justify-between text-sm text-muted-foreground">
        <span>
          Showing {tasks.length} task{tasks.length === 1 ? "" : "s"}
          {reachedEnd ? " (all)" : ""}
          {reachedCap && !reachedEnd ? ` (capped at ${MAX_LIMIT})` : ""}
        </span>
        <Button
          variant="outline"
          size="sm"
          disabled={!canLoadMore}
          onClick={() => setLimit((l) => Math.min(l + PAGE_SIZE, MAX_LIMIT))}
          title={
            reachedEnd
              ? "No more tasks to load."
              : reachedCap
                ? `Hit the daemon's ${MAX_LIMIT}-row cap; narrow the filter to see older tasks.`
                : undefined
          }
        >
          <ChevronDown className="h-3.5 w-3.5" />
          Load {PAGE_SIZE} more
        </Button>
      </div>
    </div>
  );
}
