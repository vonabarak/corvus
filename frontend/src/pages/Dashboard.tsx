import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { Activity, AlertCircle, Plug, Server } from "lucide-react";
import { getStatus, type StatusInfo } from "@/api/system";
import { listTasks, type TaskInfo } from "@/api/tasks";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { TaskResultBadge } from "@/components/TaskResultBadge";
import { subsystemEntityRoute } from "@/lib/entityLink";

function formatUptime(seconds: number): string {
  if (seconds < 60) return `${seconds}s`;
  const m = Math.floor(seconds / 60);
  if (m < 60) return `${m}m`;
  const h = Math.floor(m / 60);
  if (h < 24) return `${h}h ${m % 60}m`;
  const d = Math.floor(h / 24);
  return `${d}d ${h % 24}h`;
}

interface TileProps {
  label: string;
  value: string;
  icon: React.ReactNode;
}

function Tile({ label, value, icon }: TileProps) {
  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
        <CardTitle className="text-sm font-medium">{label}</CardTitle>
        <span className="text-muted-foreground">{icon}</span>
      </CardHeader>
      <CardContent>
        <div className="text-2xl font-semibold">{value}</div>
      </CardContent>
    </Card>
  );
}

function RecentTasksCard() {
  const { data, error } = useQuery<TaskInfo[]>({
    queryKey: ["tasks", "recent"],
    queryFn: ({ signal }) => listTasks({ limit: 10 }, signal),
    refetchInterval: 3000,
  });
  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-base">Recent tasks</CardTitle>
        <CardDescription>
          Last 10 mutating RPCs the daemon received.{" "}
          <Link to="/tasks" className="text-foreground hover:underline">
            All tasks →
          </Link>
        </CardDescription>
      </CardHeader>
      <CardContent className="p-0">
        {error ? (
          <p className="px-6 pb-6 text-sm text-destructive">{(error as Error).message}</p>
        ) : !data || data.length === 0 ? (
          <p className="px-6 pb-6 text-sm text-muted-foreground">No tasks recorded yet.</p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>ID</TableHead>
                <TableHead>Subsystem</TableHead>
                <TableHead>Command</TableHead>
                <TableHead>Entity</TableHead>
                <TableHead>Result</TableHead>
                <TableHead>Started</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {data.map((t) => {
                const route = subsystemEntityRoute(t.subsystem, t.entity_id);
                return (
                  <TableRow key={t.id}>
                    <TableCell>
                      <Link to={`/tasks/${t.id}`} className="font-mono text-xs hover:underline">
                        #{t.id}
                      </Link>
                    </TableCell>
                    <TableCell className="font-mono text-xs">{t.subsystem}</TableCell>
                    <TableCell className="font-mono text-xs">{t.command}</TableCell>
                    <TableCell>
                      {t.entity_name ? (
                        route ? (
                          <Link to={route} className="hover:underline">
                            {t.entity_name}
                          </Link>
                        ) : (
                          <span>{t.entity_name}</span>
                        )
                      ) : (
                        <span className="text-muted-foreground">—</span>
                      )}
                    </TableCell>
                    <TableCell>
                      <TaskResultBadge result={t.result} />
                    </TableCell>
                    <TableCell className="text-xs text-muted-foreground">
                      {new Date(t.started_at).toLocaleString()}
                    </TableCell>
                  </TableRow>
                );
              })}
            </TableBody>
          </Table>
        )}
      </CardContent>
    </Card>
  );
}

export default function Dashboard() {
  const { data, error, isLoading } = useQuery<StatusInfo>({
    queryKey: ["status"],
    queryFn: ({ signal }) => getStatus(signal),
    refetchInterval: 5000,
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
            Daemon unreachable
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!data) return null;

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold tracking-tight">Dashboard</h1>
        <p className="text-sm text-muted-foreground">Live state of the corvus daemon.</p>
      </div>
      <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-4">
        <Tile label="Daemon" value={data.version} icon={<Server className="h-4 w-4" />} />
        <Tile
          label="Uptime"
          value={formatUptime(data.uptime_seconds)}
          icon={<Activity className="h-4 w-4" />}
        />
        <Tile
          label="Connections"
          value={String(data.connections)}
          icon={<Plug className="h-4 w-4" />}
        />
        <Tile
          label="Protocol"
          value={`v${data.protocol_version}`}
          icon={<Server className="h-4 w-4" />}
        />
      </div>
      <RecentTasksCard />
    </div>
  );
}
