import { useQuery } from "@tanstack/react-query";
import { Activity, AlertCircle, Server, Plug } from "lucide-react";
import { getStatus, type StatusInfo } from "@/api/system";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";

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
    </div>
  );
}
