import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, Server } from "lucide-react";
import { listNodes, type NodeInfo } from "@/api/nodes";
import { Card, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { NodeStateBadge } from "@/components/NodeStateBadge";
import { formatBytes, formatMb } from "@/lib/format";

function ramText(n: NodeInfo): string {
  if (n.ram_mb_total === null) return "—";
  if (n.ram_mb_free === null) return formatMb(n.ram_mb_total);
  return `${formatMb(n.ram_mb_free)} / ${formatMb(n.ram_mb_total)} free`;
}

function storageText(n: NodeInfo): string {
  if (n.storage_bytes_total === null) return "—";
  if (n.storage_bytes_free === null) return formatBytes(n.storage_bytes_total);
  return `${formatBytes(n.storage_bytes_free)} / ${formatBytes(n.storage_bytes_total)} free`;
}

export default function NodeList() {
  const { data, error, isLoading } = useQuery<NodeInfo[]>({
    queryKey: ["nodes"],
    queryFn: ({ signal }) => listNodes(signal),
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
            Failed to load nodes
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const nodes = data ?? [];

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold tracking-tight">Nodes</h1>
        <p className="text-sm text-muted-foreground">
          Hosts the daemon orchestrates. Node CRUD lives in <code>corvus-admin</code> — this is a
          read-only view of capacity and agent state.
        </p>
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Name</TableHead>
              <TableHead>Host</TableHead>
              <TableHead>State</TableHead>
              <TableHead className="text-right">CPU</TableHead>
              <TableHead>RAM</TableHead>
              <TableHead>Storage</TableHead>
              <TableHead className="text-right">Load</TableHead>
              <TableHead>Agents</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {nodes.map((n) => (
              <TableRow key={n.id}>
                <TableCell>
                  <Link
                    to={`/nodes/${n.id}`}
                    className="inline-flex items-center gap-2 font-medium text-foreground hover:underline"
                  >
                    <Server className="h-3.5 w-3.5 text-muted-foreground" />
                    {n.name}
                  </Link>
                  <div className="text-xs text-muted-foreground">#{n.id}</div>
                </TableCell>
                <TableCell className="font-mono text-xs">
                  {n.host}
                  <span className="text-muted-foreground">:{n.node_agent_port}</span>
                </TableCell>
                <TableCell>
                  <NodeStateBadge state={n.admin_state} />
                </TableCell>
                <TableCell className="text-right tabular-nums">{n.cpu_count ?? "—"}</TableCell>
                <TableCell className="text-xs">{ramText(n)}</TableCell>
                <TableCell className="text-xs">{storageText(n)}</TableCell>
                <TableCell className="text-right tabular-nums">
                  {n.load_avg1 !== null ? n.load_avg1.toFixed(2) : "—"}
                </TableCell>
                <TableCell>
                  <div className="flex flex-wrap gap-1">
                    <Badge
                      variant={n.last_node_agent_push_at ? "success" : "muted"}
                      className="text-[10px]"
                    >
                      nodeagent
                    </Badge>
                    {!n.netd_disabled && (
                      <Badge
                        variant={n.netd_connected ? "success" : "muted"}
                        className="text-[10px]"
                      >
                        netd
                      </Badge>
                    )}
                  </div>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Card>
    </div>
  );
}
