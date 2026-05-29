import { useQuery } from "@tanstack/react-query";
import { Link, useParams } from "react-router-dom";
import { AlertCircle, ArrowLeft } from "lucide-react";
import { getNode, type NodeDetails } from "@/api/nodes";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { NodeStateBadge } from "@/components/NodeStateBadge";
import { formatBytes, formatMb } from "@/lib/format";

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

function fmtPercent(used: number, total: number): string {
  if (total === 0) return "—";
  return `${((used / total) * 100).toFixed(1)}%`;
}

export default function NodeDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);

  const {
    data: node,
    error,
    isLoading,
  } = useQuery<NodeDetails>({
    queryKey: ["node", id],
    queryFn: ({ signal }) => getNode(id, signal),
    refetchInterval: 5000,
    enabled: Number.isFinite(id),
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid node id.</p>;
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
            Failed to load node
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!node) return null;

  const ramUsed =
    node.ram_mb_total !== null && node.ram_mb_free !== null
      ? node.ram_mb_total - node.ram_mb_free
      : null;
  const storageUsed =
    node.storage_bytes_total !== null && node.storage_bytes_free !== null
      ? node.storage_bytes_total - node.storage_bytes_free
      : null;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/nodes">
            <ArrowLeft className="h-4 w-4" />
            Nodes
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">{node.name}</h1>
          <span className="text-sm text-muted-foreground">#{node.id}</span>
          <NodeStateBadge state={node.admin_state} />
        </div>
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="Host" value={<span className="font-mono">{node.host}</span>} />
          <Field
            label="Agent ports"
            value={
              <span className="font-mono text-xs">
                nodeagent {node.node_agent_port} · netd {node.net_agent_port}
              </span>
            }
          />
          <Field
            label="Base path"
            value={<span className="font-mono text-xs">{node.base_path}</span>}
          />
          <Field label="Created" value={new Date(node.created_at).toLocaleString()} />
          {node.kernel_release && (
            <Field
              label="Kernel"
              value={<span className="font-mono text-xs">{node.kernel_release}</span>}
            />
          )}
          {node.agent_version && (
            <Field
              label="Agent version"
              value={<span className="font-mono text-xs">{node.agent_version}</span>}
            />
          )}
          {node.description && <Field label="Description" value={node.description} />}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Capacity</CardTitle>
          <CardDescription>Last snapshot pushed by the node agent.</CardDescription>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="CPU cores" value={node.cpu_count ?? "—"} />
          <Field
            label="RAM"
            value={
              node.ram_mb_total !== null ? (
                <div>
                  <div>{formatMb(node.ram_mb_total)}</div>
                  {ramUsed !== null && node.ram_mb_total > 0 && (
                    <div className="text-xs text-muted-foreground">
                      {formatMb(ramUsed)} used ({fmtPercent(ramUsed, node.ram_mb_total)})
                    </div>
                  )}
                </div>
              ) : (
                "—"
              )
            }
          />
          <Field
            label="Storage"
            value={
              node.storage_bytes_total !== null ? (
                <div>
                  <div>{formatBytes(node.storage_bytes_total)}</div>
                  {storageUsed !== null && node.storage_bytes_total > 0 && (
                    <div className="text-xs text-muted-foreground">
                      {formatBytes(storageUsed)} used (
                      {fmtPercent(storageUsed, node.storage_bytes_total)})
                    </div>
                  )}
                </div>
              ) : (
                "—"
              )
            }
          />
          <Field
            label="Load avg"
            value={
              node.load_avg1 !== null ? (
                <span className="font-mono">
                  {node.load_avg1.toFixed(2)} · {node.load_avg5?.toFixed(2) ?? "—"} ·{" "}
                  {node.load_avg15?.toFixed(2) ?? "—"}
                </span>
              ) : (
                "—"
              )
            }
          />
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Agents</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-2">
          <Field
            label="corvus-nodeagent"
            value={
              <div className="space-y-1">
                <Badge variant={node.last_node_agent_push_at ? "success" : "muted"}>
                  {node.last_node_agent_push_at ? "connected" : "never seen"}
                </Badge>
                {node.last_node_agent_push_at && (
                  <div className="text-xs text-muted-foreground">
                    last push: {new Date(node.last_node_agent_push_at).toLocaleString()}
                  </div>
                )}
              </div>
            }
          />
          <Field
            label="corvus-netd"
            value={
              <div className="space-y-1">
                {node.netd_disabled ? (
                  <Badge variant="muted">disabled</Badge>
                ) : (
                  <Badge variant={node.netd_connected ? "success" : "muted"}>
                    {node.netd_connected ? "connected" : "disconnected"}
                  </Badge>
                )}
                {node.last_net_agent_push_at && (
                  <div className="text-xs text-muted-foreground">
                    last push: {new Date(node.last_net_agent_push_at).toLocaleString()}
                  </div>
                )}
              </div>
            }
          />
        </CardContent>
      </Card>
    </div>
  );
}
