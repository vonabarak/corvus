import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate, useParams } from "react-router-dom";
import { AlertCircle, ArrowLeft, Play, Square, Trash2 } from "lucide-react";
import { toast } from "sonner";
import {
  deleteNetwork,
  getNetwork,
  startNetwork,
  stopNetwork,
  type NetworkInfo,
} from "@/api/networks";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { NetworkStateBadge } from "@/components/NetworkStateBadge";

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

function StartStopButtons({ net }: { net: NetworkInfo }) {
  const queryClient = useQueryClient();
  const invalidate = () => {
    queryClient.invalidateQueries({ queryKey: ["network", net.id] });
    queryClient.invalidateQueries({ queryKey: ["networks"] });
  };
  const start = useMutation({
    mutationFn: () => startNetwork(net.id),
    onSuccess: invalidate,
    onError: (e) => toast.error("Start failed", { description: (e as Error).message }),
  });
  const stop = useMutation({
    mutationFn: (force: boolean) => stopNetwork(net.id, force),
    onSuccess: invalidate,
    onError: (e) => toast.error("Stop failed", { description: (e as Error).message }),
  });
  return (
    <>
      <Button
        variant="outline"
        size="sm"
        onClick={() => start.mutate()}
        disabled={net.running || start.isPending}
      >
        <Play className="h-3.5 w-3.5" />
        Start
      </Button>
      <Button
        variant="outline"
        size="sm"
        onClick={() => stop.mutate(false)}
        disabled={!net.running || stop.isPending}
      >
        <Square className="h-3.5 w-3.5" />
        Stop
      </Button>
      <Button
        variant="outline"
        size="sm"
        onClick={() => {
          if (
            window.confirm(
              `Force-stop "${net.name}"? This detaches in-flight TAPs first; only use if a guest is holding the bridge open.`,
            )
          ) {
            stop.mutate(true);
          }
        }}
        disabled={!net.running || stop.isPending}
      >
        Force stop
      </Button>
    </>
  );
}

function DeleteButton({ net }: { net: NetworkInfo }) {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const mutation = useMutation({
    mutationFn: () => deleteNetwork(net.id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["networks"] });
      navigate("/networks");
    },
    onError: (e) => toast.error("Delete failed", { description: (e as Error).message }),
  });
  const disabled = net.running || mutation.isPending;
  return (
    <Button
      variant="destructive"
      size="sm"
      disabled={disabled}
      title={net.running ? "Stop the network first" : undefined}
      onClick={() => {
        if (window.confirm(`Delete network "${net.name}"?`)) {
          mutation.mutate();
        }
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Delete
    </Button>
  );
}

export default function NetworkDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);

  const {
    data: net,
    error,
    isLoading,
  } = useQuery<NetworkInfo>({
    queryKey: ["network", id],
    queryFn: ({ signal }) => getNetwork(id, signal),
    refetchInterval: 5000,
    enabled: Number.isFinite(id),
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid network id.</p>;
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
            Failed to load network
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!net) return null;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/networks">
            <ArrowLeft className="h-4 w-4" />
            Networks
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">{net.name}</h1>
          <span className="text-sm text-muted-foreground">#{net.id}</span>
          <NetworkStateBadge running={net.running} />
        </div>
      </div>

      <div className="flex flex-wrap gap-2">
        <StartStopButtons net={net} />
        <DeleteButton net={net} />
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="Subnet" value={<span className="font-mono">{net.subnet}</span>} />
          <Field label="DHCP" value={net.dhcp ? "enabled" : "off"} />
          <Field label="NAT" value={net.nat ? "enabled" : "off"} />
          <Field label="Autostart" value={net.autostart ? "yes" : "no"} />
          <Field label="Created" value={new Date(net.created_at).toLocaleString()} />
          {net.vni !== null && <Field label="VNI" value={net.vni} />}
          {net.dnsmasq_pid !== null && <Field label="dnsmasq PID" value={net.dnsmasq_pid} />}
        </CardContent>
      </Card>

      {net.peer_node_ids.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Peer nodes</CardTitle>
            <CardDescription>
              Multi-node overlay membership; bytes flow between TAPs via VXLAN over VNI{" "}
              {net.vni ?? "—"}.
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="flex flex-wrap gap-2">
              {net.peer_node_ids.map((nodeId) => (
                <Badge key={nodeId} variant="muted" className="font-mono">
                  node #{nodeId}
                </Badge>
              ))}
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
