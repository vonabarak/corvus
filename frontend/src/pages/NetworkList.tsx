import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, Network as NetIcon } from "lucide-react";
import { listNetworks, type NetworkInfo } from "@/api/networks";
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
import { NetworkStateBadge } from "@/components/NetworkStateBadge";

function FlagBadges({ net }: { net: NetworkInfo }) {
  return (
    <div className="flex flex-wrap gap-1">
      {net.dhcp && (
        <Badge variant="muted" className="text-[10px]">
          DHCP
        </Badge>
      )}
      {net.nat && (
        <Badge variant="muted" className="text-[10px]">
          NAT
        </Badge>
      )}
      {net.autostart && (
        <Badge variant="muted" className="text-[10px]">
          autostart
        </Badge>
      )}
    </div>
  );
}

export default function NetworkList() {
  const { data, error, isLoading } = useQuery<NetworkInfo[]>({
    queryKey: ["networks"],
    queryFn: ({ signal }) => listNetworks(signal),
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
            Failed to load networks
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const networks = data ?? [];

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold tracking-tight">Networks</h1>
        <p className="text-sm text-muted-foreground">
          {networks.length === 0
            ? "No virtual networks yet."
            : `${networks.length} network${networks.length === 1 ? "" : "s"} registered.`}
        </p>
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Name</TableHead>
              <TableHead>Subnet</TableHead>
              <TableHead>State</TableHead>
              <TableHead>Flags</TableHead>
              <TableHead className="text-right">VNI</TableHead>
              <TableHead className="text-right">Peers</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {networks.map((n) => (
              <TableRow key={n.id}>
                <TableCell>
                  <Link
                    to={`/networks/${n.id}`}
                    className="inline-flex items-center gap-2 font-medium text-foreground hover:underline"
                  >
                    <NetIcon className="h-3.5 w-3.5 text-muted-foreground" />
                    {n.name}
                  </Link>
                  <div className="text-xs text-muted-foreground">#{n.id}</div>
                </TableCell>
                <TableCell className="font-mono text-xs">{n.subnet}</TableCell>
                <TableCell>
                  <NetworkStateBadge running={n.running} />
                </TableCell>
                <TableCell>
                  <FlagBadges net={n} />
                </TableCell>
                <TableCell className="text-right tabular-nums">
                  {n.vni !== null ? n.vni : <span className="text-muted-foreground">—</span>}
                </TableCell>
                <TableCell className="text-right tabular-nums">
                  {n.peer_node_ids.length > 0 ? (
                    n.peer_node_ids.length
                  ) : (
                    <span className="text-muted-foreground">—</span>
                  )}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Card>
    </div>
  );
}
