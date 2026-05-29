import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, Database, Plus } from "lucide-react";
import { listDisks, type DiskImageInfo } from "@/api/disks";
import { Card, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { formatMb } from "@/lib/format";

function PlacementSummary({ disk }: { disk: DiskImageInfo }) {
  if (disk.placements.length === 0) {
    return <span className="text-muted-foreground">—</span>;
  }
  if (disk.placements.length === 1) {
    return <span>{disk.placements[0].node_name}</span>;
  }
  return <span>{disk.placements.length} nodes</span>;
}

function AttachedSummary({ disk }: { disk: DiskImageInfo }) {
  if (disk.attached_to.length === 0) {
    return <span className="text-muted-foreground">—</span>;
  }
  return (
    <div className="flex flex-wrap gap-1">
      {disk.attached_to.map((a) => (
        <Link
          key={a.vm_id}
          to={`/vms/${a.vm_id}`}
          className="text-xs text-foreground hover:underline"
        >
          {a.vm_name}
        </Link>
      ))}
    </div>
  );
}

export default function DiskList() {
  const { data, error, isLoading } = useQuery<DiskImageInfo[]>({
    queryKey: ["disks"],
    queryFn: ({ signal }) => listDisks(signal),
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
            Failed to load disks
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const disks = data ?? [];

  return (
    <div className="space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-semibold tracking-tight">Disks</h1>
          <p className="text-sm text-muted-foreground">
            {disks.length === 0
              ? "No disks yet."
              : `${disks.length} disk${disks.length === 1 ? "" : "s"} registered.`}
          </p>
        </div>
        <Button size="sm" asChild>
          <Link to="/disks/new">
            <Plus className="h-3.5 w-3.5" />
            New disk
          </Link>
        </Button>
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Name</TableHead>
              <TableHead>Format</TableHead>
              <TableHead className="text-right">Size</TableHead>
              <TableHead>Placement</TableHead>
              <TableHead>Backing</TableHead>
              <TableHead>Attached to</TableHead>
              <TableHead>Flags</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {disks.map((d) => (
              <TableRow key={d.id}>
                <TableCell>
                  <Link
                    to={`/disks/${d.id}`}
                    className="inline-flex items-center gap-2 font-medium text-foreground hover:underline"
                  >
                    <Database className="h-3.5 w-3.5 text-muted-foreground" />
                    {d.name}
                  </Link>
                  <div className="text-xs text-muted-foreground">#{d.id}</div>
                </TableCell>
                <TableCell className="font-mono text-xs">{d.format}</TableCell>
                <TableCell className="text-right tabular-nums">{formatMb(d.size_mb)}</TableCell>
                <TableCell className="text-muted-foreground">
                  <PlacementSummary disk={d} />
                </TableCell>
                <TableCell>
                  {d.backing_image_id ? (
                    <Link
                      to={`/disks/${d.backing_image_id}`}
                      className="text-xs text-muted-foreground hover:underline"
                    >
                      {d.backing_image_name}
                    </Link>
                  ) : (
                    <span className="text-muted-foreground">—</span>
                  )}
                </TableCell>
                <TableCell>
                  <AttachedSummary disk={d} />
                </TableCell>
                <TableCell>
                  {d.ephemeral && (
                    <Badge variant="muted" className="text-[10px]">
                      ephemeral
                    </Badge>
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
