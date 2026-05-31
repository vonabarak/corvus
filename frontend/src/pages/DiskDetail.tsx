import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate, useParams } from "react-router-dom";
import { toast } from "sonner";
import {
  AlertCircle,
  ArrowLeft,
  Camera,
  GitMerge,
  Maximize2,
  RotateCcw,
  Trash2,
} from "lucide-react";
import {
  createSnapshot,
  deleteDisk,
  deleteSnapshot,
  getDisk,
  listSnapshots,
  mergeSnapshot,
  resizeDisk,
  rollbackSnapshot,
  type DiskImageInfo,
  type SnapshotInfo,
} from "@/api/disks";
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
import { Badge } from "@/components/ui/badge";
import { formatMb } from "@/lib/format";

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

function ResizeButton({ disk }: { disk: DiskImageInfo }) {
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: (newSizeMb: number) => resizeDisk(disk.id, newSizeMb),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["disk", disk.id] });
      queryClient.invalidateQueries({ queryKey: ["disks"] });
    },
    onError: (e) => toast.error("Resize failed", { description: (e as Error).message }),
  });
  return (
    <Button
      variant="outline"
      size="sm"
      disabled={mutation.isPending}
      onClick={() => {
        const answer = window.prompt(
          `New size for "${disk.name}" in MB (current: ${disk.size_mb ?? "unknown"} MB). Shrinking is not allowed.`,
          disk.size_mb !== null ? String(disk.size_mb) : "",
        );
        if (answer === null) return;
        const newSize = Number(answer);
        if (!Number.isFinite(newSize) || newSize <= 0) {
          toast.error("Please enter a positive integer.");
          return;
        }
        mutation.mutate(newSize);
      }}
    >
      <Maximize2 className="h-3.5 w-3.5" />
      Resize
    </Button>
  );
}

function DeleteDiskButton({ disk }: { disk: DiskImageInfo }) {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const mutation = useMutation({
    mutationFn: () => deleteDisk(disk.id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      navigate("/disks");
    },
    onError: (e) => toast.error("Delete failed", { description: (e as Error).message }),
  });
  const inUse = disk.attached_to.length > 0;
  return (
    <Button
      variant="destructive"
      size="sm"
      disabled={inUse || mutation.isPending}
      title={inUse ? `Attached to ${disk.attached_to.length} VM(s)` : undefined}
      onClick={() => {
        if (window.confirm(`Delete disk "${disk.name}"? This is irreversible.`)) {
          mutation.mutate();
        }
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Delete
    </Button>
  );
}

function SnapshotCreate({ diskId }: { diskId: number }) {
  const [name, setName] = useState("");
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: (snapName: string) => createSnapshot(diskId, snapName),
    onSuccess: () => {
      setName("");
      queryClient.invalidateQueries({ queryKey: ["disk-snapshots", diskId] });
    },
    onError: (e) => toast.error("Snapshot failed", { description: (e as Error).message }),
  });
  return (
    <form
      className="flex items-center gap-2"
      onSubmit={(e) => {
        e.preventDefault();
        if (name.trim().length === 0) return;
        mutation.mutate(name.trim());
      }}
    >
      <input
        type="text"
        value={name}
        onChange={(e) => setName(e.target.value)}
        placeholder="snapshot name"
        className="h-8 rounded-md border border-input bg-background px-3 text-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
      />
      <Button type="submit" size="sm" disabled={mutation.isPending || name.trim().length === 0}>
        <Camera className="h-3.5 w-3.5" />
        Snapshot
      </Button>
    </form>
  );
}

function SnapshotRow({ diskId, snapshot }: { diskId: number; snapshot: SnapshotInfo }) {
  const queryClient = useQueryClient();
  const invalidate = () => {
    queryClient.invalidateQueries({ queryKey: ["disk-snapshots", diskId] });
    queryClient.invalidateQueries({ queryKey: ["disk", diskId] });
  };
  const rollback = useMutation({
    mutationFn: () => rollbackSnapshot(diskId, snapshot.id),
    onSuccess: invalidate,
    onError: (e) => toast.error("Rollback failed", { description: (e as Error).message }),
  });
  const merge = useMutation({
    mutationFn: () => mergeSnapshot(diskId, snapshot.id),
    onSuccess: invalidate,
    onError: (e) => toast.error("Merge failed", { description: (e as Error).message }),
  });
  const del = useMutation({
    mutationFn: () => deleteSnapshot(diskId, snapshot.id),
    onSuccess: invalidate,
    onError: (e) => toast.error("Delete failed", { description: (e as Error).message }),
  });
  const busy = rollback.isPending || merge.isPending || del.isPending;
  return (
    <TableRow>
      <TableCell className="font-medium">{snapshot.name}</TableCell>
      <TableCell className="text-muted-foreground">
        {new Date(snapshot.created_at).toLocaleString()}
      </TableCell>
      <TableCell className="text-right tabular-nums">{formatMb(snapshot.size_mb)}</TableCell>
      <TableCell className="text-right">
        <div className="flex justify-end gap-1">
          <Button
            variant="ghost"
            size="sm"
            disabled={busy}
            onClick={() => {
              if (window.confirm(`Roll back disk to snapshot "${snapshot.name}"?`)) {
                rollback.mutate();
              }
            }}
          >
            <RotateCcw className="h-3.5 w-3.5" />
            Rollback
          </Button>
          <Button
            variant="ghost"
            size="sm"
            disabled={busy}
            onClick={() => {
              if (window.confirm(`Merge snapshot "${snapshot.name}" into its parent?`)) {
                merge.mutate();
              }
            }}
          >
            <GitMerge className="h-3.5 w-3.5" />
            Merge
          </Button>
          <Button
            variant="ghost"
            size="sm"
            disabled={busy}
            onClick={() => {
              if (window.confirm(`Delete snapshot "${snapshot.name}"?`)) {
                del.mutate();
              }
            }}
          >
            <Trash2 className="h-3.5 w-3.5" />
            Delete
          </Button>
        </div>
      </TableCell>
    </TableRow>
  );
}

export default function DiskDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);

  const {
    data: disk,
    error,
    isLoading,
  } = useQuery<DiskImageInfo>({
    queryKey: ["disk", id],
    queryFn: ({ signal }) => getDisk(id, signal),
    refetchInterval: 5000,
    enabled: Number.isFinite(id),
  });

  const { data: snapshots } = useQuery<SnapshotInfo[]>({
    queryKey: ["disk-snapshots", id],
    queryFn: ({ signal }) => listSnapshots(id, signal),
    refetchInterval: 5000,
    enabled: Number.isFinite(id) && !!disk,
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid disk id.</p>;
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
            Failed to load disk
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!disk) return null;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/disks">
            <ArrowLeft className="h-4 w-4" />
            Disks
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">{disk.name}</h1>
          <span className="text-sm text-muted-foreground">#{disk.id}</span>
          {disk.ephemeral && <Badge variant="muted">ephemeral</Badge>}
        </div>
      </div>

      <div className="flex flex-wrap gap-2">
        <ResizeButton disk={disk} />
        <DeleteDiskButton disk={disk} />
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="Format" value={<span className="font-mono">{disk.format}</span>} />
          <Field label="Size" value={formatMb(disk.size_mb)} />
          <Field label="Created" value={new Date(disk.created_at).toLocaleString()} />
          <Field
            label="Backing"
            value={
              disk.backing_image ? (
                <Link to={`/disks/${disk.backing_image.id}`} className="hover:underline">
                  {disk.backing_image.name}
                </Link>
              ) : (
                <span className="text-muted-foreground">—</span>
              )
            }
          />
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Placements</CardTitle>
          <CardDescription>Nodes that hold a copy of this image.</CardDescription>
        </CardHeader>
        <CardContent className="p-0">
          {disk.placements.length === 0 ? (
            <p className="px-6 pb-6 text-sm text-muted-foreground">No placements.</p>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Node</TableHead>
                  <TableHead>Path on node</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {disk.placements.map((p) => (
                  <TableRow key={p.node.id}>
                    <TableCell>{p.node.name}</TableCell>
                    <TableCell className="font-mono text-xs">{p.file_path}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Attached to</CardTitle>
        </CardHeader>
        <CardContent className="p-0">
          {disk.attached_to.length === 0 ? (
            <p className="px-6 pb-6 text-sm text-muted-foreground">Not attached to any VM.</p>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>VM</TableHead>
                  <TableHead className="text-right">ID</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {disk.attached_to.map((a) => (
                  <TableRow key={a.vm.id}>
                    <TableCell>
                      <Link to={`/vms/${a.vm.id}`} className="hover:underline">
                        {a.vm.name}
                      </Link>
                    </TableCell>
                    <TableCell className="text-right text-muted-foreground">#{a.vm.id}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="flex flex-row items-center justify-between">
          <div>
            <CardTitle className="text-base">Snapshots</CardTitle>
            <CardDescription>qcow2 snapshots stored inside this disk.</CardDescription>
          </div>
          <SnapshotCreate diskId={disk.id} />
        </CardHeader>
        <CardContent className="p-0">
          {(snapshots ?? []).length === 0 ? (
            <p className="px-6 pb-6 text-sm text-muted-foreground">No snapshots.</p>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Name</TableHead>
                  <TableHead>Created</TableHead>
                  <TableHead className="text-right">Size</TableHead>
                  <TableHead className="text-right">Actions</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {(snapshots ?? []).map((s) => (
                  <SnapshotRow key={s.id} diskId={disk.id} snapshot={s} />
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
