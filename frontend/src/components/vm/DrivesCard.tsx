import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Plus, Trash2 } from "lucide-react";
import { attachDrive, detachDrive, type DriveAttachBody, type VmDetails } from "@/api/vms";
import { listDisks, type DiskImageInfo } from "@/api/disks";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";

const INTERFACES = ["", "virtio", "scsi", "ide", "sata", "floppy"] as const;
const MEDIA = ["", "disk", "cdrom", "floppy"] as const;
const CACHE_TYPES = ["", "none", "writethrough", "writeback", "directsync", "unsafe"] as const;

function selectClass(): string {
  return "flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50";
}

function AttachForm({ vmId, onClose }: { vmId: number; onClose: () => void }) {
  const queryClient = useQueryClient();
  const { data: disks, isLoading: disksLoading } = useQuery<DiskImageInfo[]>({
    queryKey: ["disks"],
    queryFn: ({ signal }) => listDisks(signal),
    staleTime: 10_000,
  });

  const [diskRef, setDiskRef] = useState("");
  const [iface, setIface] = useState<string>("");
  const [media, setMedia] = useState<string>("");
  const [cache, setCache] = useState<string>("");
  const [readOnly, setReadOnly] = useState(false);
  const [discard, setDiscard] = useState(false);

  const mutation = useMutation({
    mutationFn: () => {
      const body: DriveAttachBody = {
        disk_ref: diskRef,
        interface: iface || null,
        media: media || null,
        cache_type: cache || null,
        read_only: readOnly,
        discard,
      };
      return attachDrive(vmId, body);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm", vmId] });
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      onClose();
    },
  });

  return (
    <form
      className="space-y-3 border-t border-border bg-muted/20 p-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (diskRef === "") return;
        mutation.mutate();
      }}
    >
      <div className="grid gap-3 md:grid-cols-3">
        <div className="space-y-1 md:col-span-3">
          <Label htmlFor="drive-disk">Disk</Label>
          <select
            id="drive-disk"
            value={diskRef}
            onChange={(e) => setDiskRef(e.target.value)}
            disabled={disksLoading}
            required
            className={selectClass()}
          >
            <option value="">— pick a disk —</option>
            {(disks ?? []).map((d) => (
              <option key={d.id} value={d.name}>
                {d.name} ({d.format}
                {d.size_mb !== null ? `, ${d.size_mb} MB` : ""})
              </option>
            ))}
          </select>
        </div>
        <div className="space-y-1">
          <Label htmlFor="drive-iface">Interface</Label>
          <select
            id="drive-iface"
            value={iface}
            onChange={(e) => setIface(e.target.value)}
            className={selectClass()}
          >
            {INTERFACES.map((v) => (
              <option key={v} value={v}>
                {v === "" ? "default (virtio)" : v}
              </option>
            ))}
          </select>
        </div>
        <div className="space-y-1">
          <Label htmlFor="drive-media">Media</Label>
          <select
            id="drive-media"
            value={media}
            onChange={(e) => setMedia(e.target.value)}
            className={selectClass()}
          >
            {MEDIA.map((v) => (
              <option key={v} value={v}>
                {v === "" ? "default (disk)" : v}
              </option>
            ))}
          </select>
        </div>
        <div className="space-y-1">
          <Label htmlFor="drive-cache">Cache</Label>
          <select
            id="drive-cache"
            value={cache}
            onChange={(e) => setCache(e.target.value)}
            className={selectClass()}
          >
            {CACHE_TYPES.map((v) => (
              <option key={v} value={v}>
                {v === "" ? "daemon default" : v}
              </option>
            ))}
          </select>
        </div>
      </div>
      <div className="flex flex-wrap gap-4 text-sm">
        <label className="flex items-center gap-2">
          <input
            type="checkbox"
            checked={readOnly}
            onChange={(e) => setReadOnly(e.target.checked)}
            className="h-4 w-4 rounded border-input"
          />
          Read-only
        </label>
        <label className="flex items-center gap-2">
          <input
            type="checkbox"
            checked={discard}
            onChange={(e) => setDiscard(e.target.checked)}
            className="h-4 w-4 rounded border-input"
          />
          discard / TRIM
        </label>
      </div>
      {mutation.error && (
        <pre className="overflow-auto rounded-md border border-border bg-destructive/10 p-2 font-mono text-xs text-destructive">
          {(mutation.error as Error).message}
        </pre>
      )}
      <div className="flex gap-2">
        <Button type="submit" size="sm" disabled={diskRef === "" || mutation.isPending}>
          {mutation.isPending ? "Attaching…" : "Attach"}
        </Button>
        <Button type="button" size="sm" variant="ghost" onClick={onClose}>
          Cancel
        </Button>
      </div>
    </form>
  );
}

function DetachButton({ vmId, driveId }: { vmId: number; driveId: number }) {
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: () => detachDrive(vmId, driveId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm", vmId] });
      queryClient.invalidateQueries({ queryKey: ["disks"] });
    },
    onError: (e) => window.alert(`Detach failed: ${(e as Error).message}`),
  });
  return (
    <Button
      variant="ghost"
      size="sm"
      disabled={mutation.isPending}
      onClick={() => {
        if (window.confirm("Detach this drive?")) mutation.mutate();
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Detach
    </Button>
  );
}

export function DrivesCard({ vm }: { vm: VmDetails }) {
  const [open, setOpen] = useState(false);
  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between">
        <CardTitle className="text-base">Drives</CardTitle>
        {!open && (
          <Button size="sm" onClick={() => setOpen(true)}>
            <Plus className="h-3.5 w-3.5" />
            Attach drive
          </Button>
        )}
      </CardHeader>
      {open && <AttachForm vmId={vm.id} onClose={() => setOpen(false)} />}
      <CardContent className="p-0">
        {vm.drives.length === 0 ? (
          <p className="px-6 pb-6 text-sm text-muted-foreground">No drives attached.</p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Disk</TableHead>
                <TableHead>Interface</TableHead>
                <TableHead>Format</TableHead>
                <TableHead>Media</TableHead>
                <TableHead>Cache</TableHead>
                <TableHead>Flags</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {vm.drives.map((d) => (
                <TableRow key={d.id}>
                  <TableCell>
                    <div className="font-medium">{d.disk_image_name}</div>
                    <div className="text-xs text-muted-foreground">{d.file_path}</div>
                  </TableCell>
                  <TableCell>{d.interface}</TableCell>
                  <TableCell>{d.format}</TableCell>
                  <TableCell>{d.media}</TableCell>
                  <TableCell>{d.cache_type}</TableCell>
                  <TableCell className="space-x-1">
                    {d.read_only && <span className="text-xs text-muted-foreground">RO</span>}
                    {d.discard && <span className="text-xs text-muted-foreground">discard</span>}
                  </TableCell>
                  <TableCell className="text-right">
                    <DetachButton vmId={vm.id} driveId={d.id} />
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        )}
      </CardContent>
    </Card>
  );
}
