import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { Plus, Trash2 } from "lucide-react";
import { attachVmSshKey, detachVmSshKey, listVmSshKeys, type VmSshKey } from "@/api/vms";
import { listSshKeys, type SshKeyInfo } from "@/api/sshKeys";
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
import { Label } from "@/components/ui/label";

function selectClass(): string {
  return "flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50";
}

function AttachForm({
  vmId,
  attachedIds,
  onClose,
}: {
  vmId: number;
  attachedIds: Set<number>;
  onClose: () => void;
}) {
  const queryClient = useQueryClient();
  const { data: allKeys, isLoading } = useQuery<SshKeyInfo[]>({
    queryKey: ["ssh-keys"],
    queryFn: ({ signal }) => listSshKeys(signal),
    staleTime: 10_000,
  });
  const [keyRef, setKeyRef] = useState("");

  const mutation = useMutation({
    mutationFn: () => attachVmSshKey(vmId, keyRef),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm-ssh-keys", vmId] });
      onClose();
    },
  });

  // Only show keys the VM doesn't already have.
  const candidates = (allKeys ?? []).filter((k) => !attachedIds.has(k.id));

  return (
    <form
      className="space-y-3 border-t border-border bg-muted/20 p-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (keyRef === "") return;
        mutation.mutate();
      }}
    >
      <div className="space-y-1">
        <Label htmlFor="ssh-key-ref">SSH key</Label>
        <select
          id="ssh-key-ref"
          value={keyRef}
          onChange={(e) => setKeyRef(e.target.value)}
          disabled={isLoading}
          required
          className={selectClass()}
        >
          <option value="">— pick a key —</option>
          {candidates.map((k) => (
            <option key={k.id} value={k.name}>
              {k.name}
            </option>
          ))}
        </select>
        {candidates.length === 0 && !isLoading && (
          <p className="text-xs text-muted-foreground">
            No unattached keys —{" "}
            <Link to="/ssh-keys" className="underline">
              add one
            </Link>
            .
          </p>
        )}
      </div>
      {mutation.error && (
        <pre className="overflow-auto rounded-md border border-border bg-destructive/10 p-2 font-mono text-xs text-destructive">
          {(mutation.error as Error).message}
        </pre>
      )}
      <div className="flex gap-2">
        <Button type="submit" size="sm" disabled={keyRef === "" || mutation.isPending}>
          {mutation.isPending ? "Attaching…" : "Attach"}
        </Button>
        <Button type="button" size="sm" variant="ghost" onClick={onClose}>
          Cancel
        </Button>
      </div>
    </form>
  );
}

function DetachButton({ vmId, keyName }: { vmId: number; keyName: string }) {
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: () => detachVmSshKey(vmId, keyName),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm-ssh-keys", vmId] });
    },
    onError: (e) => window.alert(`Detach failed: ${(e as Error).message}`),
  });
  return (
    <Button
      variant="ghost"
      size="sm"
      disabled={mutation.isPending}
      onClick={() => {
        if (window.confirm(`Detach SSH key "${keyName}"?`)) mutation.mutate();
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Detach
    </Button>
  );
}

export function SshKeysCard({ vmId }: { vmId: number }) {
  const [open, setOpen] = useState(false);
  const { data: keys, isLoading } = useQuery<VmSshKey[]>({
    queryKey: ["vm-ssh-keys", vmId],
    queryFn: ({ signal }) => listVmSshKeys(vmId, signal),
    refetchInterval: 30_000,
  });

  if (isLoading) return null;
  const attachedIds = new Set((keys ?? []).map((k) => k.id));

  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between">
        <div>
          <CardTitle className="text-base">SSH keys</CardTitle>
          <CardDescription>
            Injected into <code>authorized_keys</code> by cloud-init at boot.
          </CardDescription>
        </div>
        {!open && (
          <Button size="sm" onClick={() => setOpen(true)}>
            <Plus className="h-3.5 w-3.5" />
            Attach key
          </Button>
        )}
      </CardHeader>
      {open && <AttachForm vmId={vmId} attachedIds={attachedIds} onClose={() => setOpen(false)} />}
      <CardContent className="p-0">
        {(keys ?? []).length === 0 ? (
          <p className="px-6 pb-6 text-sm text-muted-foreground">No SSH keys attached.</p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Name</TableHead>
                <TableHead>Public key</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {(keys ?? []).map((k) => (
                <TableRow key={k.id}>
                  <TableCell>
                    <Link to={`/ssh-keys/${k.id}`} className="font-medium hover:underline">
                      {k.name}
                    </Link>
                  </TableCell>
                  <TableCell className="max-w-md truncate font-mono text-xs text-muted-foreground">
                    {k.public_key}
                  </TableCell>
                  <TableCell className="text-right">
                    <DetachButton vmId={vmId} keyName={k.name} />
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
