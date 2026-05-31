import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, KeyRound, Plus } from "lucide-react";
import { toast } from "sonner";
import { createSshKey, listSshKeys, type SshKeyInfo } from "@/api/sshKeys";
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

function NewKeyForm() {
  const [name, setName] = useState("");
  const [publicKey, setPublicKey] = useState("");
  const [open, setOpen] = useState(false);
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: () => createSshKey(name.trim(), publicKey.trim()),
    onSuccess: () => {
      setName("");
      setPublicKey("");
      setOpen(false);
      queryClient.invalidateQueries({ queryKey: ["ssh-keys"] });
    },
    onError: (e) => toast.error("Create failed", { description: (e as Error).message }),
  });
  if (!open) {
    return (
      <Button size="sm" onClick={() => setOpen(true)}>
        <Plus className="h-3.5 w-3.5" />
        Add key
      </Button>
    );
  }
  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-base">Add SSH key</CardTitle>
      </CardHeader>
      <CardContent>
        <form
          className="space-y-3"
          onSubmit={(e) => {
            e.preventDefault();
            if (name.trim().length === 0 || publicKey.trim().length === 0) return;
            mutation.mutate();
          }}
        >
          <input
            type="text"
            value={name}
            onChange={(e) => setName(e.target.value)}
            placeholder="key name"
            className="h-9 w-full rounded-md border border-input bg-background px-3 text-sm focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
          />
          <textarea
            value={publicKey}
            onChange={(e) => setPublicKey(e.target.value)}
            placeholder="ssh-ed25519 AAAA… user@host"
            rows={3}
            className="w-full rounded-md border border-input bg-background px-3 py-2 font-mono text-xs focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-ring"
          />
          <div className="flex gap-2">
            <Button
              type="submit"
              size="sm"
              disabled={
                mutation.isPending || name.trim().length === 0 || publicKey.trim().length === 0
              }
            >
              Add key
            </Button>
            <Button type="button" size="sm" variant="ghost" onClick={() => setOpen(false)}>
              Cancel
            </Button>
          </div>
        </form>
      </CardContent>
    </Card>
  );
}

export default function SshKeyList() {
  const { data, error, isLoading } = useQuery<SshKeyInfo[]>({
    queryKey: ["ssh-keys"],
    queryFn: ({ signal }) => listSshKeys(signal),
    refetchInterval: 10000,
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
            Failed to load SSH keys
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const keys = data ?? [];

  return (
    <div className="space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-semibold tracking-tight">SSH keys</h1>
          <p className="text-sm text-muted-foreground">
            {keys.length === 0
              ? "No SSH keys yet."
              : `${keys.length} key${keys.length === 1 ? "" : "s"} registered.`}
          </p>
        </div>
        <NewKeyForm />
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Name</TableHead>
              <TableHead>Public key</TableHead>
              <TableHead>Created</TableHead>
              <TableHead>Attached VMs</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {keys.map((k) => (
              <TableRow key={k.id}>
                <TableCell>
                  <Link
                    to={`/ssh-keys/${k.id}`}
                    className="inline-flex items-center gap-2 font-medium text-foreground hover:underline"
                  >
                    <KeyRound className="h-3.5 w-3.5 text-muted-foreground" />
                    {k.name}
                  </Link>
                  <div className="text-xs text-muted-foreground">#{k.id}</div>
                </TableCell>
                <TableCell className="max-w-md truncate font-mono text-xs text-muted-foreground">
                  {k.public_key}
                </TableCell>
                <TableCell className="text-muted-foreground">
                  {new Date(k.created_at).toLocaleString()}
                </TableCell>
                <TableCell>
                  {k.attached_vms.length === 0 ? (
                    <span className="text-muted-foreground">—</span>
                  ) : (
                    <div className="flex flex-wrap gap-1">
                      {k.attached_vms.map((v) => (
                        <Link
                          key={v.vm.id}
                          to={`/vms/${v.vm.id}`}
                          className="text-xs hover:underline"
                        >
                          {v.vm.name}
                        </Link>
                      ))}
                    </div>
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
