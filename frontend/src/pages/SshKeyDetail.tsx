import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate, useParams } from "react-router-dom";
import { AlertCircle, ArrowLeft, Trash2 } from "lucide-react";
import { toast } from "sonner";
import { deleteSshKey, getSshKey, type SshKeyInfo } from "@/api/sshKeys";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";

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

function DeleteButton({ k }: { k: SshKeyInfo }) {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const mutation = useMutation({
    mutationFn: () => deleteSshKey(k.id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["ssh-keys"] });
      navigate("/ssh-keys");
    },
    onError: (e) => toast.error("Delete failed", { description: (e as Error).message }),
  });
  const inUse = k.attached_vms.length > 0;
  return (
    <Button
      variant="destructive"
      size="sm"
      disabled={inUse || mutation.isPending}
      title={inUse ? `Attached to ${k.attached_vms.length} VM(s)` : undefined}
      onClick={() => {
        if (window.confirm(`Delete SSH key "${k.name}"?`)) {
          mutation.mutate();
        }
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Delete
    </Button>
  );
}

export default function SshKeyDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);

  const {
    data: key,
    error,
    isLoading,
  } = useQuery<SshKeyInfo>({
    queryKey: ["ssh-key", id],
    queryFn: ({ signal }) => getSshKey(id, signal),
    refetchInterval: 10000,
    enabled: Number.isFinite(id),
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid SSH key id.</p>;
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
            Failed to load SSH key
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!key) return null;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/ssh-keys">
            <ArrowLeft className="h-4 w-4" />
            SSH keys
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">{key.name}</h1>
          <span className="text-sm text-muted-foreground">#{key.id}</span>
        </div>
      </div>

      <div className="flex flex-wrap gap-2">
        <DeleteButton k={key} />
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-1 gap-4 md:grid-cols-2">
          <Field label="Created" value={new Date(key.created_at).toLocaleString()} />
          <Field
            label="Attached VMs"
            value={
              key.attached_vms.length === 0 ? (
                <span className="text-muted-foreground">none</span>
              ) : (
                <div className="flex flex-wrap gap-1">
                  {key.attached_vms.map((v) => (
                    <Link key={v.vm_id} to={`/vms/${v.vm_id}`} className="hover:underline">
                      {v.vm_name}
                    </Link>
                  ))}
                </div>
              )
            }
          />
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Public key</CardTitle>
          <CardDescription>
            The text injected into <code>authorized_keys</code> for VMs this key is attached to.
          </CardDescription>
        </CardHeader>
        <CardContent>
          <pre className="overflow-x-auto rounded-md border border-border bg-muted/30 p-3 font-mono text-xs">
            {key.public_key}
          </pre>
        </CardContent>
      </Card>
    </div>
  );
}
