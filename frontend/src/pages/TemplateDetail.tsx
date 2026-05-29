import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate, useParams } from "react-router-dom";
import { AlertCircle, ArrowLeft, Pencil, Plus, Trash2 } from "lucide-react";
import {
  deleteTemplate,
  getTemplate,
  instantiateTemplate,
  type TemplateDetails,
} from "@/api/templates";
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

function InstantiateButton({ t }: { t: TemplateDetails }) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: ({ name, node }: { name: string; node?: string }) =>
      instantiateTemplate(t.id, name, node),
    onSuccess: (vm) => {
      queryClient.invalidateQueries({ queryKey: ["vms"] });
      navigate(`/vms/${vm.id}`);
    },
    onError: (e) => window.alert(`Instantiate failed: ${(e as Error).message}`),
  });
  return (
    <Button
      size="sm"
      disabled={mutation.isPending}
      onClick={() => {
        const name = window.prompt(`Name for the new VM (from template "${t.name}"):`);
        if (!name || name.trim().length === 0) return;
        const node = window.prompt(
          `Pin to which node? (leave blank to let the scheduler pick)`,
          "",
        );
        mutation.mutate({
          name: name.trim(),
          node: node && node.trim().length > 0 ? node.trim() : undefined,
        });
      }}
    >
      <Plus className="h-3.5 w-3.5" />
      Instantiate
    </Button>
  );
}

function DeleteButton({ t }: { t: TemplateDetails }) {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const mutation = useMutation({
    mutationFn: () => deleteTemplate(t.id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["templates"] });
      navigate("/templates");
    },
    onError: (e) => window.alert(`Delete failed: ${(e as Error).message}`),
  });
  return (
    <Button
      variant="destructive"
      size="sm"
      disabled={mutation.isPending}
      onClick={() => {
        if (window.confirm(`Delete template "${t.name}"?`)) {
          mutation.mutate();
        }
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Delete
    </Button>
  );
}

export default function TemplateDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);

  const {
    data: t,
    error,
    isLoading,
  } = useQuery<TemplateDetails>({
    queryKey: ["template", id],
    queryFn: ({ signal }) => getTemplate(id, signal),
    refetchInterval: 10000,
    enabled: Number.isFinite(id),
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid template id.</p>;
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
            Failed to load template
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!t) return null;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/templates">
            <ArrowLeft className="h-4 w-4" />
            Templates
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">{t.name}</h1>
          <span className="text-sm text-muted-foreground">#{t.id}</span>
        </div>
      </div>

      <div className="flex flex-wrap gap-2">
        <InstantiateButton t={t} />
        <Button variant="outline" size="sm" asChild>
          <Link to={`/templates/${t.id}/edit`}>
            <Pencil className="h-3.5 w-3.5" />
            Edit
          </Link>
        </Button>
        <DeleteButton t={t} />
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="CPU" value={t.cpu_count} />
          <Field label="RAM" value={formatMb(t.ram_mb)} />
          <Field label="Headless" value={t.headless ? "yes" : "no"} />
          <Field label="Guest agent" value={t.guest_agent ? "yes" : "no"} />
          <Field label="Cloud-init" value={t.cloud_init ? "yes" : "no"} />
          <Field label="Autostart" value={t.autostart ? "yes" : "no"} />
          <Field label="Created" value={new Date(t.created_at).toLocaleString()} />
          {t.description && <Field label="Description" value={t.description} />}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Drives</CardTitle>
        </CardHeader>
        <CardContent className="p-0">
          {t.drives.length === 0 ? (
            <p className="px-6 pb-6 text-sm text-muted-foreground">No drives.</p>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Source</TableHead>
                  <TableHead>Strategy</TableHead>
                  <TableHead>Interface</TableHead>
                  <TableHead>Format</TableHead>
                  <TableHead className="text-right">Size</TableHead>
                  <TableHead>Flags</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {t.drives.map((d, idx) => (
                  <TableRow key={idx}>
                    <TableCell>
                      {d.disk_image_id !== null ? (
                        <Link to={`/disks/${d.disk_image_id}`} className="hover:underline">
                          {d.disk_image_name}
                        </Link>
                      ) : (
                        <span className="text-muted-foreground">create new</span>
                      )}
                    </TableCell>
                    <TableCell>
                      <Badge variant="muted" className="text-[10px]">
                        {d.clone_strategy}
                      </Badge>
                    </TableCell>
                    <TableCell>{d.interface}</TableCell>
                    <TableCell>{d.format ?? "—"}</TableCell>
                    <TableCell className="text-right tabular-nums">{formatMb(d.size_mb)}</TableCell>
                    <TableCell className="space-x-1">
                      {d.read_only && <span className="text-xs text-muted-foreground">RO</span>}
                      {d.discard && <span className="text-xs text-muted-foreground">discard</span>}
                    </TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Network interfaces</CardTitle>
        </CardHeader>
        <CardContent className="p-0">
          {t.net_ifs.length === 0 ? (
            <p className="px-6 pb-6 text-sm text-muted-foreground">No network interfaces.</p>
          ) : (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Type</TableHead>
                  <TableHead>Host device</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {t.net_ifs.map((n, idx) => (
                  <TableRow key={idx}>
                    <TableCell>{n.type}</TableCell>
                    <TableCell className="font-mono text-xs">{n.host_device ?? "—"}</TableCell>
                  </TableRow>
                ))}
              </TableBody>
            </Table>
          )}
        </CardContent>
      </Card>

      {t.ssh_keys.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">SSH keys</CardTitle>
            <CardDescription>Injected into instantiated VMs via cloud-init.</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="flex flex-wrap gap-2">
              {t.ssh_keys.map((k) => (
                <Link key={k.id} to={`/ssh-keys/${k.id}`} className="text-sm hover:underline">
                  {k.name}
                </Link>
              ))}
            </div>
          </CardContent>
        </Card>
      )}

      {t.cloud_init_config && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Cloud-init</CardTitle>
            <CardDescription>
              Applied to each instantiated VM. inject_ssh_keys:{" "}
              {t.cloud_init_config.inject_ssh_keys ? "yes" : "no"}.
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-3">
            {t.cloud_init_config.user_data && (
              <div>
                <div className="mb-1 text-xs uppercase tracking-wide text-muted-foreground">
                  user-data
                </div>
                <pre className="max-h-64 overflow-auto rounded-md border border-border bg-muted/30 p-3 font-mono text-xs">
                  {t.cloud_init_config.user_data}
                </pre>
              </div>
            )}
            {t.cloud_init_config.network_config && (
              <div>
                <div className="mb-1 text-xs uppercase tracking-wide text-muted-foreground">
                  network-config
                </div>
                <pre className="max-h-64 overflow-auto rounded-md border border-border bg-muted/30 p-3 font-mono text-xs">
                  {t.cloud_init_config.network_config}
                </pre>
              </div>
            )}
          </CardContent>
        </Card>
      )}
    </div>
  );
}
