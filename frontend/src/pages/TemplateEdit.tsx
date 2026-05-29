import { useEffect, useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate, useParams } from "react-router-dom";
import Editor from "@monaco-editor/react";
import { ArrowLeft, FileCode2, Save } from "lucide-react";
import { createTemplate, getTemplateYaml, updateTemplate } from "@/api/templates";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";

const SKELETON = `# Template YAML — see doc/templates.md for the full field list.
#
# name: web-server
# description: Debian 12 + nginx, 2 vCPU / 2 GiB.
# cpuCount: 2
# ramMb: 2048
# headless: true
# guestAgent: true
# cloudInit: true
#
# drives:
#   - source: debian-12-cloud   # disk image to clone/overlay
#     strategy: overlay         # clone | overlay | direct | create
#     interface: virtio
#     media: disk
#
# netIfs:
#   - type: managed
#     network: lan
#
# sshKeys:
#   - my-key
#
# cloudInitConfig:
#   userData: |
#     #cloud-config
#     packages:
#       - nginx
#     runcmd:
#       - systemctl enable --now nginx
`;

export default function TemplateEdit() {
  // Same component drives /templates/new (no :id param) and
  // /templates/:id/edit. ``isEdit`` flips a few labels and the
  // route-on-success path; the editor wire is identical.
  const params = useParams<{ id?: string }>();
  const editingId = params.id ? Number(params.id) : null;
  const isEdit = editingId !== null;

  const navigate = useNavigate();
  const queryClient = useQueryClient();

  // In edit mode, fetch the daemon's re-rendered YAML for the existing
  // template. The endpoint mirrors crv template show -o yaml and is
  // kept in lockstep with the Haskell template encoder, so the editor
  // sees exactly what the daemon would emit for an export.
  const { data: existingYaml, isLoading: yamlLoading } = useQuery<{ yaml: string }>({
    queryKey: ["template-yaml", editingId],
    queryFn: ({ signal }) => getTemplateYaml(editingId!, signal),
    enabled: isEdit,
    // Don't keep the previous template's YAML on the page when the id
    // changes — better to show "Loading…" than a stale buffer.
    staleTime: 0,
  });

  const [yaml, setYaml] = useState<string>(isEdit ? "" : SKELETON);
  // Track whether the buffer has been touched. We only pre-fill from
  // the server once; subsequent fetches (e.g. invalidations) shouldn't
  // clobber unsaved edits.
  const [prefilled, setPrefilled] = useState(false);

  useEffect(() => {
    if (isEdit && existingYaml && !prefilled) {
      setYaml(existingYaml.yaml);
      setPrefilled(true);
    }
  }, [existingYaml, isEdit, prefilled]);

  const create = useMutation({
    mutationFn: () => createTemplate(yaml),
    onSuccess: (tmpl) => {
      queryClient.invalidateQueries({ queryKey: ["templates"] });
      navigate(`/templates/${tmpl.id}`);
    },
  });
  const update = useMutation({
    mutationFn: () => updateTemplate(editingId!, yaml),
    onSuccess: (tmpl) => {
      queryClient.invalidateQueries({ queryKey: ["templates"] });
      queryClient.invalidateQueries({ queryKey: ["template", tmpl.id] });
      navigate(`/templates/${tmpl.id}`);
    },
  });

  const submit = () => {
    if (yaml.trim().length === 0) return;
    if (isEdit) update.mutate();
    else create.mutate();
  };
  const mutation = isEdit ? update : create;

  const title = isEdit ? "Edit template" : "New template";

  return (
    <div className="space-y-4">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to={isEdit ? `/templates/${editingId}` : "/templates"}>
            <ArrowLeft className="h-4 w-4" />
            {isEdit ? `Template #${editingId}` : "Templates"}
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="flex items-center gap-2 text-2xl font-semibold tracking-tight">
            <FileCode2 className="h-6 w-6 text-muted-foreground" />
            {title}
          </h1>
        </div>
        <div className="ml-auto">
          <Button
            size="sm"
            onClick={submit}
            disabled={mutation.isPending || yaml.trim().length === 0}
          >
            <Save className="h-3.5 w-3.5" />
            {mutation.isPending
              ? isEdit
                ? "Saving…"
                : "Creating…"
              : isEdit
                ? "Save"
                : "Create template"}
          </Button>
        </div>
      </div>

      {isEdit && (
        <p className="text-xs text-muted-foreground">
          Pre-filled from the daemon. Update replaces the whole template; existing VMs already
          instantiated from it are unaffected.
        </p>
      )}

      <Card>
        <CardContent className="p-0">
          <div className="h-[520px] overflow-hidden rounded-md">
            {isEdit && yamlLoading ? (
              <div className="flex h-full items-center justify-center text-sm text-muted-foreground">
                Loading template…
              </div>
            ) : (
              <Editor
                defaultLanguage="yaml"
                value={yaml}
                onChange={(v) => setYaml(v ?? "")}
                theme="vs-dark"
                options={{
                  minimap: { enabled: false },
                  fontSize: 13,
                  scrollBeyondLastLine: false,
                  tabSize: 2,
                  wordWrap: "on",
                  automaticLayout: true,
                }}
              />
            )}
          </div>
        </CardContent>
      </Card>

      {mutation.error && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base text-destructive">
              {isEdit ? "Update" : "Create"} failed
            </CardTitle>
            <CardDescription>
              The daemon rejected the YAML. Fix the document and retry.
            </CardDescription>
          </CardHeader>
          <CardContent>
            <pre className="overflow-auto rounded-md border border-border bg-destructive/10 p-3 font-mono text-xs text-destructive">
              {(mutation.error as Error).message}
            </pre>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
