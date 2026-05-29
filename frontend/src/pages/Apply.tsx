import { useState } from "react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import Editor from "@monaco-editor/react";
import { CheckCircle2, FileCode2, Play } from "lucide-react";
import { runApply, type ApplyCreated, type ApplyResponse } from "@/api/apply";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";

const PLACEHOLDER_YAML = `# crv apply pipeline. See doc/apply-configuration.md for the full schema.
#
# sshKeys:
#   - name: my-key
#     publicKey: ssh-ed25519 AAAA... user@host
#
# disks:
#   - name: web-root
#     sizeMb: 4096
#     format: qcow2
#
# networks:
#   - name: lan
#     subnet: 10.10.0.0/24
#     dhcp: true
#     nat: true
#
# vms:
#   - name: web-1
#     cpuCount: 2
#     ramMb: 1024
#     drives:
#       - disk: web-root
#     netIfs:
#       - network: lan
#     sshKeys: [my-key]
`;

interface CreatedGroupProps {
  label: string;
  route: string;
  items: ApplyCreated[];
}

function CreatedGroup({ label, route, items }: CreatedGroupProps) {
  if (items.length === 0) return null;
  return (
    <div>
      <div className="mb-1 text-xs uppercase tracking-wide text-muted-foreground">
        {label} ({items.length})
      </div>
      <div className="flex flex-wrap gap-2">
        {items.map((item) => (
          <Link
            key={item.id}
            to={`${route}/${item.id}`}
            className="inline-flex items-center gap-1 rounded-md border border-border bg-card px-2 py-1 text-xs hover:bg-accent"
          >
            <span className="font-medium">{item.name}</span>
            <span className="text-muted-foreground">#{item.id}</span>
          </Link>
        ))}
      </div>
    </div>
  );
}

function ResultPanel({ response }: { response: ApplyResponse }) {
  const { result, task_id } = response;
  const totalCreated =
    result.ssh_keys.length +
    result.disks.length +
    result.networks.length +
    result.vms.length +
    result.templates.length;
  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2 text-base">
          <CheckCircle2 className="h-5 w-5 text-emerald-400" />
          Apply succeeded
        </CardTitle>
        <CardDescription>
          {totalCreated === 0 ? (
            <>No new resources — everything already existed.</>
          ) : (
            <>
              {totalCreated} resource{totalCreated === 1 ? "" : "s"} created.{" "}
            </>
          )}
          Task{" "}
          <Link to={`/tasks/${task_id}`} className="font-mono text-foreground hover:underline">
            #{task_id}
          </Link>
          .
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-3">
        <CreatedGroup label="SSH keys" route="/ssh-keys" items={result.ssh_keys} />
        <CreatedGroup label="Disks" route="/disks" items={result.disks} />
        <CreatedGroup label="Networks" route="/networks" items={result.networks} />
        <CreatedGroup label="VMs" route="/vms" items={result.vms} />
        <CreatedGroup label="Templates" route="/templates" items={result.templates} />
      </CardContent>
    </Card>
  );
}

export default function Apply() {
  const [yaml, setYaml] = useState<string>(PLACEHOLDER_YAML);
  const [skipExisting, setSkipExisting] = useState<boolean>(false);
  const [response, setResponse] = useState<ApplyResponse | null>(null);
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: () => runApply(yaml, skipExisting),
    onSuccess: (resp) => {
      setResponse(resp);
      // Anything could have been created — invalidate every list.
      queryClient.invalidateQueries({ queryKey: ["vms"] });
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      queryClient.invalidateQueries({ queryKey: ["networks"] });
      queryClient.invalidateQueries({ queryKey: ["ssh-keys"] });
      queryClient.invalidateQueries({ queryKey: ["templates"] });
      queryClient.invalidateQueries({ queryKey: ["tasks"] });
    },
  });

  return (
    <div className="space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="flex items-center gap-2 text-2xl font-semibold tracking-tight">
            <FileCode2 className="h-6 w-6 text-muted-foreground" />
            Apply
          </h1>
          <p className="text-sm text-muted-foreground">
            Declarative environment YAML. Same schema as <code>crv apply</code> — see{" "}
            <code>doc/apply-configuration.md</code> for the field list.
          </p>
        </div>
        <div className="flex items-center gap-3">
          <label className="flex items-center gap-2 text-sm">
            <input
              type="checkbox"
              checked={skipExisting}
              onChange={(e) => setSkipExisting(e.target.checked)}
              className="h-4 w-4 rounded border-input"
            />
            <span>Skip existing</span>
          </label>
          <Button
            size="sm"
            onClick={() => {
              setResponse(null);
              mutation.mutate();
            }}
            disabled={mutation.isPending || yaml.trim().length === 0}
          >
            <Play className="h-3.5 w-3.5" />
            {mutation.isPending ? "Applying…" : "Apply"}
          </Button>
        </div>
      </div>

      <Card>
        <CardContent className="p-0">
          <div className="h-[480px] overflow-hidden rounded-md">
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
          </div>
        </CardContent>
      </Card>

      {mutation.error && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base text-destructive">Apply failed</CardTitle>
            <CardDescription>
              The daemon rejected the pipeline. Fix the YAML and retry. With{" "}
              <Badge variant="muted">Skip existing</Badge> enabled, duplicates by name are ignored.
            </CardDescription>
          </CardHeader>
          <CardContent>
            <pre className="overflow-auto rounded-md border border-border bg-destructive/10 p-3 font-mono text-xs text-destructive">
              {(mutation.error as Error).message}
            </pre>
          </CardContent>
        </Card>
      )}

      {response && <ResultPanel response={response} />}
    </div>
  );
}
