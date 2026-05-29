import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate } from "react-router-dom";
import { ArrowLeft, ChevronDown, ChevronRight, Plus, ServerCog } from "lucide-react";
import { createVm, type VmCreateBody } from "@/api/vms";
import { listNodes, type NodeInfo } from "@/api/nodes";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Textarea } from "@/components/ui/textarea";

interface CheckboxFieldProps {
  id: string;
  label: string;
  hint?: string;
  checked: boolean;
  onChange: (v: boolean) => void;
}

function CheckboxField({ id, label, hint, checked, onChange }: CheckboxFieldProps) {
  return (
    <div className="flex items-start gap-3">
      <input
        id={id}
        type="checkbox"
        checked={checked}
        onChange={(e) => onChange(e.target.checked)}
        className="mt-1 h-4 w-4 rounded border-input"
      />
      <div className="flex-1 space-y-0.5">
        <Label htmlFor={id} className="cursor-pointer">
          {label}
        </Label>
        {hint && <p className="text-xs text-muted-foreground">{hint}</p>}
      </div>
    </div>
  );
}

export default function VmCreate() {
  const navigate = useNavigate();
  const queryClient = useQueryClient();

  // Populate the node dropdown from /api/nodes. Cached on the list
  // page query key so navigating away and back doesn't re-fetch
  // immediately.
  const { data: nodes, isLoading: nodesLoading } = useQuery<NodeInfo[]>({
    queryKey: ["nodes"],
    queryFn: ({ signal }) => listNodes(signal),
    staleTime: 30_000,
  });

  const [name, setName] = useState("");
  const [node, setNode] = useState("");
  const [cpuCount, setCpuCount] = useState<number>(1);
  const [ramMb, setRamMb] = useState<number>(1024);
  const [description, setDescription] = useState("");
  const [headless, setHeadless] = useState(false);
  const [guestAgent, setGuestAgent] = useState(false);
  const [cloudInit, setCloudInit] = useState(false);
  const [autostart, setAutostart] = useState(false);
  const [rebootQuirk, setRebootQuirk] = useState(false);
  const [cpuModel, setCpuModel] = useState("host");
  const [advancedOpen, setAdvancedOpen] = useState(false);

  const mutation = useMutation({
    mutationFn: (body: VmCreateBody) => createVm(body),
    onSuccess: (vm) => {
      queryClient.invalidateQueries({ queryKey: ["vms"] });
      navigate(`/vms/${vm.id}`);
    },
  });

  const onSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (name.trim().length === 0) return;
    mutation.mutate({
      name: name.trim(),
      node: node.trim() || null,
      cpu_count: cpuCount,
      ram_mb: ramMb,
      description: description.trim() || null,
      headless,
      guest_agent: guestAgent,
      cloud_init: cloudInit,
      autostart,
      reboot_quirk: rebootQuirk,
      cpu_model: cpuModel.trim() || "host",
    });
  };

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/vms">
            <ArrowLeft className="h-4 w-4" />
            VMs
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="flex items-center gap-2 text-2xl font-semibold tracking-tight">
            <ServerCog className="h-6 w-6 text-muted-foreground" />
            New VM
          </h1>
        </div>
      </div>

      <form onSubmit={onSubmit} className="space-y-6">
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Basics</CardTitle>
            <CardDescription>
              Creates a bare VM record. Attach drives, NICs, SSH keys, and cloud-init from the
              detail page, or use <code>crv apply</code> for one-shot creation.
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="grid gap-4 md:grid-cols-2">
              <div className="space-y-1.5">
                <Label htmlFor="name">Name</Label>
                <Input
                  id="name"
                  value={name}
                  onChange={(e) => setName(e.target.value)}
                  placeholder="web-1"
                  autoFocus
                  required
                />
              </div>
              <div className="space-y-1.5">
                <Label htmlFor="node">Node</Label>
                <select
                  id="node"
                  value={node}
                  onChange={(e) => setNode(e.target.value)}
                  disabled={nodesLoading}
                  className="flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50"
                >
                  <option value="">— let the scheduler pick —</option>
                  {(nodes ?? []).map((n) => (
                    <option key={n.id} value={n.name} disabled={n.admin_state !== "online"}>
                      {n.name}
                      {n.admin_state !== "online" ? ` (${n.admin_state})` : ""}
                    </option>
                  ))}
                </select>
              </div>
              <div className="space-y-1.5">
                <Label htmlFor="cpu">CPU count</Label>
                <Input
                  id="cpu"
                  type="number"
                  min={1}
                  max={256}
                  value={cpuCount}
                  onChange={(e) => setCpuCount(Math.max(1, Number(e.target.value) || 1))}
                />
              </div>
              <div className="space-y-1.5">
                <Label htmlFor="ram">RAM (MB)</Label>
                <Input
                  id="ram"
                  type="number"
                  min={64}
                  step={64}
                  value={ramMb}
                  onChange={(e) => setRamMb(Math.max(64, Number(e.target.value) || 1024))}
                />
              </div>
            </div>
            <div className="space-y-1.5">
              <Label htmlFor="description">Description (optional)</Label>
              <Textarea
                id="description"
                value={description}
                onChange={(e) => setDescription(e.target.value)}
                rows={2}
              />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="text-base">Boot &amp; integration flags</CardTitle>
          </CardHeader>
          <CardContent className="grid gap-4 md:grid-cols-2">
            <CheckboxField
              id="headless"
              label="Headless"
              hint="No SPICE display device; serial console only. Click to enable in-browser console."
              checked={headless}
              onChange={setHeadless}
            />
            <CheckboxField
              id="qga"
              label="QEMU guest agent"
              hint="VM transitions through `starting` until first QGA ping; needs the qemu-guest-agent package in the guest."
              checked={guestAgent}
              onChange={setGuestAgent}
            />
            <CheckboxField
              id="cloud-init"
              label="Cloud-init"
              hint="Daemon generates a NoCloud ISO with SSH keys + user-data and attaches it as a CD-ROM."
              checked={cloudInit}
              onChange={setCloudInit}
            />
            <CheckboxField
              id="autostart"
              label="Autostart"
              hint="Start automatically when the daemon comes up."
              checked={autostart}
              onChange={setAutostart}
            />
          </CardContent>
        </Card>

        <Card>
          <CardHeader
            onClick={() => setAdvancedOpen((v) => !v)}
            className="cursor-pointer select-none"
          >
            <CardTitle className="flex items-center gap-2 text-base">
              {advancedOpen ? (
                <ChevronDown className="h-4 w-4" />
              ) : (
                <ChevronRight className="h-4 w-4" />
              )}
              Advanced
            </CardTitle>
          </CardHeader>
          {advancedOpen && (
            <CardContent className="space-y-4">
              <div className="space-y-1.5">
                <Label htmlFor="cpu-model">CPU model</Label>
                <Input
                  id="cpu-model"
                  value={cpuModel}
                  onChange={(e) => setCpuModel(e.target.value)}
                  placeholder="host"
                />
                <p className="text-xs text-muted-foreground">
                  QEMU <code>-cpu</code> value. <code>host</code> exposes every host CPU feature;
                  pick a stable name like <code>Westmere</code> for migration between dissimilar
                  nodes.
                </p>
              </div>
              <CheckboxField
                id="reboot-quirk"
                label="Reboot quirk"
                hint="Workaround for guests that don't propagate ACPI reboot to QMP. Off by default."
                checked={rebootQuirk}
                onChange={setRebootQuirk}
              />
            </CardContent>
          )}
        </Card>

        {mutation.error && (
          <Card>
            <CardHeader>
              <CardTitle className="text-base text-destructive">Create failed</CardTitle>
              <CardDescription>{(mutation.error as Error).message}</CardDescription>
            </CardHeader>
          </Card>
        )}

        <div className="flex items-center gap-3">
          <Button type="submit" disabled={mutation.isPending || name.trim().length === 0}>
            <Plus className="h-3.5 w-3.5" />
            {mutation.isPending ? "Creating…" : "Create VM"}
          </Button>
          <Button type="button" variant="ghost" asChild>
            <Link to="/vms">Cancel</Link>
          </Button>
        </div>
      </form>
    </div>
  );
}
