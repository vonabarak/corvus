import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate, useParams } from "react-router-dom";
import { toast } from "sonner";
import {
  AlertCircle,
  ArrowLeft,
  Pause,
  Play,
  RotateCcw,
  Save,
  Square,
  Terminal as TerminalIcon,
  Trash2,
} from "lucide-react";
import { deleteVm, getVm, vmAction, type VmAction, type VmDetails } from "@/api/vms";
import { getVmCloudInit, type CloudInitInfo } from "@/api/templates";
import { useWebSocketJson } from "@/hooks/useWebSocketJson";
import { Badge } from "@/components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { VmStatusBadge } from "@/components/VmStatusBadge";
import { DrivesCard } from "@/components/vm/DrivesCard";
import { NetIfsCard } from "@/components/vm/NetIfsCard";
import { ResourceUsageCard } from "@/components/vm/ResourceUsageCard";
import { SshKeysCard } from "@/components/vm/SshKeysCard";

interface GuestAgentFrame {
  vm_id: number;
  enabled: boolean;
  reachable: boolean;
  last_healthcheck: string | null;
  message: string | null;
}

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

/** Map a VM's current status to the lifecycle actions that are
 * legal from that state. Mirrors `validateTransition` in
 * src/Corvus/Model/VmState.hs exactly — the daemon is authoritative,
 * the frontend just disables buttons that would 4xx so the user
 * doesn't have to learn the rules by trial and error.
 *
 * Quick reference (Ctrl+Alt+Del rides along with anything that can
 * pause/save — it makes sense to send it whenever the guest is
 * actually running):
 *   stopped  → start
 *   starting → stop, reset
 *   running  → stop, pause, reset, save, send-ctrl-alt-del
 *   stopping → reset
 *   paused   → start (resume), reset, save
 *   saved    → start (resume), reset
 *   error    → reset
 */
function allowedActions(status: string): VmAction[] {
  switch (status) {
    case "stopped":
      return ["start"];
    case "starting":
      return ["stop", "reset"];
    case "running":
      return ["stop", "pause", "reset", "save", "send-ctrl-alt-del"];
    case "stopping":
      return ["reset"];
    case "paused":
      return ["start", "reset", "save"];
    case "saved":
      return ["start", "reset"];
    case "error":
      return ["reset"];
    default:
      return [];
  }
}

interface ActionButtonProps {
  vmId: number;
  status: string;
  action: VmAction;
  icon: React.ReactNode;
  label: string;
  variant?: "default" | "destructive" | "outline";
}

function ActionButton({ vmId, status, action, icon, label, variant }: ActionButtonProps) {
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: () => vmAction(vmId, action),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm", vmId] });
      queryClient.invalidateQueries({ queryKey: ["vms"] });
    },
    onError: (e) => toast.error(`${label} failed`, { description: (e as Error).message }),
  });
  const disabled = !allowedActions(status).includes(action) || mutation.isPending;
  return (
    <Button
      variant={variant ?? "outline"}
      size="sm"
      onClick={() => mutation.mutate()}
      disabled={disabled}
      title={mutation.error ? (mutation.error as Error).message : undefined}
    >
      {icon}
      {label}
    </Button>
  );
}

function DeleteButton({ vm }: { vm: VmDetails }) {
  const queryClient = useQueryClient();
  const navigate = useNavigate();
  const mutation = useMutation({
    mutationFn: () => deleteVm(vm.id),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vms"] });
      navigate("/vms");
    },
    onError: (e) => toast.error("Delete failed", { description: (e as Error).message }),
  });
  const disabled = vm.status !== "stopped" || mutation.isPending;
  return (
    <Button
      variant="destructive"
      size="sm"
      disabled={disabled}
      onClick={() => {
        if (window.confirm(`Delete VM "${vm.name}"? This also reaps ephemeral disks.`)) {
          mutation.mutate();
        }
      }}
      title={vm.status !== "stopped" ? "Stop the VM first" : undefined}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Delete
    </Button>
  );
}

export default function VmDetail() {
  const params = useParams<{ id: string }>();
  const id = Number(params.id);
  const {
    data: vm,
    error,
    isLoading,
  } = useQuery<VmDetails>({
    queryKey: ["vm", id],
    queryFn: ({ signal }) => getVm(id, signal),
    refetchInterval: 5000,
    enabled: Number.isFinite(id),
  });

  // Live guest-agent reachability. The daemon's poller pushes a
  // GuestAgentStatus every poll cycle (~5 s) to each subscriber; we
  // overlay it on the static VM record so the badge updates instantly
  // without waiting for the next /api/vms/{id} poll.
  const guestAgentEnabled = Number.isFinite(id) && !!vm?.guest_agent;
  const { last: guestAgent } = useWebSocketJson<GuestAgentFrame>(
    `/api/vms/${id}/guest-agent/ws`,
    guestAgentEnabled,
  );

  // Cloud-init read is per-VM and only meaningful when the VM has
  // cloud-init enabled. The detail endpoint returns ``has_user_data``
  // / ``has_network_config`` so the panel can decide whether to render.
  const { data: cloudInit } = useQuery<CloudInitInfo>({
    queryKey: ["vm-cloud-init", id],
    queryFn: ({ signal }) => getVmCloudInit(id, signal),
    enabled: Number.isFinite(id) && !!vm?.cloud_init,
  });

  if (!Number.isFinite(id)) {
    return <p className="text-destructive">Invalid VM id.</p>;
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
            Failed to load VM
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  if (!vm) return null;

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
          <h1 className="text-2xl font-semibold tracking-tight">{vm.name}</h1>
          <span className="text-sm text-muted-foreground">#{vm.id}</span>
          <VmStatusBadge status={vm.status} />
          {vm.guest_agent && (
            <Badge variant={guestAgent?.reachable ? "success" : "muted"}>
              QGA: {guestAgent ? (guestAgent.reachable ? "reachable" : "unreachable") : "…"}
            </Badge>
          )}
        </div>
      </div>

      <div className="flex flex-wrap gap-2">
        <ActionButton
          vmId={vm.id}
          status={vm.status}
          action="start"
          icon={<Play className="h-3.5 w-3.5" />}
          label="Start"
        />
        <ActionButton
          vmId={vm.id}
          status={vm.status}
          action="stop"
          icon={<Square className="h-3.5 w-3.5" />}
          label="Stop"
        />
        <ActionButton
          vmId={vm.id}
          status={vm.status}
          action="pause"
          icon={<Pause className="h-3.5 w-3.5" />}
          label="Pause"
        />
        <ActionButton
          vmId={vm.id}
          status={vm.status}
          action="reset"
          icon={<RotateCcw className="h-3.5 w-3.5" />}
          label="Reset"
        />
        <ActionButton
          vmId={vm.id}
          status={vm.status}
          action="save"
          icon={<Save className="h-3.5 w-3.5" />}
          label="Save"
        />
        {/* Console kind tracks the VM's display mode: headless VMs
            get the xterm.js serial console, graphical VMs get the
            spice-html5 page. Both are disabled while the VM is
            stopped — neither makes sense before QEMU launches. */}
        <Button variant="outline" size="sm" asChild disabled={vm.status === "stopped"}>
          <Link to={vm.headless ? `/vms/${vm.id}/console` : `/vms/${vm.id}/spice`}>
            <TerminalIcon className="h-3.5 w-3.5" />
            {vm.headless ? "Serial console" : "Graphical console"}
          </Link>
        </Button>
        <DeleteButton vm={vm} />
      </div>

      <Card>
        <CardHeader>
          <CardTitle className="text-base">Overview</CardTitle>
        </CardHeader>
        <CardContent className="grid grid-cols-2 gap-4 md:grid-cols-4">
          <Field label="Node" value={vm.node.name} />
          <Field label="CPU" value={`${vm.cpu_count} × ${vm.cpu_model}`} />
          <Field label="RAM" value={`${vm.ram_mb} MB`} />
          <Field label="Created" value={new Date(vm.created_at).toLocaleString()} />
          {vm.description && <Field label="Description" value={vm.description} />}
          {vm.spice_port !== null && <Field label="SPICE port" value={vm.spice_port} />}
          {vm.vsock_cid !== null && <Field label="vsock CID" value={vm.vsock_cid} />}
          {vm.last_healthcheck && (
            <Field
              label="Last healthcheck"
              value={new Date(vm.last_healthcheck).toLocaleString()}
            />
          )}
          {vm.error_message && (
            <Field
              label="Error"
              value={<span className="text-destructive">{vm.error_message}</span>}
            />
          )}
        </CardContent>
      </Card>

      {vm.status === "running" && <ResourceUsageCard vmId={vm.id} vmCpuCount={vm.cpu_count} />}

      <DrivesCard vm={vm} />

      <NetIfsCard vm={vm} />

      <SshKeysCard vmId={vm.id} />

      {vm.cloud_init && cloudInit && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Cloud-init</CardTitle>
            <CardDescription>
              Effective config the VM boots with. inject_ssh_keys:{" "}
              {cloudInit.inject_ssh_keys ? "yes" : "no"}.
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-3">
            {cloudInit.user_data && (
              <div>
                <div className="mb-1 text-xs uppercase tracking-wide text-muted-foreground">
                  user-data
                </div>
                <pre className="max-h-64 overflow-auto rounded-md border border-border bg-muted/30 p-3 font-mono text-xs">
                  {cloudInit.user_data}
                </pre>
              </div>
            )}
            {cloudInit.network_config && (
              <div>
                <div className="mb-1 text-xs uppercase tracking-wide text-muted-foreground">
                  network-config
                </div>
                <pre className="max-h-64 overflow-auto rounded-md border border-border bg-muted/30 p-3 font-mono text-xs">
                  {cloudInit.network_config}
                </pre>
              </div>
            )}
            {!cloudInit.user_data && !cloudInit.network_config && (
              <p className="text-sm text-muted-foreground">
                Default cloud-init (SSH key injection only).
              </p>
            )}
          </CardContent>
        </Card>
      )}
    </div>
  );
}
