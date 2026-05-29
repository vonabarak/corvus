import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { Plus, Trash2 } from "lucide-react";
import { addNetIf, removeNetIf, type NetIfAddBody, type VmDetails } from "@/api/vms";
import { listNetworks, type NetworkInfo } from "@/api/networks";
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
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";

// Mirrors NetInterfaceType in src/Corvus/Model.hs: user, tap, bridge,
// macvtap, managed. Default `user` is QEMU's SLIRP — no host setup.
const TYPES = ["user", "managed", "tap", "bridge", "macvtap"] as const;

function selectClass(): string {
  return "flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50";
}

function AddForm({ vmId, onClose }: { vmId: number; onClose: () => void }) {
  const queryClient = useQueryClient();
  const [type, setType] = useState<(typeof TYPES)[number]>("user");
  const [networkRef, setNetworkRef] = useState("");
  const [hostDevice, setHostDevice] = useState("");
  const [macAddress, setMacAddress] = useState("");

  // Only fetch networks when the user picks `managed` — saves a round-
  // trip for the much more common `user` path.
  const { data: networks } = useQuery<NetworkInfo[]>({
    queryKey: ["networks"],
    queryFn: ({ signal }) => listNetworks(signal),
    enabled: type === "managed",
    staleTime: 10_000,
  });

  const mutation = useMutation({
    mutationFn: () => {
      const body: NetIfAddBody = {
        type,
        network_ref: type === "managed" ? networkRef || null : null,
        host_device:
          type === "tap" || type === "bridge" || type === "macvtap" ? hostDevice || null : null,
        mac_address: macAddress || null,
      };
      return addNetIf(vmId, body);
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm", vmId] });
      onClose();
    },
  });

  const needsNetwork = type === "managed";
  const needsHostDevice = type === "tap" || type === "bridge" || type === "macvtap";
  const submitDisabled =
    mutation.isPending ||
    (needsNetwork && networkRef === "") ||
    (needsHostDevice && hostDevice.trim() === "");

  return (
    <form
      className="space-y-3 border-t border-border bg-muted/20 p-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (submitDisabled) return;
        mutation.mutate();
      }}
    >
      <div className="grid gap-3 md:grid-cols-2">
        <div className="space-y-1">
          <Label htmlFor="nic-type">Type</Label>
          <select
            id="nic-type"
            value={type}
            onChange={(e) => setType(e.target.value as (typeof TYPES)[number])}
            className={selectClass()}
          >
            {TYPES.map((t) => (
              <option key={t} value={t}>
                {t}
              </option>
            ))}
          </select>
        </div>
        {needsNetwork && (
          <div className="space-y-1">
            <Label htmlFor="nic-network">Network</Label>
            <select
              id="nic-network"
              value={networkRef}
              onChange={(e) => setNetworkRef(e.target.value)}
              required
              className={selectClass()}
            >
              <option value="">— pick a managed network —</option>
              {(networks ?? []).map((n) => (
                <option key={n.id} value={n.name}>
                  {n.name} ({n.subnet})
                </option>
              ))}
            </select>
            {(networks ?? []).length === 0 && (
              <p className="text-xs text-muted-foreground">
                No managed networks yet —{" "}
                <Link to="/networks/new" className="underline">
                  create one
                </Link>
                .
              </p>
            )}
          </div>
        )}
        {needsHostDevice && (
          <div className="space-y-1">
            <Label htmlFor="nic-host">Host device</Label>
            <Input
              id="nic-host"
              value={hostDevice}
              onChange={(e) => setHostDevice(e.target.value)}
              placeholder={type === "bridge" ? "br0" : "tap0"}
              required
            />
          </div>
        )}
        <div className="space-y-1">
          <Label htmlFor="nic-mac">MAC (optional)</Label>
          <Input
            id="nic-mac"
            value={macAddress}
            onChange={(e) => setMacAddress(e.target.value)}
            placeholder="auto-generated"
            className="font-mono text-xs"
          />
        </div>
      </div>
      {mutation.error && (
        <pre className="overflow-auto rounded-md border border-border bg-destructive/10 p-2 font-mono text-xs text-destructive">
          {(mutation.error as Error).message}
        </pre>
      )}
      <div className="flex gap-2">
        <Button type="submit" size="sm" disabled={submitDisabled}>
          {mutation.isPending ? "Adding…" : "Add"}
        </Button>
        <Button type="button" size="sm" variant="ghost" onClick={onClose}>
          Cancel
        </Button>
      </div>
    </form>
  );
}

function RemoveButton({ vmId, netIfId }: { vmId: number; netIfId: number }) {
  const queryClient = useQueryClient();
  const mutation = useMutation({
    mutationFn: () => removeNetIf(vmId, netIfId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["vm", vmId] });
    },
    onError: (e) => window.alert(`Remove failed: ${(e as Error).message}`),
  });
  return (
    <Button
      variant="ghost"
      size="sm"
      disabled={mutation.isPending}
      onClick={() => {
        if (window.confirm("Remove this network interface?")) mutation.mutate();
      }}
    >
      <Trash2 className="h-3.5 w-3.5" />
      Remove
    </Button>
  );
}

export function NetIfsCard({ vm }: { vm: VmDetails }) {
  const [open, setOpen] = useState(false);
  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between">
        <CardTitle className="text-base">Network interfaces</CardTitle>
        {!open && (
          <Button size="sm" onClick={() => setOpen(true)}>
            <Plus className="h-3.5 w-3.5" />
            Add interface
          </Button>
        )}
      </CardHeader>
      {open && <AddForm vmId={vm.id} onClose={() => setOpen(false)} />}
      <CardContent className="p-0">
        {vm.net_ifs.length === 0 ? (
          <p className="px-6 pb-6 text-sm text-muted-foreground">No network interfaces.</p>
        ) : (
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead>Type</TableHead>
                <TableHead>Network</TableHead>
                <TableHead>MAC</TableHead>
                <TableHead>Host device</TableHead>
                <TableHead>Guest IPs</TableHead>
                <TableHead className="text-right">Actions</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {vm.net_ifs.map((n) => (
                <TableRow key={n.id}>
                  <TableCell>{n.type}</TableCell>
                  <TableCell>{n.network_name ?? "—"}</TableCell>
                  <TableCell className="font-mono text-xs">{n.mac_address}</TableCell>
                  <TableCell className="font-mono text-xs">{n.host_device || "—"}</TableCell>
                  <TableCell className="font-mono text-xs">{n.guest_ip_addresses ?? "—"}</TableCell>
                  <TableCell className="text-right">
                    <RemoveButton vmId={vm.id} netIfId={n.id} />
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
