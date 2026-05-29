import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate } from "react-router-dom";
import { ArrowLeft, Network as NetIcon, Plus } from "lucide-react";
import { createNetwork } from "@/api/networks";
import { listNodes, type NodeInfo } from "@/api/nodes";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";

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

// Loose CIDR sanity check. Accepts e.g. 10.10.0.0/24 and 2001:db8::/64.
// The daemon does the authoritative validation; this is just a cheap
// "before you submit" guard so we don't fire off an obviously bad RPC.
function looksLikeCidr(s: string): boolean {
  const trimmed = s.trim();
  if (trimmed.length === 0) return false;
  const slash = trimmed.indexOf("/");
  if (slash < 0) return false;
  const addr = trimmed.slice(0, slash);
  const prefix = Number(trimmed.slice(slash + 1));
  if (!Number.isFinite(prefix) || prefix < 0 || prefix > 128) return false;
  // Allow IPv4 ("a.b.c.d") or IPv6 (contains a colon); we don't try to
  // parse octets — the daemon will fail-loud on garbage.
  return addr.includes(".") || addr.includes(":");
}

export default function NetworkCreate() {
  const navigate = useNavigate();
  const queryClient = useQueryClient();

  const { data: nodes, isLoading: nodesLoading } = useQuery<NodeInfo[]>({
    queryKey: ["nodes"],
    queryFn: ({ signal }) => listNodes(signal),
    staleTime: 30_000,
  });

  const [name, setName] = useState("");
  const [subnet, setSubnet] = useState("10.10.0.0/24");
  const [node, setNode] = useState("");
  const [dhcp, setDhcp] = useState(true);
  const [nat, setNat] = useState(true);
  const [autostart, setAutostart] = useState(false);

  const mutation = useMutation({
    mutationFn: () =>
      createNetwork({
        name: name.trim(),
        subnet: subnet.trim(),
        node: node || null,
        dhcp,
        nat,
        autostart,
      }),
    onSuccess: (net) => {
      queryClient.invalidateQueries({ queryKey: ["networks"] });
      navigate(`/networks/${net.id}`);
    },
  });

  const subnetOk = looksLikeCidr(subnet);
  const submitDisabled = name.trim().length === 0 || !subnetOk || mutation.isPending;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/networks">
            <ArrowLeft className="h-4 w-4" />
            Networks
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="flex items-center gap-2 text-2xl font-semibold tracking-tight">
            <NetIcon className="h-6 w-6 text-muted-foreground" />
            New network
          </h1>
        </div>
      </div>

      <form
        onSubmit={(e) => {
          e.preventDefault();
          if (submitDisabled) return;
          mutation.mutate();
        }}
        className="space-y-6"
      >
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Basics</CardTitle>
            <CardDescription>
              The bridge + dnsmasq live on the picked node's root netns. The CIDR sets the gateway
              IP (the bridge takes <code>.1</code>) and the DHCP pool.
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
                  placeholder="lan"
                  autoFocus
                  required
                />
              </div>
              <div className="space-y-1.5">
                <Label htmlFor="subnet">Subnet (CIDR)</Label>
                <Input
                  id="subnet"
                  value={subnet}
                  onChange={(e) => setSubnet(e.target.value)}
                  placeholder="10.10.0.0/24"
                  required
                  className={
                    subnet.length > 0 && !subnetOk
                      ? "border-destructive focus-visible:ring-destructive"
                      : undefined
                  }
                />
                {subnet.length > 0 && !subnetOk && (
                  <p className="text-xs text-destructive">
                    Doesn't look like a CIDR (expected e.g. <code>10.10.0.0/24</code>).
                  </p>
                )}
              </div>
              <div className="space-y-1.5 md:col-span-2">
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
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="text-base">Behaviour</CardTitle>
          </CardHeader>
          <CardContent className="grid gap-4 md:grid-cols-2">
            <CheckboxField
              id="dhcp"
              label="DHCP"
              hint="Run dnsmasq bound to the bridge, leasing addresses from the subnet."
              checked={dhcp}
              onChange={setDhcp}
            />
            <CheckboxField
              id="nat"
              label="NAT"
              hint="Add an nftables masquerade so guests reach the outside world via the host."
              checked={nat}
              onChange={setNat}
            />
            <CheckboxField
              id="autostart"
              label="Autostart"
              hint="Bring the bridge up automatically when the daemon comes up. Otherwise use Start on the detail page."
              checked={autostart}
              onChange={setAutostart}
            />
          </CardContent>
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
          <Button type="submit" disabled={submitDisabled}>
            <Plus className="h-3.5 w-3.5" />
            {mutation.isPending ? "Creating…" : "Create network"}
          </Button>
          <Button type="button" variant="ghost" asChild>
            <Link to="/networks">Cancel</Link>
          </Button>
        </div>
      </form>
    </div>
  );
}
