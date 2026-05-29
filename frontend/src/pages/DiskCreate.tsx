import { useState } from "react";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { Link, useNavigate } from "react-router-dom";
import {
  ArrowLeft,
  Copy as CopyIcon,
  Database,
  Download,
  HardDriveDownload,
  Layers,
  Plus,
} from "lucide-react";
import {
  cloneDisk,
  createDisk,
  createOverlay,
  importDiskUrl,
  listDisks,
  type DiskImageInfo,
} from "@/api/disks";
import { listNodes, type NodeInfo } from "@/api/nodes";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { cn } from "@/lib/utils";

type TabKey = "blank" | "overlay" | "clone" | "import-url";

interface TabDef {
  key: TabKey;
  label: string;
  description: string;
  icon: React.ReactNode;
}

const TABS: TabDef[] = [
  {
    key: "blank",
    label: "Blank",
    description: "Allocate a fresh empty image.",
    icon: <Database className="h-3.5 w-3.5" />,
  },
  {
    key: "overlay",
    label: "Overlay",
    description: "qcow2 copy-on-write over a backing image.",
    icon: <Layers className="h-3.5 w-3.5" />,
  },
  {
    key: "clone",
    label: "Clone",
    description: "Deep-copy an existing image.",
    icon: <CopyIcon className="h-3.5 w-3.5" />,
  },
  {
    key: "import-url",
    label: "Import URL",
    description: "Download from an HTTP URL into daemon storage.",
    icon: <HardDriveDownload className="h-3.5 w-3.5" />,
  },
];

const FORMATS = ["qcow2", "raw", "vmdk", "vdi"] as const;

function NodeSelect({
  value,
  onChange,
  nodes,
  loading,
}: {
  value: string;
  onChange: (v: string) => void;
  nodes: NodeInfo[] | undefined;
  loading: boolean;
}) {
  return (
    <select
      value={value}
      onChange={(e) => onChange(e.target.value)}
      disabled={loading}
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
  );
}

function DiskSelect({
  value,
  onChange,
  disks,
  loading,
  placeholder,
}: {
  value: string;
  onChange: (v: string) => void;
  disks: DiskImageInfo[] | undefined;
  loading: boolean;
  placeholder: string;
}) {
  return (
    <select
      value={value}
      onChange={(e) => onChange(e.target.value)}
      disabled={loading}
      required
      className="flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring disabled:cursor-not-allowed disabled:opacity-50"
    >
      <option value="">{placeholder}</option>
      {(disks ?? []).map((d) => (
        <option key={d.id} value={d.name}>
          {d.name} ({d.format}
          {d.size_mb !== null ? `, ${d.size_mb} MB` : ""})
        </option>
      ))}
    </select>
  );
}

function EphemeralCheck({
  id,
  checked,
  onChange,
}: {
  id: string;
  checked: boolean;
  onChange: (v: boolean) => void;
}) {
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
          Ephemeral
        </Label>
        <p className="text-xs text-muted-foreground">
          Auto-delete when the VM this disk is attached to is removed.
        </p>
      </div>
    </div>
  );
}

// ----- per-tab forms -------------------------------------------------------

function BlankForm({
  nodes,
  nodesLoading,
}: {
  nodes: NodeInfo[] | undefined;
  nodesLoading: boolean;
}) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [name, setName] = useState("");
  const [sizeMb, setSizeMb] = useState<number>(4096);
  const [format, setFormat] = useState<string>("qcow2");
  const [node, setNode] = useState("");
  const [ephemeral, setEphemeral] = useState(false);

  const mutation = useMutation({
    mutationFn: () =>
      createDisk({
        name: name.trim(),
        size_mb: sizeMb,
        format,
        ephemeral,
        node: node || null,
      }),
    onSuccess: (disk) => {
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      navigate(`/disks/${disk.id}`);
    },
  });

  return (
    <form
      className="space-y-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (name.trim().length === 0) return;
        mutation.mutate();
      }}
    >
      <div className="grid gap-4 md:grid-cols-2">
        <div className="space-y-1.5">
          <Label htmlFor="blank-name">Name</Label>
          <Input
            id="blank-name"
            value={name}
            onChange={(e) => setName(e.target.value)}
            placeholder="web-root"
            required
          />
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="blank-size">Size (MB)</Label>
          <Input
            id="blank-size"
            type="number"
            min={1}
            value={sizeMb}
            onChange={(e) => setSizeMb(Math.max(1, Number(e.target.value) || 1))}
          />
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="blank-format">Format</Label>
          <select
            id="blank-format"
            value={format}
            onChange={(e) => setFormat(e.target.value)}
            className="flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
          >
            {FORMATS.map((f) => (
              <option key={f} value={f}>
                {f}
              </option>
            ))}
          </select>
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="blank-node">Node</Label>
          <NodeSelect value={node} onChange={setNode} nodes={nodes} loading={nodesLoading} />
        </div>
      </div>
      <EphemeralCheck id="blank-ephemeral" checked={ephemeral} onChange={setEphemeral} />
      <Submit mutation={mutation} disabled={name.trim().length === 0} />
    </form>
  );
}

function OverlayForm({
  disks,
  disksLoading,
}: {
  disks: DiskImageInfo[] | undefined;
  disksLoading: boolean;
}) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [name, setName] = useState("");
  const [backing, setBacking] = useState("");
  const [ephemeral, setEphemeral] = useState(true);

  const mutation = useMutation({
    mutationFn: () =>
      createOverlay({
        name: name.trim(),
        backing_disk_ref: backing,
        ephemeral,
      }),
    onSuccess: (disk) => {
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      navigate(`/disks/${disk.id}`);
    },
  });

  return (
    <form
      className="space-y-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (name.trim().length === 0 || backing === "") return;
        mutation.mutate();
      }}
    >
      <div className="grid gap-4 md:grid-cols-2">
        <div className="space-y-1.5">
          <Label htmlFor="overlay-name">Name</Label>
          <Input
            id="overlay-name"
            value={name}
            onChange={(e) => setName(e.target.value)}
            placeholder="vm-root"
            required
          />
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="overlay-backing">Backing disk</Label>
          <DiskSelect
            value={backing}
            onChange={setBacking}
            disks={disks}
            loading={disksLoading}
            placeholder="— pick a backing image —"
          />
        </div>
      </div>
      <EphemeralCheck id="overlay-ephemeral" checked={ephemeral} onChange={setEphemeral} />
      <Submit mutation={mutation} disabled={name.trim().length === 0 || backing === ""} />
    </form>
  );
}

function CloneForm({
  disks,
  disksLoading,
}: {
  disks: DiskImageInfo[] | undefined;
  disksLoading: boolean;
}) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [source, setSource] = useState("");
  const [newName, setNewName] = useState("");
  const [path, setPath] = useState("");
  const [ephemeral, setEphemeral] = useState(false);

  const mutation = useMutation({
    mutationFn: () =>
      cloneDisk({
        source_ref: source,
        new_name: newName.trim(),
        path: path.trim() || null,
        ephemeral,
      }),
    onSuccess: (disk) => {
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      navigate(`/disks/${disk.id}`);
    },
  });

  return (
    <form
      className="space-y-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (source === "" || newName.trim().length === 0) return;
        mutation.mutate();
      }}
    >
      <div className="grid gap-4 md:grid-cols-2">
        <div className="space-y-1.5">
          <Label htmlFor="clone-source">Source disk</Label>
          <DiskSelect
            value={source}
            onChange={setSource}
            disks={disks}
            loading={disksLoading}
            placeholder="— pick a source image —"
          />
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="clone-new-name">New disk name</Label>
          <Input
            id="clone-new-name"
            value={newName}
            onChange={(e) => setNewName(e.target.value)}
            placeholder="web-root-copy"
            required
          />
        </div>
        <div className="space-y-1.5 md:col-span-2">
          <Label htmlFor="clone-path">Destination path (optional)</Label>
          <Input
            id="clone-path"
            value={path}
            onChange={(e) => setPath(e.target.value)}
            placeholder={"<basePath>/<new_name>.<ext>"}
          />
          <p className="text-xs text-muted-foreground">
            Leave blank for the default. Relative paths resolve against the daemon's
            <code> basePath</code>; absolute paths are honoured as-is.
          </p>
        </div>
      </div>
      <EphemeralCheck id="clone-ephemeral" checked={ephemeral} onChange={setEphemeral} />
      <Submit mutation={mutation} disabled={source === "" || newName.trim().length === 0} />
    </form>
  );
}

function ImportUrlForm({
  nodes,
  nodesLoading,
}: {
  nodes: NodeInfo[] | undefined;
  nodesLoading: boolean;
}) {
  const navigate = useNavigate();
  const queryClient = useQueryClient();
  const [name, setName] = useState("");
  const [url, setUrl] = useState("");
  const [format, setFormat] = useState<string>("");
  const [sizeMb, setSizeMb] = useState<number | "">("");
  const [node, setNode] = useState("");
  const [ephemeral, setEphemeral] = useState(false);

  const mutation = useMutation({
    mutationFn: () =>
      importDiskUrl({
        name: name.trim(),
        url: url.trim(),
        format: format || null,
        size_mb: sizeMb === "" ? null : sizeMb,
        ephemeral,
        node: node || null,
      }),
    onSuccess: ({ task_id }) => {
      queryClient.invalidateQueries({ queryKey: ["disks"] });
      queryClient.invalidateQueries({ queryKey: ["tasks"] });
      navigate(`/tasks/${task_id}`);
    },
  });

  return (
    <form
      className="space-y-4"
      onSubmit={(e) => {
        e.preventDefault();
        if (name.trim().length === 0 || url.trim().length === 0) return;
        mutation.mutate();
      }}
    >
      <div className="grid gap-4 md:grid-cols-2">
        <div className="space-y-1.5">
          <Label htmlFor="imp-name">Name</Label>
          <Input
            id="imp-name"
            value={name}
            onChange={(e) => setName(e.target.value)}
            placeholder="debian-12-cloud"
            required
          />
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="imp-url">URL</Label>
          <Input
            id="imp-url"
            type="url"
            value={url}
            onChange={(e) => setUrl(e.target.value)}
            placeholder="https://cloud.debian.org/.../debian-12-genericcloud-amd64.qcow2"
            required
          />
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="imp-format">Format (optional)</Label>
          <select
            id="imp-format"
            value={format}
            onChange={(e) => setFormat(e.target.value)}
            className="flex h-9 w-full rounded-md border border-input bg-background px-3 py-1 text-sm shadow-sm focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring"
          >
            <option value="">— detect from extension —</option>
            {FORMATS.map((f) => (
              <option key={f} value={f}>
                {f}
              </option>
            ))}
          </select>
        </div>
        <div className="space-y-1.5">
          <Label htmlFor="imp-size">Resize after download (MB, optional)</Label>
          <Input
            id="imp-size"
            type="number"
            min={1}
            value={sizeMb}
            onChange={(e) => {
              const v = e.target.value;
              setSizeMb(v === "" ? "" : Math.max(1, Number(v) || 1));
            }}
            placeholder="(leave blank to keep original)"
          />
        </div>
        <div className="space-y-1.5 md:col-span-2">
          <Label htmlFor="imp-node">Node</Label>
          <NodeSelect value={node} onChange={setNode} nodes={nodes} loading={nodesLoading} />
        </div>
      </div>
      <EphemeralCheck id="imp-ephemeral" checked={ephemeral} onChange={setEphemeral} />
      <p className="text-xs text-muted-foreground">
        Runs asynchronously. On submit you'll be routed to the new task page to watch progress.
      </p>
      <Submit
        mutation={mutation}
        disabled={name.trim().length === 0 || url.trim().length === 0}
        label="Start import"
      />
    </form>
  );
}

function Submit({
  mutation,
  disabled,
  label = "Create disk",
}: {
  mutation: { isPending: boolean; error: unknown };
  disabled: boolean;
  label?: string;
}) {
  return (
    <div className="space-y-3">
      {mutation.error != null && (
        <pre className="overflow-auto rounded-md border border-border bg-destructive/10 p-3 font-mono text-xs text-destructive">
          {(mutation.error as Error).message}
        </pre>
      )}
      <div className="flex items-center gap-3">
        <Button type="submit" disabled={disabled || mutation.isPending}>
          <Plus className="h-3.5 w-3.5" />
          {mutation.isPending ? "Working…" : label}
        </Button>
        <Button type="button" variant="ghost" asChild>
          <Link to="/disks">Cancel</Link>
        </Button>
      </div>
    </div>
  );
}

// ----- page ---------------------------------------------------------------

export default function DiskCreate() {
  const [tab, setTab] = useState<TabKey>("blank");

  const { data: nodes, isLoading: nodesLoading } = useQuery<NodeInfo[]>({
    queryKey: ["nodes"],
    queryFn: ({ signal }) => listNodes(signal),
    staleTime: 30_000,
  });
  // Disks list powers the overlay/clone source dropdowns; only fetch
  // when one of those tabs is active.
  const wantsDisks = tab === "overlay" || tab === "clone";
  const { data: disks, isLoading: disksLoading } = useQuery<DiskImageInfo[]>({
    queryKey: ["disks"],
    queryFn: ({ signal }) => listDisks(signal),
    staleTime: 10_000,
    enabled: wantsDisks,
  });

  const active = TABS.find((t) => t.key === tab)!;

  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to="/disks">
            <ArrowLeft className="h-4 w-4" />
            Disks
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="flex items-center gap-2 text-2xl font-semibold tracking-tight">
            <Download className="h-6 w-6 text-muted-foreground" />
            New disk
          </h1>
        </div>
      </div>

      <Card>
        <CardHeader className="space-y-3">
          <div className="grid grid-cols-2 gap-1 rounded-md border border-border bg-muted/20 p-1 md:grid-cols-4">
            {TABS.map((t) => (
              <button
                key={t.key}
                type="button"
                onClick={() => setTab(t.key)}
                className={cn(
                  "inline-flex items-center justify-center gap-2 rounded-md px-3 py-1.5 text-sm transition-colors",
                  tab === t.key
                    ? "bg-background font-medium text-foreground shadow-sm"
                    : "text-muted-foreground hover:text-foreground",
                )}
              >
                {t.icon}
                {t.label}
              </button>
            ))}
          </div>
          <CardTitle className="text-base">{active.label}</CardTitle>
          <CardDescription>{active.description}</CardDescription>
        </CardHeader>
        <CardContent>
          {tab === "blank" && <BlankForm nodes={nodes} nodesLoading={nodesLoading} />}
          {tab === "overlay" && <OverlayForm disks={disks} disksLoading={disksLoading} />}
          {tab === "clone" && <CloneForm disks={disks} disksLoading={disksLoading} />}
          {tab === "import-url" && <ImportUrlForm nodes={nodes} nodesLoading={nodesLoading} />}
        </CardContent>
      </Card>
    </div>
  );
}
