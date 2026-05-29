import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, Cpu, HardDrive } from "lucide-react";
import { listVms, type VmInfo } from "@/api/vms";
import { Card, CardHeader, CardTitle, CardDescription } from "@/components/ui/card";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { VmStatusBadge } from "@/components/VmStatusBadge";

function FlagBadges({ vm }: { vm: VmInfo }) {
  return (
    <div className="flex flex-wrap gap-1">
      {vm.guest_agent && (
        <Badge variant="muted" className="text-[10px]">
          QGA
        </Badge>
      )}
      {vm.cloud_init && (
        <Badge variant="muted" className="text-[10px]">
          cloud-init
        </Badge>
      )}
      {vm.autostart && (
        <Badge variant="muted" className="text-[10px]">
          autostart
        </Badge>
      )}
      {vm.headless && (
        <Badge variant="muted" className="text-[10px]">
          headless
        </Badge>
      )}
    </div>
  );
}

export default function VmList() {
  const { data, error, isLoading } = useQuery<VmInfo[]>({
    queryKey: ["vms"],
    queryFn: ({ signal }) => listVms(signal),
    refetchInterval: 5000,
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
            Failed to load VMs
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const vms = data ?? [];

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-2xl font-semibold tracking-tight">Virtual Machines</h1>
        <p className="text-sm text-muted-foreground">
          {vms.length === 0
            ? "No VMs yet."
            : `${vms.length} VM${vms.length === 1 ? "" : "s"} registered.`}
        </p>
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Name</TableHead>
              <TableHead>Node</TableHead>
              <TableHead>Status</TableHead>
              <TableHead className="text-right">CPU</TableHead>
              <TableHead className="text-right">RAM</TableHead>
              <TableHead>Flags</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {vms.map((vm) => (
              <TableRow key={vm.id}>
                <TableCell>
                  <Link
                    to={`/vms/${vm.id}`}
                    className="font-medium text-foreground hover:underline"
                  >
                    {vm.name}
                  </Link>
                  <div className="text-xs text-muted-foreground">#{vm.id}</div>
                </TableCell>
                <TableCell className="text-muted-foreground">{vm.node_name}</TableCell>
                <TableCell>
                  <VmStatusBadge status={vm.status} />
                </TableCell>
                <TableCell className="text-right tabular-nums">
                  <span className="inline-flex items-center gap-1">
                    <Cpu className="h-3 w-3 text-muted-foreground" />
                    {vm.cpu_count}
                  </span>
                </TableCell>
                <TableCell className="text-right tabular-nums">
                  <span className="inline-flex items-center gap-1">
                    <HardDrive className="h-3 w-3 text-muted-foreground" />
                    {vm.ram_mb} MB
                  </span>
                </TableCell>
                <TableCell>
                  <FlagBadges vm={vm} />
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Card>
    </div>
  );
}
