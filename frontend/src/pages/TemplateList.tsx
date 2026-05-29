import { useQuery } from "@tanstack/react-query";
import { Link } from "react-router-dom";
import { AlertCircle, Cpu, FileCode, Plus } from "lucide-react";
import { listTemplates, type TemplateVmInfo } from "@/api/templates";
import { Card, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from "@/components/ui/table";
import { Badge } from "@/components/ui/badge";
import { formatMb } from "@/lib/format";

function FlagBadges({ t }: { t: TemplateVmInfo }) {
  return (
    <div className="flex flex-wrap gap-1">
      {t.guest_agent && (
        <Badge variant="muted" className="text-[10px]">
          QGA
        </Badge>
      )}
      {t.autostart && (
        <Badge variant="muted" className="text-[10px]">
          autostart
        </Badge>
      )}
      {t.headless && (
        <Badge variant="muted" className="text-[10px]">
          headless
        </Badge>
      )}
    </div>
  );
}

export default function TemplateList() {
  const { data, error, isLoading } = useQuery<TemplateVmInfo[]>({
    queryKey: ["templates"],
    queryFn: ({ signal }) => listTemplates(signal),
    refetchInterval: 10000,
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
            Failed to load templates
          </CardTitle>
          <CardDescription>{(error as Error).message}</CardDescription>
        </CardHeader>
      </Card>
    );
  }
  const templates = data ?? [];

  return (
    <div className="space-y-6">
      <div className="flex items-start justify-between gap-4">
        <div>
          <h1 className="text-2xl font-semibold tracking-tight">Templates</h1>
          <p className="text-sm text-muted-foreground">
            {templates.length === 0
              ? "No templates yet."
              : `${templates.length} template${templates.length === 1 ? "" : "s"} registered.`}
          </p>
        </div>
        <Button size="sm" asChild>
          <Link to="/templates/new">
            <Plus className="h-3.5 w-3.5" />
            New template
          </Link>
        </Button>
      </div>
      <Card>
        <Table>
          <TableHeader>
            <TableRow>
              <TableHead>Name</TableHead>
              <TableHead className="text-right">CPU</TableHead>
              <TableHead className="text-right">RAM</TableHead>
              <TableHead>Flags</TableHead>
              <TableHead>Description</TableHead>
            </TableRow>
          </TableHeader>
          <TableBody>
            {templates.map((t) => (
              <TableRow key={t.id}>
                <TableCell>
                  <Link
                    to={`/templates/${t.id}`}
                    className="inline-flex items-center gap-2 font-medium text-foreground hover:underline"
                  >
                    <FileCode className="h-3.5 w-3.5 text-muted-foreground" />
                    {t.name}
                  </Link>
                  <div className="text-xs text-muted-foreground">#{t.id}</div>
                </TableCell>
                <TableCell className="text-right tabular-nums">
                  <span className="inline-flex items-center gap-1">
                    <Cpu className="h-3 w-3 text-muted-foreground" />
                    {t.cpu_count}
                  </span>
                </TableCell>
                <TableCell className="text-right tabular-nums">{formatMb(t.ram_mb)}</TableCell>
                <TableCell>
                  <FlagBadges t={t} />
                </TableCell>
                <TableCell className="text-sm text-muted-foreground">
                  {t.description ?? "—"}
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </Card>
    </div>
  );
}
