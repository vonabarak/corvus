import { Badge } from "@/components/ui/badge";

/**
 * Maps `Vm.status` strings (from src/Corvus/Model.hs `VmStatus`) to
 * shadcn Badge variants. Keep this in lockstep with the enum:
 * `stopped | starting | running | stopping | paused | saved | error
 *  | saving | loading | migrating`.
 */
function variantFor(status: string): "success" | "warning" | "destructive" | "muted" | "default" {
  switch (status) {
    case "running":
      return "success";
    case "starting":
    case "stopping":
    case "saving":
    case "loading":
    case "migrating":
      return "warning";
    case "error":
      return "destructive";
    case "stopped":
      return "muted";
    case "paused":
    case "saved":
      return "default";
    default:
      return "muted";
  }
}

export function VmStatusBadge({ status }: { status: string }) {
  return <Badge variant={variantFor(status)}>{status}</Badge>;
}
