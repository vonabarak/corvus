import { Badge } from "@/components/ui/badge";

/**
 * NodeAdminState enum from src/Corvus/Model.hs:
 *   online | draining | maintenance | offline
 *
 * online → success, draining → warning, the rest muted.
 */
export function NodeStateBadge({ state }: { state: string }) {
  switch (state) {
    case "online":
      return <Badge variant="success">online</Badge>;
    case "draining":
      return <Badge variant="warning">draining</Badge>;
    case "maintenance":
      return <Badge variant="warning">maintenance</Badge>;
    case "offline":
      return <Badge variant="destructive">offline</Badge>;
    default:
      return <Badge variant="muted">{state}</Badge>;
  }
}
