import { Badge } from "@/components/ui/badge";

/**
 * NodeAdminState enum from src/Corvus/Model.hs:
 *   online | maintenance | offline
 *
 * online → success, maintenance → warning, offline → destructive.
 */
export function NodeStateBadge({ state }: { state: string }) {
  switch (state) {
    case "online":
      return <Badge variant="success">online</Badge>;
    case "maintenance":
      return <Badge variant="warning">maintenance</Badge>;
    case "offline":
      return <Badge variant="destructive">offline</Badge>;
    default:
      return <Badge variant="muted">{state}</Badge>;
  }
}
