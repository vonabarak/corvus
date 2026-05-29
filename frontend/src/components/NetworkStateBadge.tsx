import { Badge } from "@/components/ui/badge";

/**
 * Networks are binary: either the bridge is up (success) or down
 * (muted). No intermediate states — the daemon brings the bridge
 * up/down synchronously through corvus-netd.
 */
export function NetworkStateBadge({ running }: { running: boolean }) {
  return <Badge variant={running ? "success" : "muted"}>{running ? "running" : "stopped"}</Badge>;
}
