import { Badge } from "@/components/ui/badge";

/**
 * Task result enum from src/Corvus/Model.hs `TaskResult`:
 *   running | success | error
 */
export function TaskResultBadge({ result }: { result: string }) {
  switch (result) {
    case "running":
      return <Badge variant="warning">running</Badge>;
    case "success":
      return <Badge variant="success">success</Badge>;
    case "error":
      return <Badge variant="destructive">error</Badge>;
    default:
      return <Badge variant="muted">{result}</Badge>;
  }
}
