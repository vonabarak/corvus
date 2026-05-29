/**
 * Map a task's (subsystem, entity_id) pair to the resource route on
 * this UI. Returns ``null`` for subsystems that don't have a page yet
 * (system, snapshot, shared-dir, apply — the last lands with the
 * Apply slice).
 */
export function subsystemEntityRoute(subsystem: string, entityId: number | null): string | null {
  if (entityId === null) return null;
  switch (subsystem) {
    case "vm":
      return `/vms/${entityId}`;
    case "disk":
      return `/disks/${entityId}`;
    case "network":
      return `/networks/${entityId}`;
    case "ssh-key":
      return `/ssh-keys/${entityId}`;
    case "template":
      return `/templates/${entityId}`;
    default:
      return null;
  }
}
