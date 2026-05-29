import { apiGet, apiSend } from "./client";

/** Mirrors corvus_client.types.DiskImagePlacement. */
export interface DiskImagePlacement {
  node_id: number;
  node_name: string;
  file_path: string;
}

/** Mirrors corvus_client.types.DiskAttachment. */
export interface DiskAttachment {
  vm_id: number;
  vm_name: string;
}

/** Mirrors corvus_client.types.DiskImageInfo (used by both list + detail). */
export interface DiskImageInfo {
  id: number;
  name: string;
  format: string;
  created_at: string;
  placements: DiskImagePlacement[];
  attached_to: DiskAttachment[];
  size_mb: number | null;
  backing_image_id: number | null;
  backing_image_name: string | null;
  ephemeral: boolean;
}

export interface SnapshotInfo {
  id: number;
  name: string;
  created_at: string;
  size_mb: number | null;
}

export function listDisks(signal?: AbortSignal): Promise<DiskImageInfo[]> {
  return apiGet<DiskImageInfo[]>("/disks", signal);
}

export function getDisk(id: number, signal?: AbortSignal): Promise<DiskImageInfo> {
  return apiGet<DiskImageInfo>(`/disks/${id}`, signal);
}

export function resizeDisk(id: number, newSizeMb: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("POST", `/disks/${id}/resize`, { new_size_mb: newSizeMb });
}

export function deleteDisk(id: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("DELETE", `/disks/${id}`);
}

export function listSnapshots(diskId: number, signal?: AbortSignal): Promise<SnapshotInfo[]> {
  return apiGet<SnapshotInfo[]>(`/disks/${diskId}/snapshots`, signal);
}

export function createSnapshot(diskId: number, name: string): Promise<SnapshotInfo> {
  return apiSend<SnapshotInfo>("POST", `/disks/${diskId}/snapshots`, { name });
}

export function rollbackSnapshot(diskId: number, snapId: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("POST", `/disks/${diskId}/snapshots/${snapId}/rollback`);
}

export function mergeSnapshot(diskId: number, snapId: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("POST", `/disks/${diskId}/snapshots/${snapId}/merge`);
}

export function deleteSnapshot(diskId: number, snapId: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("DELETE", `/disks/${diskId}/snapshots/${snapId}`);
}
