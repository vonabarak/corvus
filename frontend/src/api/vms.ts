import { apiGet, apiSend } from "./client";

/** Subset of fields returned by `/api/vms` (list view). Mirrors
 * corvus_client.types.VmInfo. */
export interface VmInfo {
  id: number;
  name: string;
  node_id: number;
  node_name: string;
  status: string;
  cpu_count: number;
  ram_mb: number;
  headless: boolean;
  guest_agent: boolean;
  cloud_init: boolean;
  autostart: boolean;
  last_healthcheck: string | null;
  reboot_quirk: boolean;
  cpu_model: string;
}

export interface DriveInfo {
  id: number;
  disk_image_id: number;
  disk_image_name: string;
  interface: string;
  file_path: string;
  format: string;
  media: string;
  read_only: boolean;
  cache_type: string;
  discard: boolean;
}

export interface NetIfInfo {
  id: number;
  type: string;
  host_device: string;
  mac_address: string;
  network_id: number | null;
  network_name: string | null;
  guest_ip_addresses: string | null;
  ip_address: string | null;
}

export interface SharedDirInfo {
  id: number;
  path: string;
  tag: string;
  cache: string;
  read_only: boolean;
  pid: number | null;
}

/** Detail view returned by `/api/vms/{id}`. Mirrors
 * corvus_client.types.VmDetails. */
export interface VmDetails extends Omit<VmInfo, "last_healthcheck"> {
  created_at: string;
  monitor_socket: string;
  serial_socket: string;
  guest_agent_socket: string;
  drives: DriveInfo[];
  net_ifs: NetIfInfo[];
  shared_dirs: SharedDirInfo[];
  description: string | null;
  spice_port: number | null;
  vsock_cid: number | null;
  last_healthcheck: string | null;
  error_message: string | null;
  last_error_at: string | null;
}

export function listVms(signal?: AbortSignal): Promise<VmInfo[]> {
  return apiGet<VmInfo[]>("/vms", signal);
}

export function getVm(id: number, signal?: AbortSignal): Promise<VmDetails> {
  return apiGet<VmDetails>(`/vms/${id}`, signal);
}

export type VmAction = "start" | "stop" | "pause" | "reset" | "save" | "send-ctrl-alt-del";

export function vmAction(id: number, action: VmAction): Promise<{ status: string }> {
  return apiSend<{ status: string }>("POST", `/vms/${id}/${action}`);
}

export function deleteVm(id: number, keepDisks = false): Promise<{ status: string }> {
  const qs = keepDisks ? "?keep_disks=true" : "";
  return apiSend<{ status: string }>("DELETE", `/vms/${id}${qs}`);
}
