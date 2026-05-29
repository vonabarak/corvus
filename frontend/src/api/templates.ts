import { apiGet, apiSend } from "./client";

export interface TemplateVmInfo {
  id: number;
  name: string;
  cpu_count: number;
  ram_mb: number;
  headless: boolean;
  guest_agent: boolean;
  autostart: boolean;
  description: string | null;
}

export interface TemplateDriveInfo {
  interface: string;
  read_only: boolean;
  cache_type: string;
  discard: boolean;
  clone_strategy: string;
  disk_image_id: number | null;
  disk_image_name: string | null;
  media: string | null;
  size_mb: number | null;
  format: string | null;
}

export interface TemplateNetIfInfo {
  type: string;
  host_device: string | null;
}

export interface TemplateSshKeyInfo {
  id: number;
  name: string;
}

export interface CloudInitInfo {
  user_data: string | null;
  network_config: string | null;
  inject_ssh_keys: boolean;
}

export interface TemplateDetails {
  id: number;
  name: string;
  cpu_count: number;
  ram_mb: number;
  headless: boolean;
  cloud_init: boolean;
  guest_agent: boolean;
  autostart: boolean;
  created_at: string;
  drives: TemplateDriveInfo[];
  net_ifs: TemplateNetIfInfo[];
  ssh_keys: TemplateSshKeyInfo[];
  description: string | null;
  cloud_init_config: CloudInitInfo | null;
}

export function listTemplates(signal?: AbortSignal): Promise<TemplateVmInfo[]> {
  return apiGet<TemplateVmInfo[]>("/templates", signal);
}

export function getTemplate(id: number, signal?: AbortSignal): Promise<TemplateDetails> {
  return apiGet<TemplateDetails>(`/templates/${id}`, signal);
}

export function instantiateTemplate(
  id: number,
  name: string,
  node?: string,
): Promise<{ id: number; name: string }> {
  const body: Record<string, string> = { name };
  if (node) body.node = node;
  return apiSend<{ id: number; name: string }>("POST", `/templates/${id}/instantiate`, body);
}

export function deleteTemplate(id: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("DELETE", `/templates/${id}`);
}

export function getVmCloudInit(vmId: number, signal?: AbortSignal): Promise<CloudInitInfo> {
  return apiGet<CloudInitInfo>(`/vms/${vmId}/cloud-init`, signal);
}
