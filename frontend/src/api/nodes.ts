import { apiGet } from "./client";

/** Mirrors corvus_client.types.NodeInfo. */
export interface NodeInfo {
  id: number;
  name: string;
  host: string;
  node_agent_port: number;
  net_agent_port: number;
  admin_state: string;
  created_at: string;
  cpu_count: number | null;
  ram_mb_total: number | null;
  ram_mb_free: number | null;
  storage_bytes_total: number | null;
  storage_bytes_free: number | null;
  load_avg1: number | null;
  last_node_agent_push_at: string | null;
  last_net_agent_push_at: string | null;
  netd_disabled: boolean;
  netd_connected: boolean;
}

/** Mirrors corvus_client.types.NodeDetails (a superset of NodeInfo). */
export interface NodeDetails extends NodeInfo {
  base_path: string;
  description: string | null;
  load_avg5: number | null;
  load_avg15: number | null;
  kernel_release: string | null;
  agent_version: string | null;
}

export function listNodes(signal?: AbortSignal): Promise<NodeInfo[]> {
  return apiGet<NodeInfo[]>("/nodes", signal);
}

export function getNode(id: number, signal?: AbortSignal): Promise<NodeDetails> {
  return apiGet<NodeDetails>(`/nodes/${id}`, signal);
}
