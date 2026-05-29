import { apiGet, apiSend } from "./client";

/** Mirrors corvus_client.types.NetworkInfo. */
export interface NetworkInfo {
  id: number;
  name: string;
  subnet: string;
  dhcp: boolean;
  nat: boolean;
  running: boolean;
  autostart: boolean;
  created_at: string;
  dnsmasq_pid: number | null;
  vni: number | null;
  peer_node_ids: number[];
}

export interface NetworkCreateBody {
  name: string;
  subnet: string;
  node?: string | null;
  dhcp?: boolean;
  nat?: boolean;
  autostart?: boolean;
}

export function listNetworks(signal?: AbortSignal): Promise<NetworkInfo[]> {
  return apiGet<NetworkInfo[]>("/networks", signal);
}

export function createNetwork(body: NetworkCreateBody): Promise<NetworkInfo> {
  return apiSend<NetworkInfo>("POST", "/networks", body);
}

export function getNetwork(id: number, signal?: AbortSignal): Promise<NetworkInfo> {
  return apiGet<NetworkInfo>(`/networks/${id}`, signal);
}

export function startNetwork(id: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("POST", `/networks/${id}/start`);
}

export function stopNetwork(id: number, force = false): Promise<{ status: string }> {
  const qs = force ? "?force=true" : "";
  return apiSend<{ status: string }>("POST", `/networks/${id}/stop${qs}`);
}

export function deleteNetwork(id: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("DELETE", `/networks/${id}`);
}
