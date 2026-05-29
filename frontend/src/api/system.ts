import { apiGet } from "./client";

export interface StatusInfo {
  uptime_seconds: number;
  connections: number;
  version: string;
  protocol_version: number;
}

export function getStatus(signal?: AbortSignal): Promise<StatusInfo> {
  return apiGet<StatusInfo>("/status", signal);
}
