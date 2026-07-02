import { apiGet } from "./client";

export interface StatusInfo {
  uptime_seconds: number;
  connections: number;
  version: string;
  protocol_version: number;
  database_backend: string;
  database_version: string;
}

export function getStatus(signal?: AbortSignal): Promise<StatusInfo> {
  return apiGet<StatusInfo>("/status", signal);
}
