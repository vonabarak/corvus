import { apiGet } from "./client";
import type { NamedRef } from "./refs";

/** Mirrors corvus_client.types.TaskInfo. */
export interface TaskInfo {
  id: number;
  started_at: string;
  subsystem: string;
  command: string;
  result: string;
  client_name: string;
  // Flat parent reference (no NamedRef): tasks don't have a
  // human-readable name field. See CLAUDE.md
  // `## Project Rules / Cross-entity references`.
  parent_id: number | null;
  finished_at: string | null;
  entity: NamedRef | null;
  message: string | null;
}

export interface ListTasksParams {
  limit?: number;
  subsystem?: string;
  entity_id?: number;
  result?: string;
}

export function listTasks(params: ListTasksParams = {}, signal?: AbortSignal): Promise<TaskInfo[]> {
  const qs = new URLSearchParams();
  if (params.limit !== undefined) qs.set("limit", String(params.limit));
  if (params.subsystem) qs.set("subsystem", params.subsystem);
  if (params.entity_id !== undefined) qs.set("entity_id", String(params.entity_id));
  if (params.result) qs.set("result", params.result);
  const suffix = qs.toString();
  return apiGet<TaskInfo[]>(`/tasks${suffix ? `?${suffix}` : ""}`, signal);
}

export function getTask(id: number, signal?: AbortSignal): Promise<TaskInfo> {
  return apiGet<TaskInfo>(`/tasks/${id}`, signal);
}

export function listTaskChildren(id: number, signal?: AbortSignal): Promise<TaskInfo[]> {
  return apiGet<TaskInfo[]>(`/tasks/${id}/children`, signal);
}
