import { apiSend } from "./client";

export interface ApplyCreated {
  name: string;
  id: number;
}

export interface ApplyResult {
  ssh_keys: ApplyCreated[];
  disks: ApplyCreated[];
  networks: ApplyCreated[];
  vms: ApplyCreated[];
  templates: ApplyCreated[];
}

export interface ApplyResponse {
  result: ApplyResult;
  task_id: number;
}

export function runApply(yaml: string, skipExisting: boolean): Promise<ApplyResponse> {
  return apiSend<ApplyResponse>("POST", "/apply", { yaml, skip_existing: skipExisting });
}
