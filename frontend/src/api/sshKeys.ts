import { apiGet, apiSend } from "./client";
import type { NamedRef } from "./refs";

export interface VmAttachment {
  vm: NamedRef;
}

/** Mirrors corvus_client.types.SshKeyInfo. */
export interface SshKeyInfo {
  id: number;
  name: string;
  public_key: string;
  created_at: string;
  attached_vms: VmAttachment[];
}

export function listSshKeys(signal?: AbortSignal): Promise<SshKeyInfo[]> {
  return apiGet<SshKeyInfo[]>("/ssh-keys", signal);
}

export function getSshKey(id: number, signal?: AbortSignal): Promise<SshKeyInfo> {
  return apiGet<SshKeyInfo>(`/ssh-keys/${id}`, signal);
}

export function createSshKey(name: string, publicKey: string): Promise<SshKeyInfo> {
  return apiSend<SshKeyInfo>("POST", "/ssh-keys", { name, public_key: publicKey });
}

export function deleteSshKey(id: number): Promise<{ status: string }> {
  return apiSend<{ status: string }>("DELETE", `/ssh-keys/${id}`);
}
