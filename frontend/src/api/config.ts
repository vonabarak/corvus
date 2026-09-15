/** Configuration endpoint: enum values and valid transitions.

The frontend calls ``GET /api/config`` on mount to get the canonical
enum values and lifecycle transition table so it never has to
hard-code values that could drift from the daemon.
*/

export interface ConfigResponse {
  drive_interfaces: string[];
  drive_media: string[];
  cache_types: string[];
  transitions: Record<string, string[]>;
}

let _cached: ConfigResponse | null = null;

/** Fetch the gateway config, caching the result for the session.

The config is static for the lifetime of the SPA — it only changes
on redeploy — so a single fetch covers all consumers.
*/
export async function fetchConfig(): Promise<ConfigResponse> {
  if (_cached) return _cached;
  const res = await fetch("/api/config");
  if (!res.ok) throw new Error(`config: ${res.status} ${res.statusText}`);
  _cached = (await res.json()) as ConfigResponse;
  return _cached;
}
