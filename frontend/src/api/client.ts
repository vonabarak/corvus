/**
 * Tiny fetch wrapper.
 *
 * Same-origin in production (the gateway serves both the SPA and the
 * API); the vite dev server proxies /api/ to the gateway, so the same
 * base path works in development too.
 */

export class ApiError extends Error {
  constructor(
    public readonly status: number,
    public readonly statusText: string,
    public readonly body: string,
  ) {
    super(`${status} ${statusText}: ${body || "(empty body)"}`);
    this.name = "ApiError";
  }
}

export async function apiGet<T>(path: string, signal?: AbortSignal): Promise<T> {
  const res = await fetch(`/api${path}`, {
    method: "GET",
    headers: { Accept: "application/json" },
    signal,
  });
  if (!res.ok) {
    throw new ApiError(res.status, res.statusText, await res.text());
  }
  return (await res.json()) as T;
}

/** Generic mutating request. JSON body is encoded when provided; the
 * response body is parsed as JSON regardless (every gateway endpoint
 * returns at least ``{"status": "..."}``). */
export async function apiSend<T>(
  method: "POST" | "DELETE" | "PUT" | "PATCH",
  path: string,
  body?: unknown,
  signal?: AbortSignal,
): Promise<T> {
  const init: RequestInit = {
    method,
    headers: { Accept: "application/json" },
    signal,
  };
  if (body !== undefined) {
    (init.headers as Record<string, string>)["Content-Type"] = "application/json";
    init.body = JSON.stringify(body);
  }
  const res = await fetch(`/api${path}`, init);
  if (!res.ok) {
    throw new ApiError(res.status, res.statusText, await res.text());
  }
  return (await res.json()) as T;
}
