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
