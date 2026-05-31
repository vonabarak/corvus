/**
 * Tiny fetch wrapper.
 *
 * Same-origin in production (the gateway serves both the SPA and the
 * API); the vite dev server proxies /api/ to the gateway, so the same
 * base path works in development too.
 */

export class ApiError extends Error {
  /** The parsed `detail` field from a FastAPI JSON error body, or the
   * raw body when it isn't JSON. Callers (`onError` handlers, etc.)
   * should surface this directly to the user — it carries the daemon's
   * verbatim rejection reason after the gateway's exception handler
   * translates it. */
  public readonly detail: string;

  constructor(
    public readonly status: number,
    public readonly statusText: string,
    public readonly body: string,
  ) {
    // FastAPI's default + the gateway's `CorvusError` handler emit
    // `{"detail": "..."}` JSON bodies. Parse it so the `Error.message`
    // operators see is the human-readable reason, not the raw envelope.
    let detail = body;
    try {
      const parsed: unknown = JSON.parse(body);
      if (
        parsed &&
        typeof parsed === "object" &&
        "detail" in parsed &&
        typeof (parsed as { detail: unknown }).detail === "string"
      ) {
        detail = (parsed as { detail: string }).detail;
      }
    } catch {
      // Not JSON — keep the raw body.
    }
    super(`${status} ${statusText}: ${detail || "(empty body)"}`);
    this.name = "ApiError";
    this.detail = detail;
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
