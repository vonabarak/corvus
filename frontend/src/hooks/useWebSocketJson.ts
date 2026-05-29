import { useEffect, useState } from "react";

export type WsState = "connecting" | "open" | "closed" | "error";

export interface WsResult<T> {
  /** Last JSON frame received from the server, or `null` until the first frame. */
  last: T | null;
  /** Connection state. */
  state: WsState;
  /** Server-supplied close reason, if any. */
  reason: string;
}

/**
 * Subscribe to a streaming JSON WebSocket endpoint.
 *
 * The hook opens a WS to the given path (relative to the page origin,
 * automatically wss:// for https://) and stores the most recent JSON
 * frame in state. ``open`` flips to ``closed`` when the server closes
 * normally and to ``error`` on transport failure.
 *
 * Pass ``enabled=false`` to skip opening the WS — useful when the
 * caller wants to wait for a precondition (e.g. only subscribe to a
 * VM's guest-agent stream once the VM detail query returned a record
 * with ``guest_agent === true``).
 */
export function useWebSocketJson<T>(path: string, enabled: boolean = true): WsResult<T> {
  const [last, setLast] = useState<T | null>(null);
  const [state, setState] = useState<WsState>("connecting");
  const [reason, setReason] = useState<string>("");

  useEffect(() => {
    if (!enabled) {
      setState("closed");
      return;
    }
    setState("connecting");
    setLast(null);
    setReason("");

    const proto = window.location.protocol === "https:" ? "wss:" : "ws:";
    const ws = new WebSocket(`${proto}//${window.location.host}${path}`);

    ws.onopen = () => setState("open");
    ws.onmessage = (e) => {
      if (typeof e.data === "string") {
        try {
          setLast(JSON.parse(e.data) as T);
        } catch {
          // ignore malformed frames
        }
      }
    };
    ws.onerror = () => setState("error");
    ws.onclose = (e) => {
      // If we already flagged an error, keep that state — onclose
      // always fires after onerror.
      setState((prev) => (prev === "error" ? prev : "closed"));
      if (e.reason) setReason(e.reason);
    };

    return () => {
      ws.close();
    };
  }, [path, enabled]);

  return { last, state, reason };
}
