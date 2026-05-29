// Hand-written ambient declarations for the vendored spice-html5 source
// at frontend/src/lib/spice/. Upstream ships no .d.ts; we only need a
// thin shim covering the parts we actually touch from SpiceConsole.tsx.
//
// If you reach for a member that's not declared here, prefer extending
// this file over `// @ts-expect-error` — keeps the surface explicit.

declare module "@/lib/spice/main.js" {
  /** Subset of the SpiceMainConn config object, covering only what
   * SpiceConsole.tsx passes in. The library accepts more keys (dump_id,
   * message_id, port_id, …) — add them here as we start using them. */
  export interface SpiceMainConnConfig {
    uri: string;
    password: string;
    /** DOM id (string) of the element that will host the SPICE
     * <canvas>. The lib calls ``document.getElementById`` on this —
     * passing a node reference silently fails. */
    screen_id: string;
    onerror?: (e: Event | Error | string) => void;
    onsuccess?: () => void;
    onagent?: (agent: unknown) => void;
  }

  export class SpiceMainConn {
    constructor(config: SpiceMainConnConfig);
    /** Tear down the WebSocket + all attached channels. Idempotent. */
    stop(): void;
    /** Ask the guest agent to reconfigure its framebuffer.
     * Signature mirrors the upstream resize_helper call:
     * resize_window(monitor, width, height, depth, x, y). Width/height
     * should be multiples of 8 (Xorg constraint). Depth is bits-per-
     * pixel (32 in practice). Monitor + x/y stay 0 for single-screen. */
    resize_window(
      monitor: number,
      width: number,
      height: number,
      depth: number,
      x: number,
      y: number,
    ): void;
  }
}

declare module "@/lib/spice/inputs.js" {
  import type { SpiceMainConn } from "@/lib/spice/main.js";
  /** Sends Ctrl+Alt+Del on the active inputs channel of the given
   * SpiceMainConn instance. The SPICE wire-level scancodes are baked
   * into this helper; we don't have to emit them ourselves. */
  export function sendCtrlAltDel(conn: SpiceMainConn): void;
}
