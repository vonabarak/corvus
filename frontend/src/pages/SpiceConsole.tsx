import { useCallback, useEffect, useRef, useState } from "react";
import { Link, useParams } from "react-router-dom";
import { ArrowLeft, Circle, Keyboard, Zap } from "lucide-react";
import { toast } from "sonner";
import { createSpiceSession } from "@/api/vms";
import { SpiceMainConn } from "@/lib/spice/main.js";
import { sendCtrlAltDel } from "@/lib/spice/inputs.js";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";

type ConnState = "connecting" | "open" | "closed" | "error";

/**
 * In-browser SPICE console.
 *
 * Two-step handshake (see python/corvus_web/routes/spice.py for the
 * server side):
 *   1. POST /api/vms/{id}/spice → {session_id, password, ttl_seconds}.
 *      The gateway has already pushed the password to QEMU via QMP.
 *   2. Open WS at /api/vms/{id}/spice/ws?session=<session_id>. The
 *      gateway proxies binary frames to QEMU's SPICE TCP port.
 *   3. Hand WS uri + password to SpiceMainConn; the library does the
 *      SPICE protocol handshake against QEMU's stored ticket.
 *
 * Cleanup: sc.stop() tears down the SPICE channels and closes the WS,
 * which the gateway sees and closes the TCP socket.
 *
 * Guest-side resize: once the SPICE main channel is up we ask the
 * guest to reflow its framebuffer to the host div's dimensions via
 * sc.resize_window(). The guest needs spice-vdagent + a SPICE-aware
 * display driver (qxl on Linux, the SPICE Windows driver on Windows)
 * for this to take effect; without those, the canvas stays at the
 * guest's boot resolution and the host div scrolls.
 */
export default function SpiceConsole() {
  const { id } = useParams<{ id: string }>();
  const vmId = Number(id);

  // spice-html5's display.js does ``document.getElementById(screen_id)``
  // to host its <canvas>, so we need a stable string id (not an element
  // ref). Tie it to the route param so multiple console tabs at once
  // wouldn't collide (rare, but cheap).
  const screenDomId = `spice-screen-${vmId}`;
  // Outer wrapper that we control. spice-html5 mutates the inner
  // (screen_id) div's CSS height to match the guest's framebuffer
  // every time a surface is created, so we can't use *that* element
  // to ask for a new size — we'd be reading back the value the library
  // just wrote. The outer ref gives us a stable "this is how much
  // space the page allots" measurement instead.
  const wrapperRef = useRef<HTMLDivElement | null>(null);
  const scRef = useRef<SpiceMainConn | null>(null);
  const [state, setState] = useState<ConnState>("connecting");
  const [reason, setReason] = useState<string>("");

  /** Ask the guest to resize its framebuffer to match our wrapper.
   * Xorg / qxl require both dimensions to be multiples of 8 — round
   * down. We clamp to ≥320×200 so the guest doesn't see an absurd
   * request mid-resize while the layout is settling. */
  const requestGuestResize = useCallback(() => {
    const sc = scRef.current;
    const wrap = wrapperRef.current;
    if (!sc || !wrap) return;
    const w = Math.max(320, wrap.clientWidth - (wrap.clientWidth % 8));
    const h = Math.max(200, wrap.clientHeight - (wrap.clientHeight % 8));
    try {
      sc.resize_window(0, w, h, 32, 0, 0);
    } catch {
      // resize_window throws if the agent channel isn't ready yet —
      // safe to ignore; the onsuccess handler retries once it is.
    }
  }, []);

  useEffect(() => {
    if (!Number.isFinite(vmId)) return;
    let cancelled = false;

    (async () => {
      let session;
      try {
        session = await createSpiceSession(vmId);
      } catch (e) {
        if (!cancelled) {
          setState("error");
          setReason((e as Error).message);
        }
        return;
      }
      if (cancelled) return;

      const proto = window.location.protocol === "https:" ? "wss:" : "ws:";
      const uri = `${proto}//${window.location.host}/api/vms/${vmId}/spice/ws?session=${encodeURIComponent(session.session_id)}`;

      const sc = new SpiceMainConn({
        uri,
        password: session.password,
        screen_id: screenDomId,
        onsuccess: () => setState("open"),
        // The agent channel connects after the main + display
        // channels — only here is it safe to fire resize_window,
        // because send_agent_message_queue silently drops messages
        // while agent_connected is false (main.js:349). onagent fires
        // whenever spice-vdagent (re)connects in the guest, so this
        // also picks up the case where the user logs into Windows
        // and the agent starts up post-handshake.
        onagent: () => requestGuestResize(),
        onerror: (e) => {
          setState("error");
          setReason(typeof e === "string" ? e : ((e as Error)?.message ?? "SPICE error"));
        },
      });
      scRef.current = sc;
    })();

    // Debounced resize: window.resize fires per pixel during a drag,
    // and each call asks the guest to reconfigure its framebuffer —
    // heavy work for the guest. 200 ms matches the upstream
    // handle_resize debounce in spice-html5's resize.js.
    let resizeTimer: number | undefined;
    const onWindowResize = () => {
      if (resizeTimer !== undefined) window.clearTimeout(resizeTimer);
      resizeTimer = window.setTimeout(requestGuestResize, 200);
    };
    window.addEventListener("resize", onWindowResize);

    return () => {
      cancelled = true;
      window.removeEventListener("resize", onWindowResize);
      if (resizeTimer !== undefined) window.clearTimeout(resizeTimer);
      if (scRef.current) {
        try {
          scRef.current.stop();
        } catch {
          // SpiceMainConn.stop() throws if already torn down; safe to ignore.
        }
        scRef.current = null;
      }
    };
  }, [vmId, screenDomId, requestGuestResize]);

  const onCtrlAltDel = useCallback(() => {
    if (!scRef.current) return;
    try {
      sendCtrlAltDel(scRef.current);
    } catch (e) {
      toast.error("Ctrl-Alt-Del failed", { description: (e as Error).message });
    }
  }, []);

  const onDisconnect = useCallback(() => {
    if (!scRef.current) return;
    try {
      scRef.current.stop();
    } catch {
      // stop() throws on second call; ignore.
    }
    scRef.current = null;
    setState("closed");
  }, []);

  if (!Number.isFinite(vmId)) {
    return <p className="text-destructive">Invalid VM id.</p>;
  }

  return (
    // h-[calc(100vh-7rem)] on the outer wrapper reserves the page
    // chrome (top nav + this header strip) and lets the screen card
    // fill what's left. The exact 7rem matches the layout's 14-unit
    // header plus the toolbar margin — adjust if the chrome ever
    // changes height.
    <div className="flex h-[calc(100vh-7rem)] flex-col gap-3">
      <div className="flex flex-wrap items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to={`/vms/${vmId}`}>
            <ArrowLeft className="h-4 w-4" />
            VM #{vmId}
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">Graphical console</h1>
          <ConnBadge state={state} />
        </div>
        <div className="ml-auto flex gap-2">
          <Button
            variant="outline"
            size="sm"
            disabled={state !== "open"}
            onClick={onCtrlAltDel}
            title="Send Ctrl+Alt+Del to the guest (browser-side keyboard would be caught by the host)."
          >
            <Keyboard className="h-3.5 w-3.5" />
            Send Ctrl+Alt+Del
          </Button>
          <Button variant="outline" size="sm" disabled={state === "closed"} onClick={onDisconnect}>
            Disconnect
          </Button>
        </div>
      </div>

      {state === "error" && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-base text-destructive">
              <Zap className="h-4 w-4" />
              Connection failed
            </CardTitle>
            <CardDescription>{reason || "Unknown SPICE error."}</CardDescription>
          </CardHeader>
        </Card>
      )}
      {state === "closed" && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Disconnected</CardTitle>
            <CardDescription>
              Reload the page to reconnect — each session uses a fresh single-use grant.
            </CardDescription>
          </CardHeader>
        </Card>
      )}

      <Card className="min-h-0 flex-1">
        <CardContent className="h-full p-3">
          {/* Two-layer host:
               - wrapperRef: viewport-bounded outer box, the one we
                 measure for resize_window requests. Scrolls when the
                 guest's framebuffer doesn't (or can't) match.
               - screen_id div: library-managed inner element. Its
                 width/height are set imperatively by spice-html5 when
                 the display channel surfaces arrive, so we leave its
                 CSS alone. */}
          <div
            ref={wrapperRef}
            className="h-full w-full overflow-auto rounded-md"
            style={{ background: "#0b1220" }}
          >
            <div id={screenDomId} />
          </div>
        </CardContent>
      </Card>

      <p className="text-xs text-muted-foreground">
        Browser SPICE has feature limits compared to <code>crv vm view</code>: no audio, no USB
        redirect, no 3D acceleration. Locale-specific keys (non-US layouts) may misbehave — use the
        native viewer for serious work. Auto-resize requires <code>spice-vdagent</code> in the
        guest.
      </p>
    </div>
  );
}

function ConnBadge({ state }: { state: ConnState }) {
  const map: Record<ConnState, { label: string; className: string }> = {
    connecting: {
      label: "connecting…",
      className: "bg-amber-500/15 text-amber-400",
    },
    open: {
      label: "connected",
      className: "bg-emerald-600/15 text-emerald-400",
    },
    closed: {
      label: "disconnected",
      className: "bg-muted text-muted-foreground",
    },
    error: {
      label: "error",
      className: "bg-destructive/15 text-destructive",
    },
  };
  const { label, className } = map[state];
  return (
    <span
      className={`inline-flex items-center gap-1 rounded-md border border-transparent px-2 py-0.5 text-xs font-medium ${className}`}
    >
      <Circle className="h-2 w-2 fill-current" />
      {label}
    </span>
  );
}
