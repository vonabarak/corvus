import { useCallback, useEffect, useRef, useState } from "react";
import { Link, useParams } from "react-router-dom";
import { Terminal } from "@xterm/xterm";
import { FitAddon } from "@xterm/addon-fit";
import { ArrowLeft, Circle, Maximize2, Zap } from "lucide-react";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import "@xterm/xterm/css/xterm.css";

type ConnState = "connecting" | "open" | "closed" | "error";

interface TermSize {
  cols: number;
  rows: number;
}

// UTF-8 encoder for keystrokes + the resize helper. Lives at module
// scope because TextEncoder has no state — sharing avoids a tiny alloc
// on every keystroke.
const ENCODER = new TextEncoder();

/**
 * Serial console page.
 *
 * Wire shape (matches python/corvus_web/routes/vms.py
 * ``serial_console_ws``):
 *   * Server → client: binary frames of raw daemon bytes (incl. the
 *     ~1 MB ring-buffer replay on connect).
 *   * Client → server: binary frames of UTF-8-encoded keystrokes.
 *
 * xterm.js renders the daemon's output; ``onData`` callback ships
 * keystrokes back. FitAddon resizes the grid to the container — note
 * that we don't propagate cols/rows to the daemon: serial consoles
 * are dumb pipes, the guest decides geometry via stty / TERM.
 *
 * Cleanup: the effect's unmount tears down xterm + the websocket; the
 * backend handler cleans up its half (cancels the bridge tasks, closes
 * the ByteStream) when it sees the WS close.
 */
export default function Console() {
  const { id } = useParams<{ id: string }>();
  const vmId = Number(id);

  const containerRef = useRef<HTMLDivElement | null>(null);
  const termRef = useRef<Terminal | null>(null);
  const fitRef = useRef<FitAddon | null>(null);
  const wsRef = useRef<WebSocket | null>(null);

  const [state, setState] = useState<ConnState>("connecting");
  const [reason, setReason] = useState<string>("");
  const [size, setSize] = useState<TermSize>({ cols: 80, rows: 24 });

  useEffect(() => {
    if (!Number.isFinite(vmId) || !containerRef.current) return;

    const term = new Terminal({
      cursorBlink: true,
      convertEol: true,
      fontFamily: '"JetBrains Mono", "Fira Code", ui-monospace, monospace',
      fontSize: 13,
      theme: {
        // Phosphor-green vibe: light green on a near-black background,
        // matching the classic vt220 look. ANSI palette stays at xterm
        // defaults so colorised output (ls --color, htop, …) still
        // reads correctly; only the default foreground / cursor are
        // tinted.
        background: "#0b1220",
        foreground: "#90ee90",
        cursor: "#90ee90",
        selectionBackground: "#33415588",
      },
    });
    const fit = new FitAddon();
    term.loadAddon(fit);
    term.open(containerRef.current);
    // First fit must happen after open(); a second pass on resize lives below.
    fit.fit();
    termRef.current = term;
    fitRef.current = fit;
    setSize({ cols: term.cols, rows: term.rows });

    // The vite dev server proxies /ws to the gateway; production
    // serves both off the same origin. Either way, build the URL
    // off ``location`` so the protocol matches the page (ws:// for
    // http://, wss:// for https://).
    const proto = window.location.protocol === "https:" ? "wss:" : "ws:";
    const url = `${proto}//${window.location.host}/api/vms/${vmId}/serial/ws`;
    const ws = new WebSocket(url);
    ws.binaryType = "arraybuffer";
    wsRef.current = ws;

    const onData = term.onData((data) => {
      if (ws.readyState !== WebSocket.OPEN) return;
      // xterm.js delivers keystrokes as UTF-8 strings; the daemon
      // expects raw bytes. Encode and ship as a binary frame.
      ws.send(ENCODER.encode(data));
    });

    ws.onopen = () => setState("open");
    ws.onmessage = (e) => {
      if (e.data instanceof ArrayBuffer) {
        term.write(new Uint8Array(e.data));
      } else if (typeof e.data === "string") {
        term.write(e.data);
      }
    };
    ws.onerror = () => {
      setState("error");
      setReason("WebSocket error — check that the daemon is running and the VM is started.");
    };
    ws.onclose = (e) => {
      setState((prev) => (prev === "error" ? prev : "closed"));
      if (e.reason && !reason) setReason(e.reason);
    };

    const onResize = () => {
      fit.fit();
      // Keep state in sync so the "Resize guest tty" button always
      // ships the current grid dimensions.
      setSize({ cols: term.cols, rows: term.rows });
    };
    window.addEventListener("resize", onResize);

    return () => {
      window.removeEventListener("resize", onResize);
      onData.dispose();
      ws.close();
      term.dispose();
      termRef.current = null;
      fitRef.current = null;
      wsRef.current = null;
    };
    // We intentionally re-init on vmId change; ``reason`` is read but
    // not a dep — including it would tear down the WS on close-with-reason.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [vmId]);

  // Inject `stty cols X rows Y\n` into the guest. Only safe at a
  // shell prompt — in vim, less, etc. the command would be typed
  // into the editor. We surface this caveat in the button tooltip.
  //
  // Serial consoles are dumb byte pipes: there's no SIGWINCH like
  // SSH carries, so the guest tty stays at its default geometry
  // (typically 80×24) regardless of how wide xterm.js is here. The
  // alternative is asking the user to run `resize` (from xterm or
  // ncurses-bin) inside the guest, or `eval "$(resize)"` in their
  // .bashrc. This button is the click-once shortcut.
  const sendStty = useCallback(() => {
    const ws = wsRef.current;
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    const cmd = `stty rows ${size.rows} cols ${size.cols}\n`;
    ws.send(ENCODER.encode(cmd));
  }, [size]);

  if (!Number.isFinite(vmId)) {
    return <p className="text-destructive">Invalid VM id.</p>;
  }

  return (
    <div className="space-y-4">
      <div className="flex flex-wrap items-center gap-4">
        <Button variant="ghost" size="sm" asChild>
          <Link to={`/vms/${vmId}`}>
            <ArrowLeft className="h-4 w-4" />
            VM #{vmId}
          </Link>
        </Button>
        <div className="flex items-baseline gap-3">
          <h1 className="text-2xl font-semibold tracking-tight">Serial console</h1>
          <ConnBadge state={state} />
          <span className="font-mono text-xs text-muted-foreground">
            {size.cols}×{size.rows}
          </span>
        </div>
        <div className="ml-auto">
          <Button
            variant="outline"
            size="sm"
            disabled={state !== "open"}
            onClick={sendStty}
            title={`Sends 'stty rows ${size.rows} cols ${size.cols}' to the guest. Only safe at a shell prompt — would be typed into vim / less / etc.`}
          >
            <Maximize2 className="h-3.5 w-3.5" />
            Resize guest tty
          </Button>
        </div>
      </div>

      {state === "closed" && (
        <Card>
          <CardHeader>
            <CardTitle className="text-base">Disconnected</CardTitle>
            <CardDescription>
              {reason || "The daemon ended the session. Reload the page to reconnect."}
            </CardDescription>
          </CardHeader>
        </Card>
      )}
      {state === "error" && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-base text-destructive">
              <Zap className="h-4 w-4" />
              Connection failed
            </CardTitle>
            <CardDescription>{reason}</CardDescription>
          </CardHeader>
        </Card>
      )}

      <Card>
        <CardContent className="p-3">
          <div
            ref={containerRef}
            className="h-[calc(100vh-260px)] min-h-[360px] w-full overflow-hidden rounded-md"
            style={{ background: "#0b1220" }}
          />
        </CardContent>
      </Card>
      <div className="space-y-1 text-xs text-muted-foreground">
        <p>
          Daemon-side ring buffer replays the last ~1 MB on connect, so reload doesn't lose context.
          Type to send input; reload to reconnect.
        </p>
        <p>
          Serial is a dumb byte pipe — the guest can't know the terminal size on its own. If
          interactive apps (htop, vim, …) look cramped, click <em>Resize guest tty</em> at a shell
          prompt, or add <code>eval "$(resize)"</code> to your <code>.bashrc</code>.
        </p>
      </div>
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
