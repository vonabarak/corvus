/**
 * Live resource-usage panel for `VmDetail`.
 *
 * On mount, seeds a ring buffer of up to 60 prior samples from
 * `GET /api/vms/{id}/stats/history`. Then opens a WebSocket to
 * `/api/vms/{id}/stats/ws` and appends each incoming frame to the
 * ring (capacity 60 == 10-minute window at the agent's 10s cadence).
 *
 * Renders four cards: CPU, RAM, Disk I/O, Net I/O. Each shows the
 * current rate (computed from the most recent pair of samples) and
 * a 10-minute sparkline.
 */

import { useEffect, useRef, useState } from "react";

import { getVmStatsHistory, type DriveIo, type NetIo, type VmStats } from "@/api/vms";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { useWebSocketJson } from "@/hooks/useWebSocketJson";
import { sparklinePoints } from "@/lib/sparkline";

const RING_CAPACITY = 60;
const SPARK_WIDTH = 160;
const SPARK_HEIGHT = 32;
const STALE_THRESHOLD_MS = 30_000;

export function ResourceUsageCard({ vmId, vmCpuCount }: { vmId: number; vmCpuCount: number }) {
  const [samples, setSamples] = useState<VmStats[]>([]);
  const ring = useRef<VmStats[]>([]);

  // 1. One-shot history fetch to seed the ring.
  useEffect(() => {
    let cancelled = false;
    getVmStatsHistory(vmId)
      .then((hist) => {
        if (!cancelled && hist.length > 0) {
          ring.current = hist.slice(-RING_CAPACITY);
          setSamples([...ring.current]);
        }
      })
      .catch(() => {
        // Empty history is fine — the WS will start filling it.
      });
    return () => {
      cancelled = true;
    };
  }, [vmId]);

  // 2. Live stream.
  const ws = useWebSocketJson<VmStats>(`/api/vms/${vmId}/stats/ws`);
  useEffect(() => {
    if (!ws.last) return;
    ring.current = [...ring.current, ws.last].slice(-RING_CAPACITY);
    setSamples([...ring.current]);
  }, [ws.last]);

  if (samples.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Resource Usage</CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-muted-foreground">Waiting for the first sample…</p>
        </CardContent>
      </Card>
    );
  }

  const latest = samples[samples.length - 1];
  const prior = samples.length >= 2 ? samples[samples.length - 2] : null;
  const stale = isStale(latest);

  return (
    <Card>
      <CardHeader>
        <CardTitle>
          Resource Usage
          {stale && (
            <span className="ml-2 text-xs font-normal text-muted-foreground">(sample stale)</span>
          )}
        </CardTitle>
      </CardHeader>
      <CardContent>
        <div
          className={`grid grid-cols-1 gap-4 md:grid-cols-2 lg:grid-cols-4 ${
            stale ? "opacity-50" : ""
          }`}
        >
          <CpuTile samples={samples} vmCpuCount={vmCpuCount} />
          <MemoryTile latest={latest} samples={samples} />
          <DiskTile latest={latest} prior={prior} samples={samples} />
          <NetTile latest={latest} prior={prior} samples={samples} />
        </div>
      </CardContent>
    </Card>
  );
}

// ---------------------------------------------------------------------------
// Tiles

function CpuTile({ samples, vmCpuCount }: { samples: VmStats[]; vmCpuCount: number }) {
  // CPU is a cumulative jiffies counter — convert consecutive samples
  // into a per-interval rate, scaled to "cores used" (i.e. a value of
  // 1.0 means 100% of one vCPU; 2.0 means 200% etc.).
  const series = samples.map((s, i) => {
    if (i === 0) return NaN;
    const prev = samples[i - 1];
    if (s.clk_tck === 0 || s.interval_millis === 0) return NaN;
    const deltaJiffies = s.cpu_jiffies_total - prev.cpu_jiffies_total;
    const seconds = deltaJiffies / s.clk_tck;
    const wallSec = s.interval_millis / 1000;
    return wallSec > 0 ? seconds / wallSec : NaN;
  });
  const last = lastFinite(series);
  const pct = last !== null && vmCpuCount > 0 ? (last / vmCpuCount) * 100 : null;
  return (
    <Tile
      label="CPU"
      headline={pct !== null ? `${pct.toFixed(1)} %` : "—"}
      sub={last !== null ? `${last.toFixed(2)} of ${vmCpuCount} vCPUs` : "warming up"}
      series={series}
    />
  );
}

function MemoryTile({ latest, samples }: { latest: VmStats; samples: VmStats[] }) {
  const balloonLine =
    latest.balloon_max_bytes > 0
      ? `${formatBytes(latest.balloon_actual_bytes)} / ${formatBytes(latest.balloon_max_bytes)} (balloon)`
      : "no balloon device";
  // Sparkline for RAM uses host RSS directly (a gauge, not a counter).
  const series = samples.map((s) => s.host_rss_bytes);
  return (
    <Tile
      label="RAM"
      headline={`${formatBytes(latest.host_rss_bytes)} RSS`}
      sub={balloonLine}
      series={series}
    />
  );
}

function DiskTile({
  latest,
  prior,
  samples,
}: {
  latest: VmStats;
  prior: VmStats | null;
  samples: VmStats[];
}) {
  const driveCount = latest.drives.length;
  const rate = aggregateDriveRate(latest, prior);
  // Sparkline plots aggregate (read+write) bytes-per-sec across
  // consecutive sample pairs.
  const series = samples.map((s, i) => {
    if (i === 0) return NaN;
    return aggregateDriveRate(s, samples[i - 1])?.total ?? NaN;
  });
  return (
    <Tile
      label="Disk I/O"
      headline={rate ? `▼ ${formatRate(rate.read)} · ▲ ${formatRate(rate.write)}` : "—"}
      sub={`${driveCount} drive${driveCount === 1 ? "" : "s"}`}
      series={series}
    />
  );
}

function NetTile({
  latest,
  prior,
  samples,
}: {
  latest: VmStats;
  prior: VmStats | null;
  samples: VmStats[];
}) {
  const tapCount = latest.nets.length;
  const rate = aggregateNetRate(latest, prior);
  const series = samples.map((s, i) => {
    if (i === 0) return NaN;
    return aggregateNetRate(s, samples[i - 1])?.total ?? NaN;
  });
  return (
    <Tile
      label="Net I/O"
      headline={rate ? `▼ ${formatBitsPerSec(rate.rx)} · ▲ ${formatBitsPerSec(rate.tx)}` : "—"}
      sub={`${tapCount} interface${tapCount === 1 ? "" : "s"}`}
      series={series}
    />
  );
}

function Tile({
  label,
  headline,
  sub,
  series,
}: {
  label: string;
  headline: string;
  sub: string;
  series: number[];
}) {
  const { points } = sparklinePoints(series, SPARK_WIDTH, SPARK_HEIGHT);
  return (
    <div className="rounded-md border bg-card p-3">
      <div className="text-xs font-medium uppercase tracking-wide text-muted-foreground">
        {label}
      </div>
      <div className="mt-1 text-lg font-semibold">{headline}</div>
      <div className="text-xs text-muted-foreground">{sub}</div>
      <svg
        width={SPARK_WIDTH}
        height={SPARK_HEIGHT}
        className="mt-2 w-full text-primary"
        viewBox={`0 0 ${SPARK_WIDTH} ${SPARK_HEIGHT}`}
        preserveAspectRatio="none"
      >
        {points && <polyline points={points} fill="none" stroke="currentColor" strokeWidth="1.5" />}
      </svg>
    </div>
  );
}

// ---------------------------------------------------------------------------
// Helpers

function lastFinite(xs: number[]): number | null {
  for (let i = xs.length - 1; i >= 0; i -= 1) {
    if (Number.isFinite(xs[i])) return xs[i];
  }
  return null;
}

function isStale(s: VmStats): boolean {
  if (s.sampled_at_nanos === 0) return true;
  const nowMs = Date.now();
  const sampleMs = s.sampled_at_nanos / 1_000_000;
  return nowMs - sampleMs > STALE_THRESHOLD_MS;
}

function aggregateDriveRate(
  cur: VmStats,
  prior: VmStats | null,
): { read: number; write: number; total: number } | null {
  if (!prior || cur.interval_millis === 0) return null;
  const wallSec = cur.interval_millis / 1000;
  // Sum across drives so the headline summarises the VM, not one
  // drive. Per-drive breakdown belongs in a future tab.
  const sumKey = (xs: DriveIo[], k: keyof DriveIo) => xs.reduce((a, x) => a + (x[k] as number), 0);
  const read =
    (sumKey(cur.drives, "read_bytes_total") - sumKey(prior.drives, "read_bytes_total")) / wallSec;
  const write =
    (sumKey(cur.drives, "write_bytes_total") - sumKey(prior.drives, "write_bytes_total")) / wallSec;
  return { read, write, total: read + write };
}

function aggregateNetRate(
  cur: VmStats,
  prior: VmStats | null,
): { rx: number; tx: number; total: number } | null {
  if (!prior || cur.interval_millis === 0) return null;
  const wallSec = cur.interval_millis / 1000;
  const sumKey = (xs: NetIo[], k: keyof NetIo) => xs.reduce((a, x) => a + (x[k] as number), 0);
  const rx = (sumKey(cur.nets, "rx_bytes_total") - sumKey(prior.nets, "rx_bytes_total")) / wallSec;
  const tx = (sumKey(cur.nets, "tx_bytes_total") - sumKey(prior.nets, "tx_bytes_total")) / wallSec;
  return { rx, tx, total: rx + tx };
}

function formatBytes(n: number): string {
  const u = ["B", "KiB", "MiB", "GiB", "TiB"];
  let i = 0;
  let x = n;
  while (x >= 1024 && i < u.length - 1) {
    x /= 1024;
    i += 1;
  }
  return `${x.toFixed(i === 0 ? 0 : 1)} ${u[i]}`;
}

function formatRate(bytesPerSec: number): string {
  return `${formatBytes(Math.max(0, bytesPerSec))}/s`;
}

function formatBitsPerSec(bytesPerSec: number): string {
  const bits = Math.max(0, bytesPerSec) * 8;
  const u = ["bps", "kbps", "Mbps", "Gbps"];
  let i = 0;
  let x = bits;
  while (x >= 1000 && i < u.length - 1) {
    x /= 1000;
    i += 1;
  }
  return `${x.toFixed(i === 0 ? 0 : 1)} ${u[i]}`;
}
