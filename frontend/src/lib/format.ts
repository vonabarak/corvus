/**
 * Format an MB count as a short human string.
 *   < 1024 MB        -> "<N> MB"
 *   < 1024 GB        -> "<N.NN> GB"
 *   otherwise        -> "<N.NN> TB"
 */
export function formatMb(mb: number | null | undefined): string {
  if (mb === null || mb === undefined) return "—";
  if (mb < 1024) return `${mb} MB`;
  const gb = mb / 1024;
  if (gb < 1024) return `${gb.toFixed(gb < 10 ? 2 : 1)} GB`;
  const tb = gb / 1024;
  return `${tb.toFixed(tb < 10 ? 2 : 1)} TB`;
}

/**
 * Format a byte count as a short human string (KB/MB/GB/TB). Used for
 * node storage capacities, which the daemon reports in bytes (the
 * agent's df output is exact, not pre-divided).
 */
export function formatBytes(bytes: number | null | undefined): string {
  if (bytes === null || bytes === undefined) return "—";
  const units = ["B", "KB", "MB", "GB", "TB", "PB"];
  let value = bytes;
  let unit = 0;
  while (value >= 1024 && unit < units.length - 1) {
    value /= 1024;
    unit += 1;
  }
  return `${value.toFixed(value < 10 && unit > 0 ? 2 : 1)} ${units[unit]}`;
}
