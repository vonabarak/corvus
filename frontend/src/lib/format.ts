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
