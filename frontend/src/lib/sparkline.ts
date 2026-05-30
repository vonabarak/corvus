/**
 * Inline SVG sparkline rendering.
 *
 * The `<polyline points="...">` form is wide-browser-supported and
 * needs no chart library. We pass a sequence of numeric values
 * plus a target pixel box; the helper returns a "points" string the
 * caller pastes into a polyline element.
 */

export interface SparklineLayout {
  /** "x,y x,y …" string suitable for SVG <polyline points={…}>. */
  points: string;
  /** Min and max of the input series (after dropping non-finite values). */
  min: number;
  max: number;
  /** Number of plotted samples (may be < values.length if a leading
   * tail of NaNs got skipped). */
  count: number;
}

/**
 * Map `values` onto a `width × height` pixel box. Samples are spaced
 * uniformly horizontally; the y-axis stretches from `min` to `max`
 * with a small floor so a flat line still renders mid-box.
 */
export function sparklinePoints(values: number[], width: number, height: number): SparklineLayout {
  const clean = values.filter((v) => Number.isFinite(v));
  if (clean.length === 0) {
    return { points: "", min: 0, max: 0, count: 0 };
  }
  const min = Math.min(...clean);
  const max = Math.max(...clean);
  const range = Math.max(max - min, 1e-9);
  const step = clean.length > 1 ? width / (clean.length - 1) : 0;
  const points = clean
    .map((v, i) => {
      const x = i * step;
      // Invert y so larger values render higher on the SVG canvas.
      const y = height - ((v - min) / range) * height;
      return `${x.toFixed(1)},${y.toFixed(1)}`;
    })
    .join(" ");
  return { points, min, max, count: clean.length };
}
