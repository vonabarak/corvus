/** Exact binary sizes for human input and presentation. */
export function parseSize(text: string, ram = false): bigint {
  const match = /^([0-9]+)([BKMGT])$/i.exec(text);
  if (!match) throw new Error("Use a positive integer with B, K, M, G or T (for example 1G).");
  const bytes = BigInt(match[1]) * 1024n ** BigInt("BKMGT".indexOf(match[2].toUpperCase()));
  if (bytes <= 0n || bytes > 9223372036854775807n)
    throw new Error("Size must fit a positive signed 64-bit byte count.");
  if (ram && (bytes < 67108864n || bytes % 1048576n !== 0n))
    throw new Error("RAM must be at least 64M and a whole number of MiB.");
  return bytes;
}

export function formatBytes(bytes: bigint | number | null | undefined): string {
  if (bytes === null || bytes === undefined) return "—";
  const value = typeof bytes === "bigint" ? bytes : BigInt(Math.trunc(bytes));
  if (value === 0n) return "0B";
  for (let power = 4; power >= 0; power--) {
    const factor = 1024n ** BigInt(power);
    if (value % factor === 0n) return `${value / factor}${"BKMGT"[power]}`;
  }
  return `${value}B`;
}
