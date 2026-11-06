import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import ts from "typescript";
import { stringify } from "lossless-json";

// Execute the actual TypeScript helpers without adding a test runtime dependency.
async function load(relative) {
  const source = await readFile(new URL(relative, import.meta.url), "utf8");
  const compiled = ts
    .transpileModule(source, {
      compilerOptions: { target: ts.ScriptTarget.ES2022, module: ts.ModuleKind.ES2022 },
    })
    .outputText.replace('"lossless-json"', JSON.stringify(import.meta.resolve("lossless-json")));
  return import(`data:text/javascript;base64,${Buffer.from(compiled).toString("base64")}`);
}
const { parseSize, formatBytes } = await load("../src/lib/format.ts");
const { decodeJson } = await load("../src/api/client.ts");

test("binary suffixes preserve exact small and large sizes", () => {
  for (const bytes of [1n, 512n, 1537n, 1073741824n, 9007199254740993n, 9223372036854775807n]) {
    assert.equal(parseSize(formatBytes(bytes)), bytes);
  }
  assert.equal(parseSize("1g"), 1073741824n);
  assert.equal(formatBytes(0n), "0B");
  assert.equal(formatBytes(null), "—");
  for (const invalid of ["1", "0B", "1.5G", "-1M", "8388608T"]) {
    assert.throws(() => parseSize(invalid));
  }
  assert.throws(() => parseSize("1537B", true));
});

test("JSON capacities remain exact numeric byte tokens above Number.MAX_SAFE_INTEGER", () => {
  const bytes = 9223372036854775807n;
  const encoded = stringify({ id: 7, size: bytes, ram: 1073741824n });
  assert.equal(encoded, '{"id":7,"size":9223372036854775807,"ram":1073741824}');
  assert.deepEqual(decodeJson(encoded), { id: 7, size: bytes, ram: 1073741824n });
});

test("REST stats keep number arithmetic and match WebSocket JSON above the safe integer limit", () => {
  const encoded = stringify([
    {
      sampled_at_nanos: 1793981293653098083n,
      interval_millis: 10000,
      cpu_jiffies_total: 9007199254740993n,
      clk_tck: 100,
      host_rss_bytes: 9007199254740993n,
      balloon_actual_bytes: 17179869184,
      balloon_max_bytes: 17179869184,
      drives: [{ read_bytes_total: 9007199254740993n, write_bytes_total: 9007199254740993n }],
      nets: [{ rx_bytes_total: 9007199254740993n, tx_bytes_total: 9007199254740993n }],
    },
  ]);
  const [sample] = decodeJson(encoded);
  assert.deepEqual(sample, JSON.parse(encoded)[0]);
  assert.ok(Number.isFinite(sample.sampled_at_nanos / 1_000_000));
  assert.ok(Number.isFinite(sample.cpu_jiffies_total / sample.clk_tck));
  assert.doesNotThrow(() => formatBytes(sample.host_rss_bytes));
  for (const counters of [sample.drives[0], sample.nets[0]]) {
    for (const value of Object.values(counters)) {
      assert.ok(Number.isFinite((value - 100) / (sample.interval_millis / 1000)));
    }
  }
});

test("all capacity fields stay bigint alongside numeric fields and nullable capacities", () => {
  const capacities = Object.fromEntries(
    [
      "ram",
      "size",
      "ram_total",
      "ram_free",
      "storage_bytes_total",
      "storage_bytes_free",
      "total_size",
    ].map((key) => [key, 9223372036854775807n]),
  );
  const payload = { capacities, nullable: { size: null }, load_avg1: 0.125, cpu_count: 8 };
  assert.deepEqual(decodeJson(stringify(payload)), payload);
});
