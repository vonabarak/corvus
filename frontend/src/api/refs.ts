/**
 * Cross-entity reference shape.
 *
 * Wherever one corvus entity references another (a drive's disk image,
 * a VM's node, a NIC's network, …) the JSON / REST payload carries a
 * nested `{id, name}` object rather than flat `<role>_id` /
 * `<role>_name` keys. This interface is the TypeScript mirror of
 * Haskell's `Corvus.Protocol.NamedRef` and Python's
 * `corvus_client.types.NamedRef`.
 *
 * Optional references appear as `NamedRef | null` — the wire sentinel
 * `id == 0` is translated to `null` at the corvus-web boundary.
 *
 * See CLAUDE.md `## Project Rules / Cross-entity references`.
 */
export interface NamedRef {
  id: number;
  name: string;
}
