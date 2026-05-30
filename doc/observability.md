# Observability

Corvus emits per-VM and per-node resource metrics that integrate with
the standard Prometheus + Grafana stack. The agent's `StatusPoller`
samples every running VM every 10 seconds; samples flow agent → daemon
→ `corvus-web`, which exposes them in three shapes:

| Consumer    | Endpoint                                  | Shape                                       |
| ----------- | ----------------------------------------- | ------------------------------------------- |
| `crv vm show` | (one-shot)                              | Most-recent sample only, human-readable     |
| WebUI       | `/api/vms/{id}/stats/{ws,history}`        | Live JSON stream + 10-minute history (60 samples) |
| Prometheus  | `/metrics`                                | Standard text format, counters + gauges     |

The Prometheus surface is the canonical home for long-window history
(hours, days, weeks). Corvus itself only retains the most recent 60
samples per VM in the daemon (10 minutes at the 10s cadence) — enough
to seed the WebUI sparkline panel on page load.

## Scrape configuration

corvus-web is one process per cluster, so Prometheus scrapes a single
target and gets every VM on every node, distinguished by labels:

```yaml
# /etc/prometheus/prometheus.yml — minimal scrape config.
scrape_configs:
  - job_name: corvus
    metrics_path: /metrics
    scheme: http                # use https + tls_config if corvus-web is fronted by mTLS
    static_configs:
      - targets: ["corvus-web.internal:8080"]
```

The scrape cadence is the operator's call. The agent samples every
10 seconds and the cached values change at most that fast, so a 15s
or 30s Prometheus scrape interval is a sensible default.

## Metric reference

All metric names are prefixed with `corvus_`. Counters end in
`_total`; everything else is a gauge.

### Per-VM (labels: `vm_id`, `vm_name`, `node`)

| Metric                                  | Type    | Notes                                              |
| --------------------------------------- | ------- | -------------------------------------------------- |
| `corvus_vm_up`                          | gauge   | 1 when a recent sample exists; absent otherwise.  |
| `corvus_vm_cpu_seconds_total`           | counter | utime + stime / `clk_tck`. Convert to %vCPU via `rate() * 100`. |
| `corvus_vm_memory_rss_bytes`            | gauge   | Host RSS (`/proc/<qemu-pid>/status:VmRSS`).        |
| `corvus_vm_memory_balloon_actual_bytes` | gauge   | `query-balloon` actual; 0 if no balloon device.    |
| `corvus_vm_memory_balloon_max_bytes`    | gauge   | VM's RAM ceiling; 0 if no balloon device.          |
| `corvus_vm_disk_read_bytes_total`       | counter | Per-drive (label `drive`). Cumulative since QEMU launch. |
| `corvus_vm_disk_write_bytes_total`      | counter | Per-drive. Cumulative.                             |
| `corvus_vm_disk_read_ops_total`         | counter | Per-drive read I/O ops.                            |
| `corvus_vm_disk_write_ops_total`        | counter | Per-drive write I/O ops.                           |
| `corvus_vm_net_rx_bytes_total`          | counter | Per-TAP (label `tap`). Cumulative.                 |
| `corvus_vm_net_tx_bytes_total`          | counter | Per-TAP. Cumulative.                               |

### Per-node (label: `node`)

| Metric                            | Type  | Source                                |
| --------------------------------- | ----- | ------------------------------------- |
| `corvus_node_load1`               | gauge | `/proc/loadavg` field 1.              |
| `corvus_node_memory_total_bytes`  | gauge | `/proc/meminfo:MemTotal`.             |
| `corvus_node_memory_free_bytes`   | gauge | `/proc/meminfo:MemAvailable`.         |
| `corvus_node_storage_total_bytes` | gauge | `statvfs(basePath)` block accounting. |
| `corvus_node_storage_free_bytes`  | gauge | Same, free bytes.                     |

## Common queries

```promql
# Per-VM CPU%: 100% of one vCPU == 100. Multiply by 1/cpu_count
# for a 0..100% scale across an N-vCPU VM.
rate(corvus_vm_cpu_seconds_total[1m]) * 100

# Disk throughput (MiB/s read + write) summed per VM.
sum by (vm_name, node) (
  rate(corvus_vm_disk_read_bytes_total[1m])
  + rate(corvus_vm_disk_write_bytes_total[1m])
) / 1048576

# Top-5 noisy-neighbor VMs by write IOPS in the last 5 min.
topk(5, sum by (vm_name, node) (rate(corvus_vm_disk_write_ops_total[5m])))

# Fleet-wide free RAM (across nodes).
sum(corvus_node_memory_free_bytes)

# VMs the daemon hasn't sampled in over a minute.
absent_over_time(corvus_vm_up[1m])
```

## Grafana dashboard (starter JSON)

A minimal dashboard with the four headline panels lives at
[doc/grafana-corvus.json](grafana-corvus.json) (a follow-up may
ship a polished version). The structure is straightforward:

- **Panel 1**: `rate(corvus_vm_cpu_seconds_total[1m]) * 100`, legend
  `{{vm_name}} on {{node}}`.
- **Panel 2**: `corvus_vm_memory_rss_bytes / 1024 / 1024 / 1024`,
  unit GiB, legend same.
- **Panel 3**: stacked `rate(corvus_vm_disk_{read,write}_bytes_total[1m])`,
  unit `binBps`.
- **Panel 4**: stacked `rate(corvus_vm_net_{rx,tx}_bytes_total[1m])`,
  unit `Bps`.

## Authentication

The `/metrics` endpoint shares the corvus-web FastAPI port. Operators
are expected to either firewall the port to their Prometheus instance
or front corvus-web with an mTLS reverse-proxy. There is no separate
auth bypass for the metrics endpoint — if you can reach the WebUI, you
can reach `/metrics`. A dedicated auth path is a future follow-up.

## Implementation notes

- **Caching cadence**: corvus-web polls the daemon every 10 s (matches
  the agent's `StatusPoller` cadence) and stores the latest VmStats /
  NodeStats per id. The `/metrics` endpoint serves from this cache —
  no daemon round-trip per scrape.
- **Stale entries**: cache entries older than 60 s are dropped from the
  output. Prometheus's `absent()` then surfaces gone-VMs.
- **Cardinality**: one series per `(vm, drive)` × 4 disk metrics and
  one series per `(vm, tap)` × 2 net metrics. For a typical 100-VM
  cluster with one drive + one TAP each, that's ~800 series total —
  well within Prometheus's comfort zone.

## See also

- [vm-management.md](vm-management.md#inspecting-resource-usage) — `crv vm show` Resource Usage section.
- [multi-node.md](multi-node.md) — How agent → daemon push works.
- [doc/INDEX.md](INDEX.md) — Full doc tree.
