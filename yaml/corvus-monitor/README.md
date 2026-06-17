# corvus-monitor

A pre-baked Debian 12 image with **Prometheus** and **Grafana**
installed and configured to scrape Corvus daemon metrics exported
by `corvus-web` (see
[`python/corvus_web/routes/metrics.py`](../../python/corvus_web/routes/metrics.py)).

Boot a VM from the resulting image, point Prometheus at your
`corvus-web` host, open Grafana on port 3000, and watch live
per-VM and per-node telemetry.

## What's inside

| Service | Port | Purpose |
|---|---|---|
| Prometheus | 9090 | Scrapes `/metrics` from `corvus-web`. |
| Grafana | 3000 | Renders the pre-imported `Corvus Overview` dashboard. |

Configuration files (baked into the image):

```
/etc/prometheus/prometheus.yml                       # scrape config (1 corvus-web target)
/etc/grafana/grafana.ini                             # listen on 0.0.0.0:3000
/etc/grafana/provisioning/datasources/corvus.yaml    # Prometheus datasource auto-imported
/etc/grafana/provisioning/dashboards/corvus.yaml     # filesystem dashboard provider
/var/lib/grafana/dashboards/corvus.json              # the dashboard JSON
```

The dashboard covers all 15 metrics corvus-web exports today:
per-VM CPU / RSS / balloon / disk-I/O / net-I/O counters and
per-node load + memory + storage gauges.

## Build

```bash
# 1. Ensure the debian12 template + its base disk exist.
crv apply yaml/multi-os/multi-os.yml

# 2. Bake the image. This takes a few minutes — apt pulls
#    Prometheus + Grafana over the network.
crv build yaml/corvus-monitor/corvus-monitor.yml

# 3. Confirm the disk landed.
crv disk list | grep corvus-monitor
```

On success a non-ephemeral Corvus disk named `corvus-monitor` is
registered.

## Deploy a monitoring VM

```bash
# Overlay so the VM gets its own writable copy.
crv disk overlay corvus-monitor-vm corvus-monitor

# 2 CPU / 2 GB RAM is plenty for Prometheus + Grafana scraping
# a single Corvus deployment.
crv vm create corvus-monitor-vm \
  --cpus 2 --ram 2048 --guest-agent --cloud-init

crv disk attach corvus-monitor-vm corvus-monitor-vm
crv vm net-if add corvus-monitor-vm --type user
crv vm start corvus-monitor-vm
```

Point a browser at the VM's IP on port 3000. Default login is
**admin / admin** (Grafana forces a password change on first
login).

## Pointing Prometheus at your corvus-web

The image ships with a placeholder scrape target,
`host.local:8080`. From the monitoring VM's point of view,
`127.0.0.1` is the VM itself — not the host running corvus-web —
so the target has to be a hostname or IP the VM can route to.

### Option A — quick edit on the live VM

```bash
crv vm guest exec corvus-monitor-vm -- \
  sed -i 's|host.local:8080|<your-corvus-web-host>:8080|' \
  /etc/prometheus/prometheus.yml

crv vm guest exec corvus-monitor-vm -- systemctl reload prometheus
```

Open `http://<vm-ip>:9090/targets` to confirm the `corvus-web`
job's `State` is `UP`.

### Option B — re-bake with the right target

Edit
[`files/prometheus.yml`](files/prometheus.yml) before running
`crv build`. The new value is baked into every VM cloned from the
resulting image — useful when you're standing up multiple
monitoring nodes from the same image.

### Multi-node Corvus deployments

corvus-web aggregates metrics from every node it connects to —
one scrape target is usually enough. If you run multiple
independent corvus-web instances, list them all under `targets:`:

```yaml
- job_name: corvus-web
  metrics_path: /metrics
  static_configs:
    - targets:
        - host-a:8080
        - host-b:8080
```

Prometheus will scrape each in parallel and label the timeseries
with `instance="host-a:8080"` etc.

## Authentication

`/metrics` is unauthenticated by design today (see
[`python/corvus_web/config.py`](../../python/corvus_web/config.py)).
Don't expose corvus-web on a routable interface without an
upstream reverse proxy that gates `/metrics`.

## Customising the dashboard

The Grafana dashboard is filesystem-provisioned with
`allowUiUpdates: true`, so you can edit panels in the UI for
quick iteration. To persist changes, **export** the dashboard JSON
from the UI (`Share → Export → Save to file`) and overwrite
[`files/corvus-dashboard.json`](files/corvus-dashboard.json),
then re-bake.

Grafana's filesystem provider polls the dashboard directory every
30 s, so dropping a new `corvus.json` over SSH also works
without restarting Grafana.

## Troubleshooting

### `crv build` errors with "shell agent error: vmGuestExecStream: unknown vmId N"

The bake VM's QEMU guest agent (QGA) never first-pinged the
daemon within the 300 s wait the node-agent enforces during
first-boot of a cloud-init VM. The agent force-stops QEMU at the
timeout, and the next shell provisioner then sees the VM is gone.

**Why:** the upstream `debian-12-generic-cloud.qcow2` image does
NOT ship `qemu-guest-agent` pre-installed. The
[`debian12` template](../multi-os/multi-os.yml) in
`yaml/multi-os/multi-os.yml` installs it on first boot via
cloud-init:

```yaml
cloudInitConfig:
  userData:
    packages:
      - qemu-guest-agent
    runcmd:
      - systemctl enable --now qemu-guest-agent
```

When cloud-init sees `packages:` it runs `apt-get update` first,
then `apt-get install`. On hosts where the bake VM's network can't
reach apt mirrors within ~5 min (slow mirror, nested-KVM
overhead, no apt-cacher), cloud-init blocks here long enough for
the daemon's QGA-wait to fire.

The same failure surfaces in `yaml/debian-nginx/debian-nginx.yml`
and any other build that uses the stock `debian12` template — it
is not specific to this build.

**Why the `test_debian` integration test still passes:** that
test (`integration_tests/tests/test_cloud_init.py::TestCloudInit::test_debian`)
creates its VM directly from `debian-12-generic-base` *without*
the `debian12` template — so the bake VM uses the daemon's
default cloud-init user-data (no `packages:` list,
`package_update: false`) and finishes in ~13 s. It also sets
`guest_agent=False` on the VM so the daemon's 300 s QGA-wait
doesn't engage at all. The test proves cloud-init itself is fast
on this host; it doesn't exercise the slow `apt-get install`
path that bakes hit.

**Confirm the diagnosis** by timing how long Debian's cloud-init
reaches `boot-finished` with versus without the
`apt-get install qemu-guest-agent` step. With the default
user-data: ~13 s. With `packages: [qemu-guest-agent]`: > 5 min on
this host.

**Fixes**, in order of operator effort:

* **Run an apt-cacher** (`apt-cacher-ng`) on the host so the bake
  VM's apt-get update + install reads from local cache. Cheapest
  if you have multiple Debian-family builds.
* **Pre-install QGA into a derivative base disk** with
  `virt-customize`:
  ```bash
  virt-customize -a ~/VMs/BaseImages/Debian/debian-12-generic-base.qcow2 \
    --install qemu-guest-agent \
    --run-command 'systemctl enable qemu-guest-agent'
  ```
  After this, drop the `packages:` line from the template's
  userData (keep only the `runcmd:` to enable). The bake VM's
  first boot then needs only ~13 s + 1 s for the systemctl enable.
* **Switch the build to a template that ships QGA pre-installed**
  (e.g. `gentoo-cloud` on this host boots cleanly in ~75 s). This
  requires rewriting the build's `apt` / `systemctl` provisioners
  for the new distro's package manager.
* **Run the build on a faster host.** Bare-metal with a nearby
  Debian mirror completes the first-boot install in well under
  the 300 s window.

## Updating the metric inventory

If `corvus-web`'s `/metrics` exports a new family, add panels to
`files/corvus-dashboard.json` and rebuild. The list of metrics
lives at the top of
[`python/corvus_web/routes/metrics.py`](../../python/corvus_web/routes/metrics.py)
and is intentionally short — the file is the source of truth.
