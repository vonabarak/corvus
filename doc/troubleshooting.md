# Troubleshooting

Start by identifying which connection or component failed: client → daemon,
daemon → nodeagent/netd, or nodeagent → QEMU/guest. Commands below inspect state;
replace example names and task IDs with your own.

## Cannot connect to the daemon

```sh
crv ping
crv --unix ping
systemctl --user status corvus
journalctl --user -u corvus -n 100 --no-pager
```

`crv` defaults to TCP `127.0.0.1:9876` with mutual TLS. Web and desktop clients
default to the local Unix socket. If only one transport works, check the
listener flags, socket path/permissions, and certificates rather than
recreating the database. `CORVUS_HOST`, `CORVUS_PORT`, and `CORVUS_SOCKET` can
override CLI connection settings.

Quickstart uses user services for `corvus` and `corvus-nodeagent`. For a
system-service deployment, use `systemctl status corvus` and
`journalctl -u corvus` instead. A daemon started manually logs to its terminal.
See [connection options](README.md#connection-options).

## Certificate or peer-CN errors

Check that both ends trust the same CA, certificate files are readable by the
service account, and the clocks and certificate validity dates are correct.
A client needs a `corvus-client:` certificate; an agent certificate cannot be
used as a CLI identity. Agent CN names must match the registered node name.
Use `corvus-admin status` to inspect deployed components and the
[security troubleshooting guide](security.md#troubleshooting) for specific
errors. Do not fix a production certificate failure by disabling TLS.

## No online node or an unavailable agent

```sh
crv node list
crv node show my-node
systemctl --user status corvus-nodeagent
journalctl --user -u corvus-nodeagent -n 100 --no-pager
```

A new database needs a registered node. Quickstart registers its local node;
manual installations must deploy agents and register them explicitly. Check
node addresses, agent ports, last status pushes, and administrative state.
Draining or maintenance nodes are excluded from automatic placement. The
nodeagent must reach storage paths and have access to `/dev/kvm`.
See [multi-node setup and recovery](multi-node.md#operational-tips).

## VM will not start, or stays in starting

```sh
crv vm show my-vm
crv task list --all --result error
crv task show 42
```

Inspect the task error and nodeagent logs for missing QEMU/helpers, permissions,
invalid firmware paths, disk locks, or insufficient resources. A disk must have
a placement on the VM's node; paths on another machine are not automatically
shared. [Disk placement](disk-management.md#per-node-placement) explains how to
copy an image version between nodes.

With `guestAgent: true`, `starting` waits for QGA readiness. The guest must
install and run `qemu-guest-agent`; enabling the VM option only adds the channel.
Check [guest-agent troubleshooting](guest-exec.md#troubleshooting) and the
[startup readiness](vm-management.md#startup-readiness). Without guest-agent
readiness, `running` confirms the runtime, not completion of guest boot or
cloud-init. Use [cloud-init diagnostics](cloud-init.md#troubleshooting) inside
the guest when provisioning fails.

For an `error` or stuck lifecycle state, read the error before recovery.
`crv vm reset my-vm` forcibly terminates the runtime; it is not a guest reboot
and may lose unsaved work. It can remain `stopping` if termination cannot be
confirmed. Restore agent connectivity before retrying; see
[lifecycle recovery](vm-lifecycle.md#ordinary-stop-versus-reset).

## No network or guest address

User-mode networking supplies outbound NAT without netd. Inbound connections
need [user-mode port forwarding](networking.md#port-forwarding-user-mode).
A missing displayed guest IP can mean QGA discovery is unavailable, even when
the guest network works.

For managed networks, inspect `crv network list`, `crv net-if list my-vm`, and
`systemctl status corvus-netd`. Check netd logs with
`journalctl -u corvus-netd`. A node registered with netd disabled cannot supply
managed/TAP/bridge/macvtap interfaces. For DNS and bridge configuration, see
[Networking](networking.md).

## Blank console or failed graphical viewer

A headless serial console requires serial output and a getty inside the guest.
Press `Ctrl+]` then `q` to leave the CLI console. For graphical VMs, install a
supported native viewer on the client and verify that it can reach the node's
SPICE endpoint; reaching the daemon RPC port alone is insufficient.

The web SPICE proxy needs the same node reachability from the gateway host.
Browser audio requires supported codecs and may need **Enable sound** after a
user gesture. See [web console limitations](web-interface.md#graphical-console-spice)
and [native SPICE setup](vm-management.md#spice-graphical-vms).

## Web page missing or API returns 502

A missing frontend means the static assets were not built or packaged. Source
installs need `make web-build` and a Python package reinstall, or
`corvus-web --frontend-dir frontend/dist`. Release wheels already contain the
assets. HTTP 502 on daemon-dependent API requests means the gateway is not
connected to the daemon; check its logs and connection flags. It reconnects
in the background but does not replay failed mutations. Inspect tasks before
resubmitting work. See [gateway recovery](web-interface.md#daemon-connection-lifecycle)
and [metrics outage behavior](observability.md#daemon-outages-and-scrape-availability).

## Reporting a problem

Include the Corvus version (`crv status` when available), command or UI action,
task ID/error, node state, and relevant daemon/agent/gateway log excerpts.
Describe whether the deployment uses user or system services, Unix sockets or
TCP, and SQLite or PostgreSQL. Remove private keys, passwords, and other secrets
from logs before sharing them. For test failures, follow the
[integration debugging guide](../integration_tests/README.md#debugging-failed-runs).
