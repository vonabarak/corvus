# Authentication and Transport Security

Corvus uses **mutual TLS** for every TCP Cap'n Proto link the
daemon, the agents, and the CLI client speak:

```
crv ──────────TLS──────────▶ corvus (daemon)
                                  │
                                  ├─TLS─▶ corvus-nodeagent (per host)
                                  └─TLS─▶ corvus-netd       (per host)
```

Unix-socket connections (the default for ``crv`` ↔ daemon on the
same host) skip TLS — filesystem permissions on the socket are
already enough.

The PKI lives **on the admin's workstation**, not on the daemon
host. A separate Python tool, [`corvus-admin`](../python/corvus_admin/),
runs there to mint certs, push them onto the daemon and agent
hosts via SSH (or to the local filesystem with the special
`local` target), and register nodes with the daemon. The daemon
and agents only ever **load** certs at startup — they don't have
the CA key, so a daemon compromise can't be used to enroll new
attacker-controlled nodes.

## Roles, CN convention, and file layout

Each Corvus component has exactly one cert. The cert filename
matches the role's CN prefix:

| Role              | CN                       | Files                             |
| ----------------- | ------------------------ | --------------------------------- |
| Root CA           | `corvus-ca:<uuid>`       | `ca.crt` *(no key on hosts)*      |
| Daemon            | `corvus-daemon:<uuid>`   | `corvus-daemon.{crt,key}`         |
| Node agent        | `corvus-node:<name>`     | `corvus-node.{crt,key}`           |
| Net agent (netd)  | `corvus-netd:<name>`     | `corvus-netd.{crt,key}`           |
| CLI / Py client   | `corvus-client:<name>`   | `corvus-client.{crt,key}`         |

The Haskell daemon and agents search for their cert trio in:

1. `/etc/corvus/` — the system-install location (`corvus-admin`
   default).
2. `$XDG_CONFIG_HOME/corvus/` (default `~/.config/corvus/`) — for
   the user-systemd-service variant.

**Exception**: client certs are read from
`$XDG_CONFIG_HOME/corvus/` only — never `/etc/corvus`. The
client cert is per-user, so two admins on the same workstation
each have their own.

## CN prefix enforcement

After every handshake the receiving side reads the peer's CN
and validates the prefix. Wrong prefix = connection closed before
any RPC frame is exchanged:

| Listener                  | Required peer CN prefix             | Extra check                   |
| ------------------------- | ----------------------------------- | ----------------------------- |
| Daemon (CLI listener)     | `corvus-client:`                    | —                             |
| Nodeagent                 | `corvus-daemon:` or `corvus-node:`  | —                             |
| Netd                      | `corvus-daemon:`                    | —                             |
| Daemon → nodeagent (dial) | `corvus-node:`                      | suffix == registered node name|
| Daemon → netd (dial)      | `corvus-netd:`                      | suffix == registered node name|
| Nodeagent → nodeagent     | `corvus-node:`                      | —                             |

The nodeagent listener accepts both the orchestrating daemon
**and** other nodeagents. The agent-to-agent path is exercised
only during disk migration: the destination agent dials the
source agent's listener directly to claim a single-use
`DiskReader` capability and stream the bytes (see
[doc/vm-migration.md](vm-migration.md)). The session-scoped
token issued by `session.diskOpenRead` provides per-transfer
authorization on top of the cert-level check.

This is what makes "stole a node's key, used it to impersonate a
CLI client" cryptographically impossible. The CA could sign certs
with arbitrary CNs, but the CA only lives on the admin's
workstation.

## Bootstrap walkthrough

On the admin's workstation, install `corvus-admin` and create a
CA:

```
make install                  # (one-time) installs binaries + pipx-installs corvus-admin
corvus-admin init             # creates CA + admin client cert under
                              # $XDG_CONFIG_HOME/corvus/admin/
```

`init` also drops the admin's own `corvus-client.{crt,key}` into
`$XDG_CONFIG_HOME/corvus/` so `crv` finds it without further
flags.

### Single-host deployment (one command)

For a turn-key single-node setup on the admin's workstation, the
`quickstart` shortcut handles everything: CA generation, all
component certs, systemd unit files (user-mode for daemon and
nodeagent, system-mode for netd), service bring-up, and node
registration.

```
corvus-admin quickstart            # detects sudo or doas; skips netd
                                   # with a warning if neither is found
```

Quickstart always installs daemon + nodeagent as user services.
If you need a system-mode layout for either, use the granular
commands below.

### Single-host deployment (granular)

```
corvus-admin init
corvus-admin deploy daemon local --listen-ip 127.0.0.1 --user-service
corvus-admin deploy node   self local --ip 127.0.0.1
corvus-admin deploy netd   self local --ip 127.0.0.1
corvus-admin register      self --host 127.0.0.1
```

### Multi-host deployment

```
# Assumed: binaries installed on each host (your package manager,
# `stack install`, or `make install`).
corvus-admin deploy daemon root@corvus-master.lan --listen-ip 10.0.0.10
corvus-admin deploy node   alpha root@10.0.0.21 --ip 10.0.0.21
corvus-admin deploy netd   alpha root@10.0.0.21 --ip 10.0.0.21
corvus-admin deploy node   beta  root@10.0.0.22 --ip 10.0.0.22
corvus-admin deploy netd   beta  root@10.0.0.22 --ip 10.0.0.22

corvus-admin register alpha --host 10.0.0.21
corvus-admin register beta  --host 10.0.0.22

corvus-admin status         # one-shot reachability check
crv node list               # daemon's own view of the fleet
```

The `<ssh-target>` argument is parsed by OpenSSH directly, so
`~/.ssh/config` entries (jump hosts, identity files, port aliases)
just work.

## Renewing certs

Default leaf lifetime is 365 days; the CA lives 10 years. The
operator renews periodically:

```
corvus-admin renew daemon
corvus-admin renew node alpha
corvus-admin renew netd alpha
corvus-admin renew client alice
```

`renew` refuses if the cert still has more than 30 days left;
pass `--force` to renew anyway (rare — typically when you suspect
a key leaked). Each `renew` re-uses the original deploy target
from the admin store's index, so the operator doesn't have to
re-type the SSH endpoint.

The daemon's UUID is **stable** across renewals — the daemon
identity stays the same, only the cert + key rotate.

## Disabling TLS for dev

Every Corvus binary supports `--no-tls`, which makes TCP listeners
and dials use plain sockets. **Use only for dev**; if a TCP
listener is exposed on a host with `--no-tls`, anyone with L4
reach to that port can drive it.

Unix-socket connections aren't affected by `--no-tls` because
they never wrap with TLS in the first place.

## Integration tests exercise mTLS end-to-end

The integration test suite (under `integration_tests/`) runs
every test through the full mTLS handshake. The harness builds
its own ephemeral CA per test class (via the same
`corvus_admin.ca` module operators use), drops the cert trio
into `/etc/corvus/` on each test-node VM over SSH-over-VSOCK,
and starts the inner `corvus.service` / `corvus-nodeagent.service`
/ `corvus-netd.service` with TLS on. The test image installs
these units but no longer enables them at bake time — the
harness drives the cert-deploy + start step from
`integration_tests/corvus_test_harness/component_deploy.py`.

Two-node tests pick one of two shapes:

* `OneDaemonTwoNodesCase` — alpha runs daemon + agents, beta
  runs agents only; both share a CA so alpha's daemon validates
  beta's `corvus-node:beta` / `corvus-netd:beta` certs.
* `TwoDaemonsCase` — each node runs its own full stack under
  its own CA. Useful for asserting cross-cluster isolation.

After this layout lands, the test image must be rebuilt
(`make test-image-node`) before the suite passes again. Once
rebuilt, `make integration-tests` is the gate — any visible
`--no-tls` flag in the harness or test-node systemd units
indicates drift.

## Trust model + threat coverage

| Attacker capability | Mitigation |
|---|---|
| L2 sniff between daemon and agent | TLS 1.2/1.3 with mutual cert auth; no plaintext |
| Stolen node cert + key | Daemon's CN check refuses anything but `corvus-node:<that-node>`; attacker can't impersonate a different node or a CLI client |
| Stolen client cert + key | Limited to whatever the named admin can do; revoking is a follow-up (re-deploy CA + reissue, or wait for expiry) |
| Compromised daemon host | Attacker doesn't get the CA key — that's on the admin's workstation. They can disrupt the cluster but can't enroll new nodes. |
| Admin laptop compromise | Attacker gets the CA. Recovery = rotate CA, re-deploy everything. Recommendation: encrypted disk + a backup of `$XDG_CONFIG_HOME/corvus/admin/` |

## Out of scope (explicit follow-ups)

- **Revocation.** No CRL. If a cert leaks, rotate the CA and
  re-deploy. `Node.cert_revoked_at` is a small future
  enhancement.
- **CA rotation with dual-CA overlap window.** Today CA rotation
  is a brief outage — fine for a small fleet, less fine at scale.
- **Automatic renewal.** Daemon does not self-renew; admin runs
  `corvus-admin renew --auto` periodically.
- **TLS over Unix socket.** SO_PEERCRED already gates local
  access.
- **HSM / hardware-backed CA.** `cryptography` supports PKCS#11
  — wire it in when a deployment requires it.

## Troubleshooting

`corvus failed to load TLS material: TlsFilesNotFound …`
  : the daemon / agent couldn't find its cert trio in the search
    path. Fix with `corvus-admin deploy <role> …` or pass
    `--no-tls` for dev.

`peer CN "corvus-node:beta" does not start with required prefix "corvus-client:"`
  : someone is dialing the daemon's TCP listener with the wrong
    cert. Either a misrouted dial (a daemon→agent connection
    hitting the CLI port) or a stolen-cert attempt.

`peer CN "corvus-node:alpha" has name "alpha", expected "beta"`
  : the daemon dialed `beta`'s host but got `alpha`'s cert.
    Likely DNS / port mix-up; check `crv node show beta`'s
    `host` / `nodeAgentPort` against what's actually running.

`corvus-admin status` shows `UNREACHABLE`
  : open `corvus-admin status` with debug logs:
    `corvus-admin status` prints the underlying `ssl.SSLError`
    / `OSError` after the role label. Usually one of: network
    path broken, agent not running, cert mismatch (regen on the
    other side), or a clock skew large enough that the cert is
    not-yet-valid / already-expired.
