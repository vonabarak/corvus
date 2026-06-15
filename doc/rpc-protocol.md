# Corvus RPC Protocol (Cap'n Proto)

Corvus speaks [Cap'n Proto](https://capnproto.org/) RPC between the
`corvus` daemon and the `crv` CLI. The wire schema is the source of
truth, lives under [`schema/`](../schema/), and can be consumed
directly from any language with a Cap'n Proto binding (Python,
TypeScript, Rust, …). This document gives a high-level tour and a
runnable Python example.

## Files

| File | Purpose |
|---|---|
| `schema/corvus.capnp` | Root `Daemon` interface; entry point for everything. |
| `schema/common.capnp` | Cross-cutting types: `EntityRef`, `StatusInfo`, `ViewGrant`. |
| `schema/enums.capnp` | All 10 enums (`VmStatus`, `DriveFormat`, `TaskResult`, …). |
| `schema/vm.capnp` | `VmManager` + `Vm` cap; per-VM lifecycle, snapshots, console. |
| `schema/disk.capnp` | `DiskManager` + `Disk` cap; CRUD, overlays, snapshots. |
| `schema/network.capnp` | `NetworkManager` + `Network` cap; virtual networks. |
| `schema/sshkey.capnp` | `SshKeyManager` + `SshKey` cap. |
| `schema/template.capnp` | `TemplateManager` + `Template` cap. |
| `schema/task.capnp` | `TaskManager` + `Task` cap; async-op history + progress sub. |
| `schema/cloudinit.capnp` | `CloudInitManager` + custom user-data CRUD. |
| `schema/streams.capnp` | Streaming sinks: `ByteSink`, `BuildEventSink`, `ApplyEventSink`, `DiskDownloadSink`, `GuestAgentStatusSink`, `TaskProgressSink`, plus `Handle` token. |

## Capability tree

The bootstrap cap is `Daemon`. Every subsystem hangs off it:

```
Daemon
├── vms       → VmManager → Vm (lifecycle, console, snapshots…)
├── disks     → DiskManager → Disk (overlays, snapshots…)
├── networks  → NetworkManager → Network
├── sshKeys   → SshKeyManager → SshKey
├── templates → TemplateManager → Template
├── tasks     → TaskManager → Task
├── cloudInit → CloudInitManager
├── ping, status, shutdown          (local methods)
├── apply (yaml, skipExisting, wait) → (result, taskId)
└── build (yaml, sink :BuildEventSink) → (taskId)
```

Resource caps (`Vm`, `Disk`, …) are first-class objects on the wire.
Once you hold a `Vm` cap, calling `.show()` on a deleted VM gives a
clean `notFound` exception rather than a daemon crash or a stale
ID confusion.

## Entity references

Everywhere the legacy protocol accepted a textual `Ref` that the
daemon parsed as an integer ID or a name, the Cap'n Proto schema
uses a union:

```capnp
struct EntityRef {
  union {
    id   @0 :Int64;
    name @1 :Text;
  }
}
```

Third-party clients pick the branch explicitly, with no ambiguity.
The `crv` CLI keeps the old single-token UX (`crv vm start web-1`
or `crv vm start 42`): it parses bare digits as `id`, otherwise
`name`, and encodes the union locally. Names that happen to be all
digits are unaddressable by name from the CLI; library users can
construct an `EntityRef.name = "42"` to reach them.

## Streaming

The `schema/streams.capnp` file declares six sink caps the daemon
either consumes or produces:

- **`ByteSink`** -- generic byte pipe. Used by `Vm.serialConsole` /
  `Vm.hmpMonitor` in **both** directions: the caller supplies a
  `ByteSink` for daemon-to-client bytes, and the daemon returns
  another `ByteSink` for client-to-daemon bytes.
- **`BuildEventSink`** -- one-way push of `BuildEvent`s during a
  `Daemon.build` invocation. Caller supplies the sink and gets the
  parent task id back synchronously.
- **`ApplyEventSink`** -- one-way push of `ApplyEvent`s during a
  `Daemon.apply` invocation. Each phase of the declarative apply
  (`sshKeys`, `disks`, `networks`, `vms`, `templates`) emits a
  `phaseStart` followed by per-entity `entityStart` / `entityEnd`
  pairs; disk imports that download from a URL emit
  `downloadStart` / `downloadProgress` / `downloadEnd` between the
  disk's `entityStart` and `entityEnd`. Closed with a single
  `applyEnd` followed by `end()`.
- **`DiskDownloadSink`** -- one-way `progress(downloaded, total)`
  pushes from a node agent back to the daemon during a
  `Session.diskDownload` URL transfer. `total == 0` until
  Content-Length is probed (or stays 0 if the server didn't return
  one). Internal to the daemon/node-agent path; not exposed to CLI
  clients.
- **`GuestAgentStatusSink`** -- one-way push from
  `Vm.subscribeGuestAgent`. Each poll cycle the daemon pushes a
  `GuestAgentStatus` (reachable, enabled, last healthcheck) for
  every subscriber.
- **`TaskProgressSink`** -- one-way push from
  `TaskManager.subscribe`. The schema defines `started`,
  `progress`, and `finished` variants; the daemon currently emits
  only the terminal `finished` event when the watched task
  completes.

> **Flow control.** The Haskell `capnp` library doesn't yet
> implement the `stream` keyword's automatic flow control, so the
> schema methods all return `()`. Senders are expected to keep a
> small window of in-flight `push` calls; the daemon does this
> conservatively (one call at a time) for now.

## Lifetimes & subscription handles

Subscription methods (`Vm.subscribeGuestAgent`,
`TaskManager.subscribe`) return a `Handle` cap. The cap has no
methods -- it's purely a lifetime token. Drop it on the client side
(let GC sweep it, or `release` it) and the daemon's next push to
the corresponding sink will fail, after which the subscriber is
pruned. There's no explicit `unsubscribe` method.

## Errors

Errors are conveyed as Cap'n Proto exceptions. The daemon throws
strings like `"VM not found"`, `"Serial console buffer not
available"`, validation messages, etc. Pure result responses
(state transitions, ids of newly-created entities) stay as
struct returns.

## Python client

Corvus ships a Python package at [`python/corvus_client/`](../python/corvus_client/)
that wraps the schema via [pycapnp](https://capnproto.github.io/pycapnp/).
For most Python use cases, that package is what you want — see
[`python/README.md`](../python/README.md) for the full reference.

Quick sync example:

```python
# pip install -e ./python
from corvus_client import Client

with Client(unix_socket="/run/user/1000/corvus/corvus.sock") as c:
    info = c.status()
    print(f"Corvus {info.version}, up {info.uptime_seconds}s")
    for vm in c.vms.list():
        print(f"  [{vm.id}] {vm.name} -- {vm.status}")
```

Async equivalent:

```python
import asyncio, capnp
from corvus_client import AsyncClient

async def main():
    async with capnp.kj_loop():
        async with AsyncClient(unix_socket="/run/user/1000/corvus/corvus.sock") as c:
            info = await c.status()
            print(f"Corvus {info.version}, up {info.uptime_seconds}s")
            for vm in await c.vms.list():
                print(f"  [{vm.id}] {vm.name} -- {vm.status}")

asyncio.run(main())
```

### Raw pycapnp (other languages / advanced cases)

The schema can also be driven directly from any pycapnp client without
the helper package. pycapnp 2.x is async-first; under
`capnp.kj_loop()`, every cap method returns an awaitable promise.

```python
# pip install pycapnp
import asyncio
import capnp

SCHEMA = "/path/to/corvus/schema"
common  = capnp.load(f"{SCHEMA}/common.capnp")
streams = capnp.load(f"{SCHEMA}/streams.capnp", imports=[SCHEMA])
vm      = capnp.load(f"{SCHEMA}/vm.capnp",      imports=[SCHEMA])
corvus  = capnp.load(f"{SCHEMA}/corvus.capnp",  imports=[SCHEMA])

async def main():
    async with capnp.kj_loop():
        stream = await capnp.AsyncIoStream.create_unix_connection(
            "/run/user/1000/corvus/corvus.sock"
        )
        client = capnp.TwoPartyClient(stream)
        daemon = client.bootstrap().cast_as(corvus.Daemon)

        info = (await daemon.status()).info
        print(f"Corvus {info.version}, up {info.uptimeSeconds}s")

        mgr = (await daemon.vms()).mgr
        vms = (await mgr.list()).vms
        for v in vms:
            print(f"  [{v.id}] {v.name} -- {v.status}")

        if vms:
            ref = common.EntityRef.new_message()
            ref.id = vms[0].id
            h = (await mgr.get(ref=ref)).vm
            await h.start(wait=True)
            print(f"started {vms[0].name}")

asyncio.run(main())
```

### Build streaming (raw pycapnp)

```python
class BuildEventSinkImpl(streams.BuildEventSink.Server):
    async def push(self, event, _context):
        which = event.which()
        if which == "logLine":
            print(event.logLine)
        elif which == "stepStart":
            s = event.stepStart
            print(f"[step {s.stepIndex} {s.name}] {s.command}")
        elif which == "buildEnd":
            be = event.buildEnd
            print("ok" if be.success else f"FAIL: {be.errorMessage}")

    async def end(self, _context):
        pass

sink = BuildEventSinkImpl()
yaml_text = open("pipeline.yml").read()
resp = await daemon.build(yaml=yaml_text, sink=sink)
print(f"Build started; task id {resp.taskId}")
# The daemon calls sink.push(...) repeatedly, then sink.end().
```

The `corvus_client` package handles this for you: see
`AsyncClient.build_stream(yaml_path)` and the YAML preprocessing
helper that inlines `shell.script` / `file.from` / `floppy.from`
references before sending.

### Guest-agent subscription (raw pycapnp)

```python
class GAStatusSinkImpl(streams.GuestAgentStatusSink.Server):
    async def push(self, status, _context):
        print(f"vm {status.vmId}: reachable={status.reachable}, "
              f"last_hc_nanos={status.lastHealthcheck}")

mgr = (await daemon.vms()).mgr
ref = common.EntityRef.new_message(); ref.name = "web-1"
vm_cap = (await mgr.get(ref=ref)).vm
handle = (await vm_cap.subscribeGuestAgent(sink=GAStatusSinkImpl())).handle
# Keep `handle` alive for as long as you want to receive events;
# dropping it (or closing the connection) unsubscribes.
```

## Regenerating bindings

The Haskell side is regenerated with `make capnp`, which runs
`stack exec capnp compile -ohaskell …` against the whole
`schema/` directory and drops the modules into `src-generated/`.
The same target syncs the schemas into `python/corvus_client/schema/`
so the Python package ships the same contract. Add any new Haskell
module to `package.yaml`'s exposed list before shipping.

For Python bindings, `pycapnp` reads the `.capnp` files at runtime
(`capnp.load`), so no codegen step is required.
