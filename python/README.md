# `corvus_client` — Python client for Corvus

Pure-Python client for the [Corvus](../) VM management daemon, speaking
Cap'n Proto RPC via [pycapnp](https://capnproto.github.io/pycapnp/) over
a Unix socket or TCP.

Two API faces are provided:

- `corvus_client.Client` — synchronous. Holds a background asyncio +
  `capnp.kj_loop()` thread; every method is a blocking call.
- `corvus_client.AsyncClient` — asynchronous. Use as an `async with`
  inside `capnp.kj_loop()`.

Both faces share the same async core, so feature parity is automatic.

## Install

The package isn't on PyPI yet. From a checkout:

```sh
pip install -e .
```

Dependencies: `pycapnp >= 2.2.0`, `PyYAML >= 6.0`.

The `.capnp` schema files ship inside the package
(`corvus_client/schema/`). To work against the live source tree, point
`CORVUS_SCHEMA_DIR` at `<repo>/schema/`.

The wheel includes a `py.typed` marker. Mypy and other PEP 561 checkers can
use the annotations on `Client`, `AsyncClient`, their managers, resource
wrappers, and the dataclasses in `corvus_client.types`. For example:

```python
from corvus_client import Client
from corvus_client.types import VmDetails

with Client(unix_socket="/run/corvus/corvus.sock") as client:
    details: VmDetails = client.vms.get("web-1").show()
    client.vms.create("worker", ram=2 * 1024**3)  # checked keyword arguments
```

Build/apply streams yield typed event dataclasses followed by a
`("task_id", int)` tuple; use `isinstance(item, tuple)` before accessing
event fields. Async subscription callbacks receive typed event dataclasses.
`client.daemon` and `entity_ref()` expose the raw, dynamic pycapnp boundary;
they are not part of the typed wrapper API.

## Sync example

```python
from corvus_client import Client

with Client(unix_socket="/run/user/1000/corvus/corvus.sock") as c:
    info = c.status()
    print(f"Corvus {info.version}, up {info.uptime_seconds}s")

    # List + show
    for vm in c.vms.list():
        print(f"  [{vm.id}] {vm.name} -- {vm.status}")

    # Get a specific VM (id or name; bare-digit strings resolve to id,
    # matching the `crv` CLI heuristic).
    vm = c.vms.get("web-1")
    details = vm.show()
    print(details.cpu_count, "vCPUs,", details.ram, "bytes")

    # Mutate
    vm.edit(ram=2 * 1024**3)
    vm.start(wait=True)
```

## Async example

```python
import asyncio
import capnp
from corvus_client import AsyncClient


async def main():
    async with capnp.kj_loop():
        async with AsyncClient(unix_socket="/run/.../corvus.sock") as c:
            info = await c.status()
            print(f"Corvus {info.version}, up {info.uptime_seconds}s")
            vms = await c.vms.list()
            for v in vms:
                print(f"  [{v.id}] {v.name} -- {v.status}")


asyncio.run(main())
```

## Errors

Failed calls raise typed exceptions from `corvus_client.exceptions`:

```python
from corvus_client import Client, VmNotFound, DiskInUse

with Client(unix_socket="...") as c:
    try:
        c.vms.get("ghost")
    except VmNotFound as e:
        print(f"no such VM: {e}")
```

All exception types inherit from `corvus_client.CorvusError`. Use that
as the catch-all if you want to handle anything daemon-side.

Unknown messages from the daemon become `ServerError` rather than
crashing the client, so a daemon upgrade that adds new error strings
won't break existing code immediately.

## Streaming

### Build pipelines

`AsyncClient.build_stream(yaml_path)` (or `Client.build_stream`) reads
a YAML pipeline file, inlines `shell.script:` / `file.from:` references, runs
leading `upload:` media declarations from the client, then streams the
daemon's `BuildEvent`s back:

```python
from corvus_client.types import BuildStepStart, BuildBuildEnd

with Client(unix_socket="...") as c:
    for item in c.build_stream("pipeline.yml"):
        if isinstance(item, BuildStepStart):
            print(f"[step {item.step_index}] {item.name}: {item.command}")
        elif isinstance(item, BuildBuildEnd):
            if item.success:
                print(f"OK; disk id {item.artifact_disk_id}")
            else:
                print(f"FAIL: {item.error_message}")
        elif isinstance(item, tuple) and item[0] == "task_id":
            print(f"daemon task id: {item[1]}")
```

The async generator equivalent is `AsyncClient.build_stream(...)`.

### Guest-agent subscriptions

```python
import asyncio, capnp
from corvus_client import AsyncClient


async def main():
    async with capnp.kj_loop():
        async with AsyncClient(unix_socket="...") as c:
            vm = await c.vms.get("web-1")

            async def on_status(s):
                print(
                    f"vm {s.vm_id}: reachable={s.reachable}, last_hc={s.last_healthcheck}"
                )

            sub = await vm.subscribe_guest_agent(on_status)
            try:
                await asyncio.sleep(60)  # receive events for a minute
            finally:
                await sub.close()  # drop subscription


asyncio.run(main())
```

`AsyncTaskManager.subscribe(task_id, on_event)` works the same way for
task-progress streams.

### Serial console / HMP monitor

`AsyncVm.serial_console()` and `AsyncVm.hmp_monitor()` return a
`ByteStream` with `read()` / `write(chunk)` / `close()` methods.

## Coverage

The Python wrappers mirror every cap method in the Haskell client
(`Corvus.Client.Capnp.Rpc`):

- **VM**: `vms.list/get/create`; `Vm.show/start/stop/pause/reset/edit/delete/set_balloon`,
  `cloud_init`, `view_grant`, `guest_exec`, `send_ctrl_alt_del`, attach/detach
  disks, network ifs, shared dirs, snapshots, SSH keys, serial console,
  HMP monitor, guest-agent subscriptions, console flush.
- **Disk**: list/get/create/register/createOverlay/clone/rebase/importUrl/import;
  show, delete, refresh, resize; snapshotCreate/list/get.
- **Snapshot**: show, delete, rollback, merge.
- **Network**: list/get/create; show/start/stop/edit/delete.
- **SshKey**: list/get/create; show/delete.
- **Template**: list/get/create; show/delete/instantiate/update.
- **Task**: list/get/listChildren/subscribe.
- **CloudInit**: set/get/delete.
- **Daemon**: ping, status, shutdown, apply, build.

## Tests

Run against a real daemon (a fresh `corvus` subprocess per module on a
temp Unix socket + temp SQLite DB):

```sh
make python-test
```

The daemon binary is taken from `~/.local/bin/corvus` (preferred) or
`$PATH`; override with `$CORVUS_BIN`.

### Manual memory balloon adjustment

```python
vm.set_balloon(target_bytes=768 * 1024**2)
# With AsyncClient:
await vm.set_balloon(target_bytes=1024 * 1024**2)
```

`target_bytes` is a positive integer byte count, representing the absolute
guest RAM target. Lower targets inflate the balloon; higher targets deflate it.
The VM must be running with a balloon device and a ready guest driver.
Missing devices raise `BalloonDeviceNotEnabled`; inactive drivers raise
`BalloonDriverNotReady`. Out-of-range targets raise `InvalidBalloonTarget`,
and node/QMP failures raise `BalloonError`. The target cannot exceed
its live RAM ceiling. The call returns on QEMU acceptance, before guest
convergence. Configured RAM and scheduler reservations remain unchanged.
