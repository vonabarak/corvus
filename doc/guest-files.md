# Guest file transfers

Copy a single binary file between the client machine and a running VM with
QEMU Guest Agent enabled:

```sh
crv vm upload my-vm ./config.bin /tmp/config.bin
crv vm download my-vm /tmp/config.bin ./copy.bin
crv vm download my-vm /tmp/config.bin # writes ./config.bin
crv vm upload windows-vm ./data.bin 'C:\Windows\Temp\data.bin'
crv --output json vm download my-vm /tmp/config.bin ./copy.bin
```

The download command's local destination is optional. If omitted, it uses the
guest file's name in the current working directory, for both Linux and Windows
guest paths.

Paths identify exact files. Existing files are overwritten; parent directories
must exist. Directory recursion, permissions preservation, and stdin/stdout
transfers are not supported. JSON success reports `bytes_transferred`. Uploads
are tracked as VM tasks; downloads are read-only operations.

The byte stream follows client → daemon → nodeagent → QGA for uploads and
the reverse for downloads. Native `guest-file-open`, `guest-file-write`,
`guest-file-flush`, `guest-file-read`, and `guest-file-close` commands support
Linux and Windows without a shell or base64 executable in the guest.
Transfers use at most 32 KiB per chunk with one outstanding request. Short
guest writes are retried for the remaining bytes; malformed reads fail.

A download writes a sibling temporary file and replaces the destination only
after the guest handle closes, the sink ends, and byte counts match. On
failure, the existing destination is preserved and the temporary file is
removed. Uploads write directly to the guest path and can leave truncated
or partial data on failure.

Each QGA response has a five-second timeout and each stream callback a
60-second inactivity timeout. There is no whole-file deadline. QGA command
serialization is released while awaiting client callbacks so health probes
can proceed. A changed QGA connection fails the transfer rather than
replaying file commands against another guest session. Handle cleanup is
bounded and preserves the original error.

`Vm.uploadFile` accepts a scoped `ByteSource`; `Vm.downloadFile` accepts a
`ByteSink`. Python sync and async VMs expose `upload_from_file(local, guest)`
and `download_to_file(guest, local)`, returning the byte count. This feature
uses protocol version 5; deploy matching daemon, nodeagent, and client builds.

Build `file:` steps reuse the upload Action, preserving parent-directory
creation and chmod. Leading build YAML `upload:` directives import disk
images/media through the disk-upload API instead; see
[image builds](image-builds.md#file).
