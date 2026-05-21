"""SPICE protocol probe used by the integration tests.

The daemon binds QEMU's SPICE listener on the node's
@127.0.0.1:<spice_port>@ (see [src/Corvus/Qemu/Command.hs] —
@addr=...,port=<port>,disable-ticketing=off@). The host can't
reach that address directly, so the probe runs from inside the
node via an SSH-relayed Python script and the host parses the
raw reply bytes.

The probe sends a @SpiceLinkMess@ for the MAIN channel and reads
back the 16-byte @SpiceLinkHeader@. A successful exchange proves
the SPICE channel is up and speaking the protocol — the
post-link ticket handshake is deliberately skipped (we'd need the
short-lived password from @vm.view_grant@ and an RSA-encrypted
ticket; the link reply alone is sufficient evidence the listener
is live).

Constants are mirrored from @spice-protocol@'s
@/usr/include/spice-1/spice/protocol.h@ (Gentoo
@net-libs/spice-protocol@):

  * @SPICE_MAGIC = "REDQ"@
  * @SPICE_VERSION_MAJOR = 2@
  * @SPICE_VERSION_MINOR = 2@
  * @SPICE_CHANNEL_MAIN = 1@ (from @spice/enums.h@)
"""

from __future__ import annotations

import base64
import shlex
import shutil
import struct
import subprocess
from dataclasses import dataclass
from pathlib import Path

SPICE_MAGIC: bytes = b"REDQ"
SPICE_VERSION_MAJOR: int = 2
SPICE_VERSION_MINOR: int = 2
SPICE_CHANNEL_MAIN: int = 1

# struct SpiceLinkHeader { u32 magic; u32 major; u32 minor; u32 size; }
_LINK_HEADER = struct.Struct("<4sIII")
# struct SpiceLinkMess { u32 connection_id; u8 channel_type; u8 channel_id;
#                        u32 num_common_caps; u32 num_channel_caps;
#                        u32 caps_offset; }
_LINK_MESS = struct.Struct("<IBBIII")


@dataclass(frozen=True)
class SpiceLinkInfo:
    """Parsed @SpiceLinkHeader@ returned by the SPICE server."""

    magic: bytes
    major: int
    minor: int
    body_size: int


def build_link_frame(channel_type: int = SPICE_CHANNEL_MAIN) -> bytes:
    """Build a @SpiceLinkHeader@ + @SpiceLinkMess@ frame with no caps.

    The frame is what a SPICE client sends as the very first bytes on
    a freshly-opened TCP connection; QEMU validates the magic and
    replies with its own header + @SpiceLinkReply@ (which carries a
    1024-bit RSA pubkey and is too large to fit in our 16-byte read
    — we only parse the header).
    """
    body = _LINK_MESS.pack(
        0,  # connection_id — 0 lets the server allocate one
        channel_type,
        0,  # channel_id
        0,  # num_common_caps
        0,  # num_channel_caps
        _LINK_MESS.size,  # caps_offset — past the end since no caps
    )
    header = _LINK_HEADER.pack(
        SPICE_MAGIC,
        SPICE_VERSION_MAJOR,
        SPICE_VERSION_MINOR,
        _LINK_MESS.size,
    )
    return header + body


def parse_link_reply_header(buf: bytes) -> SpiceLinkInfo:
    """Parse the first 16 bytes returned by the SPICE server. Raises
    @RuntimeError@ if the magic doesn't match @SPICE_MAGIC@."""
    if len(buf) < _LINK_HEADER.size:
        raise RuntimeError(
            f"SPICE reply truncated: expected ≥{_LINK_HEADER.size} bytes, "
            f"got {len(buf)}: {buf.hex()!r}"
        )
    magic, major, minor, size = _LINK_HEADER.unpack(buf[: _LINK_HEADER.size])
    if magic != SPICE_MAGIC:
        raise RuntimeError(
            f"not speaking SPICE: bad magic {magic!r} (expected {SPICE_MAGIC!r})"
        )
    return SpiceLinkInfo(magic=magic, major=major, minor=minor, body_size=size)


def probe_spice_link(
    *,
    node_cid: int,
    spice_port: int,
    host_key_path: Path,
    user: str = "corvus",
    node_port: int = 22,
    timeout_sec: float = 10.0,
) -> SpiceLinkInfo:
    """Open a TCP connection to @127.0.0.1:spice_port@ inside the
    node, send a SPICE MAIN-channel link frame, and parse the reply
    header.

    The transport is `ssh corvus@vsock-<node_cid> -- python3 -c
    '<script>'`. The script base64-decodes the link frame from its
    argv, connects, sends, reads 16 bytes, and prints the hex of
    what it got — keeps the binary payload out of the shell
    quoting/encoding path.

    Raises @RuntimeError@ on any failure: SSH error, TCP connect
    refused inside the node, short read, or bad magic.
    """
    if not shutil.which("ssh"):
        raise RuntimeError("`ssh` not on PATH")
    if not shutil.which("socat"):
        raise RuntimeError("`socat` not on PATH; required for VSOCK SSH")
    if not host_key_path.exists():
        raise RuntimeError(
            f"SSH private key not found at {host_key_path} — "
            "run `make test-image-key` to generate it"
        )

    payload_b64 = base64.b64encode(build_link_frame()).decode("ascii")

    # Script runs on the node (Gentoo; python3 is in @system):
    # connects to qemu's SPICE listener on 127.0.0.1, sends the link
    # frame, reads up to 16 bytes (the SpiceLinkHeader), prints the
    # hex on stdout. We don't read the full SpiceLinkReply body — the
    # header alone carries the magic + version that this probe
    # asserts on.
    #
    # The script is shipped to python3 via @exec(base64.b64decode(...))@
    # to sidestep shell-quoting of the multi-line program.
    node_script = (
        f"import base64, socket, sys\n"
        f"p = base64.b64decode('{payload_b64}')\n"
        f"s = socket.create_connection(('127.0.0.1', {spice_port}), "
        f"timeout={timeout_sec})\n"
        f"s.sendall(p)\n"
        f"buf = b''\n"
        f"while len(buf) < 16:\n"
        f"    chunk = s.recv(16 - len(buf))\n"
        f"    if not chunk: break\n"
        f"    buf += chunk\n"
        f"sys.stdout.write(buf.hex())\n"
    )
    script_b64 = base64.b64encode(node_script.encode("ascii")).decode("ascii")
    bootstrap = f"import base64; exec(base64.b64decode('{script_b64}'))"

    # `ssh` joins remote-command argv with literal spaces — no
    # quoting — and ships the result to the remote shell, so we
    # must pre-quote the bootstrap ourselves. Without this the
    # bootstrap's `;` would split it across multiple shell commands.
    remote_cmd = f"python3 -c {shlex.quote(bootstrap)}"

    proxy_cmd = f"socat - VSOCK-CONNECT:{node_cid}:{node_port}"
    argv = [
        "ssh",
        "-i",
        str(host_key_path),
        "-o",
        "StrictHostKeyChecking=no",
        "-o",
        "UserKnownHostsFile=/dev/null",
        "-o",
        "BatchMode=yes",
        "-o",
        f"ProxyCommand={proxy_cmd}",
        f"{user}@vsock-{node_cid}",
        remote_cmd,
    ]
    proc = subprocess.run(
        argv,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        timeout=timeout_sec + 5,
    )
    if proc.returncode != 0:
        raise RuntimeError(
            f"SPICE probe SSH exited {proc.returncode}: "
            f"stderr={proc.stderr.decode('utf-8', 'replace').strip()!r}"
        )
    hex_reply = proc.stdout.decode("ascii", errors="replace").strip()
    try:
        reply = bytes.fromhex(hex_reply)
    except ValueError as exc:
        raise RuntimeError(
            f"SPICE probe returned non-hex stdout: {hex_reply!r}"
        ) from exc
    return parse_link_reply_header(reply)
