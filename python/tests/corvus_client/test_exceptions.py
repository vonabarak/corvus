"""Translation of daemon KjException messages to typed Python exceptions.

The daemon renders errors as "<code> :: <message>" (see
`src/Corvus/Wire/Error.hs` and the `ErrorCode` enum in
`schema/enums.capnp`); the translator maps the code to a typed
exception and keeps the message part as the human-readable body.
"""

from __future__ import annotations

import re
from pathlib import Path

import capnp
import pytest
from corvus_client.exceptions import (
    AmbiguousRef,
    CorvusError,
    DiskHasOverlays,
    DiskInUse,
    DiskNotFound,
    DriveNotFound,
    FormatNotSupported,
    GuestAgentError,
    GuestAgentNotEnabled,
    InvalidTransition,
    NetIfNotFound,
    NetworkAlreadyRunning,
    NetworkInUse,
    NetworkNotFound,
    NetworkNotRunning,
    NodeInUse,
    NodeNotFound,
    ProtocolError,
    ServerError,
    SharedDirNotFound,
    SnapshotNotFound,
    SshKeyInUse,
    SshKeyNotFound,
    TaskNotFound,
    TemplateNotFound,
    VmHeadless,
    VmMustBeStopped,
    VmNotFound,
    VmNotRunning,
    VmRunning,
    translate_kj_exception,
)

ENVELOPE = "(remote):0: failed: remote exception: "


def _fake_kj(description: str) -> capnp.KjException:
    """Build a KjException carrying the given .description for unit tests."""
    return capnp.KjException(description)


# The wire codes (minus `invalid_transition`, whose constructor shape is
# special-cased) mapped to the exception class the translator must raise.
_CODE_TO_EXC = {
    "vm_not_found": VmNotFound,
    "disk_not_found": DiskNotFound,
    "snapshot_not_found": SnapshotNotFound,
    "drive_not_found": DriveNotFound,
    "network_not_found": NetworkNotFound,
    "netif_not_found": NetIfNotFound,
    "ssh_key_not_found": SshKeyNotFound,
    "shared_dir_not_found": SharedDirNotFound,
    "template_not_found": TemplateNotFound,
    "task_not_found": TaskNotFound,
    "node_not_found": NodeNotFound,
    "disk_in_use": DiskInUse,
    "disk_has_overlays": DiskHasOverlays,
    "vm_must_be_stopped": VmMustBeStopped,
    "vm_not_running": VmNotRunning,
    "vm_headless": VmHeadless,
    "network_in_use": NetworkInUse,
    "network_already_running": NetworkAlreadyRunning,
    "network_not_running": NetworkNotRunning,
    "ssh_key_in_use": SshKeyInUse,
    "node_in_use": NodeInUse,
    "format_not_supported": FormatNotSupported,
    "guest_agent_not_enabled": GuestAgentNotEnabled,
    "guest_agent_error": GuestAgentError,
    "ambiguous_ref": AmbiguousRef,
    "internal_error": ServerError,
    "protocol_error": ProtocolError,
}


def test_code_map_matches_schema_enum():
    # The `ErrorCode` enum in schema/enums.capnp is the single source
    # of truth; the client's _CODE_MAP must cover exactly its snake_case
    # tokens (guards against Haskell/Python drift).
    from corvus_client.exceptions import _CODE_MAP

    schema = (
        Path(__file__).resolve().parents[3] / "schema" / "enums.capnp"
    ).read_text()
    block = re.search(r"enum ErrorCode \{(.*?)\}", schema, re.DOTALL)
    assert block is not None, "ErrorCode enum missing from schema/enums.capnp"
    tokens = set()
    for member in re.findall(r"\b([A-Za-z0-9]+)\s+@\d+;", block.group(1)):
        snake = re.sub(r"(?<!^)(?=[A-Z])", "_", member).lower()
        tokens.add(snake)
    assert set(_CODE_MAP) == tokens


@pytest.mark.parametrize("code,expected", sorted(_CODE_TO_EXC.items()))
def test_translate_codes_to_typed_exceptions(code, expected):
    out = translate_kj_exception(_fake_kj(f"{ENVELOPE}{code} :: something went wrong"))
    assert isinstance(out, expected)
    assert isinstance(out, CorvusError)
    # The message part (not the code prefix) is the body.
    assert str(out) == "something went wrong"


def test_translate_strips_remote_envelope():
    exc = _fake_kj(f"{ENVELOPE}vm_not_found :: VM 'web-1' not found")
    out = translate_kj_exception(exc)
    # The exception's str() should be the bare daemon message, not the
    # whole pycapnp envelope.
    assert str(out) == "VM 'web-1' not found"
    assert isinstance(out, VmNotFound)


def test_vm_not_running_and_headless_stay_in_the_vmruntime_family():
    # corvus_web catches VmRunning for HTTP 409; the split codes must
    # stay in that family so the gateway keeps working.
    out = translate_kj_exception(
        _fake_kj(f"{ENVELOPE}vm_not_running :: VM not running")
    )
    assert isinstance(out, VmNotRunning)
    assert isinstance(out, VmRunning)
    out = translate_kj_exception(
        _fake_kj(f"{ENVELOPE}vm_headless :: VM has no SPICE display")
    )
    assert isinstance(out, VmHeadless)
    assert isinstance(out, VmRunning)


def test_translate_invalid_transition_extracts_status_and_reason():
    # The daemon's `statusOrThrow` in Corvus.Rpc.Vm emits the message
    # part in this exact format; the translator must extract both halves
    # so the UI can tell the operator which state the VM is in AND why
    # the action was refused.
    exc = _fake_kj(
        f"{ENVELOPE}invalid_transition :: invalid transition from stopped: "
        "Network 'corvus' is not running"
    )
    out = translate_kj_exception(exc)
    assert isinstance(out, InvalidTransition)
    assert out.status == "stopped"
    assert out.reason == "Network 'corvus' is not running"
    # The composed message includes both halves for human display.
    assert "stopped" in str(out)
    assert "Network 'corvus' is not running" in str(out)


def test_translate_invalid_transition_from_various_states():
    # The daemon's lower-case enumToText covers every VmStatus
    # constructor; spot-check a few that operators commonly hit.
    for status in ("running", "starting", "saving", "loading", "migrating"):
        exc = _fake_kj(
            f"{ENVELOPE}invalid_transition :: invalid transition from {status}: "
            f"in-flight operation"
        )
        out = translate_kj_exception(exc)
        assert isinstance(out, InvalidTransition)
        assert out.status == status


def test_translate_ambiguous_ref():
    msg = "VM 'shared' is ambiguous: 2 matches across nodes; use the numeric id"
    out = translate_kj_exception(_fake_kj(f"{ENVELOPE}ambiguous_ref :: {msg}"))
    assert isinstance(out, AmbiguousRef)
    assert str(out) == msg


def test_message_with_delimiter_round_trips():
    # A message that itself contains the delimiter must survive: the
    # split happens on the first delimiter after the known code.
    out = translate_kj_exception(_fake_kj(f"{ENVELOPE}vm_not_found :: a :: b :: c"))
    assert isinstance(out, VmNotFound)
    assert str(out) == "a :: b :: c"


def test_translate_unknown_code_degrades_to_server_error():
    # New daemon, old client: the code is not in the client's map, so
    # the whole message is kept as the body of a generic error.
    out = translate_kj_exception(_fake_kj(f"{ENVELOPE}totally_unknown :: VM not found"))
    assert isinstance(out, ServerError)
    assert str(out) == "totally_unknown :: VM not found"


def test_translate_legacy_message_degrades_to_server_error():
    # Old daemon (no code prefix): the entire message is the body.
    out = translate_kj_exception(_fake_kj(f"{ENVELOPE}VM 'web-1' not found"))
    assert isinstance(out, ServerError)
    assert str(out) == "VM 'web-1' not found"


def test_translate_unknown_message_degrades_to_server_error():
    out = translate_kj_exception(_fake_kj(f"{ENVELOPE}something weird"))
    assert isinstance(out, ServerError)
    assert str(out) == "something weird"
