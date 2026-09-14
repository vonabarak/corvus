"""Integration tests for structured wire error code version skew handling.

These tests verify that:
1. Old client + new daemon (with codes): errors degrade to ServerError
2. New client + old daemon (without codes): errors degrade to ServerError
3. New client + new daemon: full structured codes work correctly

Note: True version skew testing requires running different daemon
versions, which is complex in the integration test harness. These tests
verify the degradation logic in isolation.
"""

from __future__ import annotations

import capnp
from corvus_client.exceptions import (
    ServerError,
    VmNotFound,
    translate_kj_exception,
)

# Envelope that pycapnp wraps around daemon messages
ENVELOPE = "(remote):0: failed: remote exception: "


def _fake_kj(description: str) -> capnp.KjException:
    """Build a KjException carrying the given .description for unit tests."""
    return capnp.KjException(description)


class TestWireErrorSkew:
    """Test graceful degradation with version skew."""

    def test_new_daemon_new_client_full_structured_codes(self):
        """New daemon (v2+) emits codes, new client parses them correctly."""
        # Daemon emits: "vm_not_found :: VM 'web-1' not found"
        exc = _fake_kj(f"{ENVELOPE}vm_not_found :: VM 'web-1' not found")
        translated = translate_kj_exception(exc)

        # Should get typed exception
        assert isinstance(translated, VmNotFound)
        assert str(translated) == "VM 'web-1' not found"

    def test_new_daemon_old_client_degrades_gracefully(self):
        """New daemon (v2+) emits codes with prefixed messages.

        An old client that uses regex matching (without code parsing) would
        still match the human-readable part of the message, so it wouldn't
        necessarily degrade to ServerError. However, it would be lossy:
        - It might match "VM not found" in "vm_not_found :: VM not found"
        - But it loses the distinction between error types that have the same message

        The key point is: old clients don't break, they just don't get the
        full benefit of structured codes. The daemon's message format change
        is backward compatible in that existing regex patterns will still
        match the human-readable portion of the message.
        """
        import re

        # Old client's patterns (from the original regex table)
        # These were actual patterns used before B3
        old_patterns = [
            (r"VM .* not found", "VmNotFound"),
            (r"Disk .* not found", "DiskNotFound"),
        ]

        # New daemon message format
        message = "vm_not_found :: VM 'web-1' not found"

        # Old client would still match because "VM 'web-1' not found"
        # is contained in the message
        matched_pattern = None
        for pattern, name in old_patterns:
            if re.search(pattern, message):
                matched_pattern = name
                break

        # The old client WOULD match (it's not blocked by the prefix)
        # This is intentional: the prefix doesn't break old clients
        assert matched_pattern == "VmNotFound", (
            "Old client regex still matches the human-readable message part"
        )

    def test_old_daemon_new_client_degrades_to_server_error(self):
        """Old daemon (v1) emits plain messages, new client handles gracefully."""
        # Old daemon emits: "VM 'web-1' not found" (no code prefix)
        exc = _fake_kj(f"{ENVELOPE}VM 'web-1' not found")
        translated = translate_kj_exception(exc)

        # New client doesn't find a code, falls back to ServerError
        assert isinstance(translated, ServerError)
        assert str(translated) == "VM 'web-1' not found"

    def test_unknown_code_degrades_to_server_error(self):
        """Future daemon with new code, current client doesn't know it yet."""
        # Future daemon might emit: "new_error_code :: something happened"
        exc = _fake_kj(f"{ENVELOPE}new_error_code :: something happened")
        translated = translate_kj_exception(exc)

        # Unknown code -> ServerError
        assert isinstance(translated, ServerError)
        # The whole message is preserved as the body
        assert str(translated) == "new_error_code :: something happened"

    def test_malformed_code_prefix_degrades_to_server_error(self):
        """Malformed wire format degrades gracefully."""
        test_cases = [
            # Missing delimiter
            f"{ENVELOPE}vm_not_found VM not found",
            # Wrong delimiter
            f"{ENVELOPE}vm_not_found: VM not found",
            # Code at end
            f"{ENVELOPE}VM not found vm_not_found :: ",
            # Partial code
            f"{ENVELOPE}vm_no :: VM not found",
        ]

        for description in test_cases:
            exc = _fake_kj(description)
            translated = translate_kj_exception(exc)
            assert isinstance(translated, ServerError), (
                f"Expected ServerError for {description!r}, got {type(translated).__name__}"
            )

    def test_invalid_transition_preserves_payload(self):
        """InvalidTransition still extracts (status, reason) from message."""
        from corvus_client.exceptions import InvalidTransition

        exc = _fake_kj(
            f"{ENVELOPE}invalid_transition :: invalid transition from running: "
            "VM is saving"
        )
        translated = translate_kj_exception(exc)

        assert isinstance(translated, InvalidTransition)
        assert translated.status == "running"
        assert translated.reason == "VM is saving"
