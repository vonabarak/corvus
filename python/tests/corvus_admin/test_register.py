"""Tests for the `register` subcommand. We don't run a real `crv`
binary; instead we stub it with a tiny shell script that records
its argv to a log file."""

from __future__ import annotations

import datetime as dt
import os
from pathlib import Path

import pytest

from corvus_admin import register as register_mod


@pytest.fixture()
def fake_crv(tmp_path, monkeypatch):
    """A stand-in for ``crv`` that:

    * records its argv into ``invocations.log``;
    * on ``crv node add …`` exits 0 with no output;
    * on ``crv node show <name>`` prints a fake response that
      always carries a non-empty healthcheck (so the poll
      finishes on the first iteration).
    """

    bin_dir = tmp_path / "bin"
    bin_dir.mkdir()
    log = bin_dir / "invocations.log"
    crv = bin_dir / "crv"
    crv.write_text(
        "#!/bin/sh\n"
        f'echo "$@" >> {log!s}\n'
        'case "$1$2" in\n'
        '  "nodeadd")\n'
        "    exit 0\n"
        "    ;;\n"
        '  "nodeshow")\n'
        '    echo "Name: $3"\n'
        '    echo "Healthcheck: 2026-05-20T19:00:00Z"\n'
        "    exit 0\n"
        "    ;;\n"
        "  *)\n"
        '    echo "unknown stub: $@" 1>&2\n'
        "    exit 2\n"
        "    ;;\n"
        "esac\n"
    )
    crv.chmod(0o755)
    monkeypatch.setenv("PATH", f"{bin_dir}:{os.environ['PATH']}")
    return crv, log


def test_register_node_calls_crv_add_with_documented_flags(fake_crv):
    crv, log = fake_crv
    result = register_mod.register_node(
        name="alpha",
        host="10.0.0.21",
        base_path="/srv/corvus/VMs",
        description="alpha test",
    )
    assert result.healthy is True
    invocations = log.read_text().splitlines()
    # The first crv invocation must be `node add` with all the
    # flags the CLI documents — silent omissions would be
    # surprising in a deploy-and-register flow.
    first = invocations[0]
    assert first.startswith("node add alpha"), first
    assert "--host 10.0.0.21" in first
    assert "--node-agent-port 9878" in first
    assert "--net-agent-port 9877" in first
    assert "--base-path /srv/corvus/VMs" in first
    assert "--description alpha test" in first


def test_register_node_surfaces_failure(tmp_path, monkeypatch):
    """If `crv node add` exits non-zero, register_node must raise
    RegisterError carrying the stderr."""

    # Use a dedicated subdir so the fake-crv fixture's bin/ doesn't
    # clash if pytest reuses tmp_path.
    bin_dir = tmp_path / "bad-bin"
    bin_dir.mkdir()
    bad_crv = bin_dir / "crv"
    bad_crv.write_text("#!/bin/sh\necho 'simulated failure' 1>&2\nexit 1\n")
    bad_crv.chmod(0o755)
    monkeypatch.setenv("PATH", f"{bin_dir}:{os.environ['PATH']}")
    with pytest.raises(register_mod.RegisterError) as exc:
        register_mod.register_node(name="alpha", host="10.0.0.21")
    assert "simulated failure" in str(exc.value) or "simulated" in (
        exc.value.stderr or ""
    )


def test_register_returns_unhealthy_when_show_lacks_healthcheck(tmp_path, monkeypatch):
    """If `crv node show` never reports a healthcheck within the
    timeout, the result should report healthy=False (rather than
    raising)."""

    bin_dir = tmp_path / "unhealthy-bin"
    bin_dir.mkdir()
    log = bin_dir / "invocations.log"
    crv = bin_dir / "crv"
    crv.write_text(
        "#!/bin/sh\n"
        f'echo "$@" >> {log!s}\n'
        'case "$1$2" in\n'
        '  "nodeadd") exit 0 ;;\n'
        '  "nodeshow")\n'
        '    echo "Name: $3"\n'
        '    echo "Healthcheck: -"\n'
        "    exit 0\n"
        "    ;;\n"
        "  *) exit 2 ;;\n"
        "esac\n"
    )
    crv.chmod(0o755)
    monkeypatch.setenv("PATH", f"{bin_dir}:{os.environ['PATH']}")
    result = register_mod.register_node(
        name="alpha",
        host="10.0.0.21",
        healthcheck_timeout_sec=0.5,
    )
    assert result.healthy is False
    # We still get the show output back for diagnostics.
    assert "Healthcheck: -" in result.show_stdout
