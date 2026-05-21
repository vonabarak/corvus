"""Binary discovery from $PATH."""

from __future__ import annotations

import pytest
from corvus_admin import binaries


def _fake_which_factory(layout: dict[str, str]):
    def fake_which(name: str) -> str | None:
        return layout.get(name)

    return fake_which


def test_find_all_resolves_user_local_layout(monkeypatch):
    layout = {
        "corvus": "/home/alice/.local/bin/corvus",
        "corvus-nodeagent": "/home/alice/.local/bin/corvus-nodeagent",
        "corvus-netd": "/home/alice/.local/bin/corvus-netd",
    }
    monkeypatch.setattr(binaries.shutil, "which", _fake_which_factory(layout))
    paths = binaries.find_all(require_netd=True)
    assert str(paths.corvus) == "/home/alice/.local/bin/corvus"
    assert str(paths.nodeagent) == "/home/alice/.local/bin/corvus-nodeagent"
    assert str(paths.netd) == "/home/alice/.local/bin/corvus-netd"


def test_find_all_resolves_distro_layout(monkeypatch):
    layout = {
        "corvus": "/usr/bin/corvus",
        "corvus-nodeagent": "/usr/bin/corvus-nodeagent",
        "corvus-netd": "/usr/bin/corvus-netd",
    }
    monkeypatch.setattr(binaries.shutil, "which", _fake_which_factory(layout))
    paths = binaries.find_all(require_netd=True)
    assert str(paths.corvus) == "/usr/bin/corvus"
    assert str(paths.nodeagent) == "/usr/bin/corvus-nodeagent"
    assert str(paths.netd) == "/usr/bin/corvus-netd"


def test_find_all_skips_netd_when_not_required(monkeypatch):
    layout = {
        "corvus": "/usr/local/bin/corvus",
        "corvus-nodeagent": "/usr/local/bin/corvus-nodeagent",
        # No corvus-netd.
    }
    monkeypatch.setattr(binaries.shutil, "which", _fake_which_factory(layout))
    paths = binaries.find_all(require_netd=False)
    assert paths.netd is None


def test_find_all_raises_when_required_binary_missing(monkeypatch):
    monkeypatch.setattr(binaries.shutil, "which", lambda _name: None)
    with pytest.raises(binaries.BinaryNotFound) as exc_info:
        binaries.find_all(require_netd=False)
    msg = str(exc_info.value)
    assert "corvus" in msg
    # Per design: error must not direct users to `make install`,
    # since the repo may not be on disk (distro packages).
    assert "make install" not in msg
