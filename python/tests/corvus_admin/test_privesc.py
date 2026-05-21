"""sudo / doas auto-detection."""

from __future__ import annotations

from corvus_admin import privesc


def test_detect_returns_sudo_when_only_sudo_present(monkeypatch):
    privesc.reset_cache()

    def fake_which(name: str) -> str | None:
        return "/usr/bin/sudo" if name == "sudo" else None

    monkeypatch.setattr(privesc.shutil, "which", fake_which)
    pe = privesc.detect()
    assert pe is not None
    assert pe.tool == "sudo"
    assert pe.argv_prefix == ("sudo", "-n")


def test_detect_returns_doas_when_only_doas_present(monkeypatch):
    privesc.reset_cache()

    def fake_which(name: str) -> str | None:
        return "/usr/bin/doas" if name == "doas" else None

    monkeypatch.setattr(privesc.shutil, "which", fake_which)
    pe = privesc.detect()
    assert pe is not None
    assert pe.tool == "doas"
    assert pe.argv_prefix == ("doas", "-n")


def test_detect_prefers_sudo_when_both_installed(monkeypatch):
    privesc.reset_cache()

    def fake_which(name: str) -> str | None:
        return f"/usr/bin/{name}"

    monkeypatch.setattr(privesc.shutil, "which", fake_which)
    pe = privesc.detect()
    assert pe is not None
    assert pe.tool == "sudo"


def test_detect_returns_none_when_neither_present(monkeypatch):
    privesc.reset_cache()
    monkeypatch.setattr(privesc.shutil, "which", lambda _name: None)
    assert privesc.detect() is None
