"""Shared pytest fixtures for the corvus-admin suite."""

from __future__ import annotations

import pytest
from corvus_admin import privesc, store


@pytest.fixture(autouse=True)
def reset_privesc_cache():
    """privesc.detect() caches its result for the lifetime of the
    process. Tests that monkey-patch $PATH or shutil.which need a
    clean slate. Clearing before AND after each test is cheap and
    avoids order-dependent failures."""

    privesc.reset_cache()
    yield
    privesc.reset_cache()


@pytest.fixture()
def admin_store(tmp_path):
    """A fresh AdminStore rooted at a per-test tmpdir. The CA has
    NOT been initialised — call `ca.init_ca(...)` in the test
    body when you want one."""

    return store.AdminStore(tmp_path / "admin")


@pytest.fixture()
def xdg_home(tmp_path, monkeypatch):
    """Pin $XDG_CONFIG_HOME to a per-test dir so client-cert
    deploy doesn't touch the developer's real ~/.config."""

    target = tmp_path / "xdg-config"
    target.mkdir()
    monkeypatch.setenv("XDG_CONFIG_HOME", str(target))
    return target
