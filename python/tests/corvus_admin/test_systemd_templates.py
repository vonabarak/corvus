"""Systemd unit template rendering."""

from __future__ import annotations

import pytest
from corvus_admin import systemd as systemd_mod


def test_daemon_user_mode_has_no_runtime_directory():
    rendered = systemd_mod.render_unit(
        "daemon",
        mode="user",
        binary_path="/home/alice/.local/bin/corvus",
    )
    assert "RuntimeDirectory=corvus" not in rendered
    assert "XDG_RUNTIME_DIR" not in rendered
    assert "ExecStart=/home/alice/.local/bin/corvus" in rendered
    # User services land under default.target so they start with
    # the user-systemd graph; system services use multi-user.
    assert "WantedBy=default.target" in rendered


def test_daemon_system_mode_sets_runtime_directory():
    rendered = systemd_mod.render_unit(
        "daemon",
        mode="system",
        binary_path="/usr/bin/corvus",
    )
    assert "RuntimeDirectory=corvus" in rendered
    assert "Environment=XDG_RUNTIME_DIR=/run/corvus" in rendered
    assert "ExecStart=/usr/bin/corvus" in rendered
    assert "WantedBy=multi-user.target" in rendered


def test_nodeagent_user_mode_has_no_runtime_directory():
    rendered = systemd_mod.render_unit(
        "nodeagent",
        mode="user",
        binary_path="/home/alice/.local/bin/corvus-nodeagent",
    )
    assert "RuntimeDirectory=corvus" not in rendered
    assert "ExecStart=/home/alice/.local/bin/corvus-nodeagent" in rendered


def test_nodeagent_system_mode_sets_runtime_directory():
    rendered = systemd_mod.render_unit(
        "nodeagent",
        mode="system",
        binary_path="/usr/bin/corvus-nodeagent",
    )
    assert "RuntimeDirectory=corvus" in rendered
    assert "Environment=XDG_RUNTIME_DIR=/run/corvus" in rendered


def test_netd_only_renders_in_system_mode():
    rendered = systemd_mod.render_unit(
        "netd",
        mode="system",
        binary_path="/usr/bin/corvus-netd",
    )
    assert "ExecStart=/usr/bin/corvus-netd" in rendered
    assert "WantedBy=multi-user.target" in rendered


def test_netd_user_mode_is_rejected():
    with pytest.raises(ValueError):
        systemd_mod.render_unit(
            "netd",
            mode="user",
            binary_path="/usr/bin/corvus-netd",
        )


def test_unknown_component_raises():
    with pytest.raises(ValueError):
        systemd_mod.render_unit(
            "ghost",
            mode="user",
            binary_path="/x/y/z",
        )


def test_log_level_is_threaded_through():
    rendered = systemd_mod.render_unit(
        "daemon",
        mode="user",
        binary_path="/x/corvus",
        log_level="debug",
    )
    assert "--log-level debug" in rendered


def test_database_url_is_threaded_through_daemon_template():
    rendered = systemd_mod.render_unit(
        "daemon",
        mode="user",
        binary_path="/x/corvus",
        database_url="postgresql://db.internal/corvus",
    )
    assert "--database postgresql://db.internal/corvus" in rendered
