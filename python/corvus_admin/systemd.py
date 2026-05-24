"""Render and install systemd unit files.

Templates live in the source tree at ``systemd/*.service.j2`` and
are bundled into the corvus-admin wheel so pipx installs keep them
on hand. Rendering parameterizes the binary path (resolved from
$PATH), the install mode (user or system), the log level, and any
mode-specific knobs (RuntimeDirectory, WantedBy).
"""

from __future__ import annotations

import importlib.resources as _ir
from pathlib import Path
from typing import Literal

import jinja2

from corvus_admin.runner import Runner

# Default install locations for the Haskell binaries on the target
# host. The user-mode default mirrors `make install`'s
# `stack install --local-bin-path ~/.local/bin`. The system-mode
# default matches the typical sysadmin layout for hand-built
# binaries; distro packages usually land in /usr/bin and operators
# pass `--binary-path /usr/bin/<bin>` to override.
_DEFAULT_BIN_DIR_USER = "~/.local/bin"
_DEFAULT_BIN_DIR_SYSTEM = "/usr/local/bin"
_BINARY_NAMES: dict[str, str] = {
    "daemon": "corvus",
    "nodeagent": "corvus-nodeagent",
    "netd": "corvus-netd",
}


def default_binary_path(component: str, *, mode: InstallMode) -> str:
    """Return the default absolute path of *component*'s binary on
    the target host. Component is one of ``daemon`` /
    ``nodeagent`` / ``netd``; mode picks the bin dir
    (``~/.local/bin`` for user, ``/usr/local/bin`` for system)."""

    if component not in _BINARY_NAMES:
        raise ValueError(f"unknown systemd component {component!r}")
    bin_dir = _DEFAULT_BIN_DIR_USER if mode == "user" else _DEFAULT_BIN_DIR_SYSTEM
    return f"{bin_dir}/{_BINARY_NAMES[component]}"


# Install locations for the rendered unit file on the target host.
# Exposed as module-level constants so tests can redirect them to
# a tmpdir without monkey-patching the install_unit helper itself.
SYSTEMD_USER_DIR = "~/.config/systemd/user"
SYSTEMD_SYSTEM_DIR = "/etc/systemd/system"

InstallMode = Literal["user", "system"]


# Map component -> (template filename, unit filename, must-be-system).
_COMPONENTS: dict[str, tuple[str, str, bool]] = {
    "daemon": ("corvus.service.j2", "corvus.service", False),
    "nodeagent": ("corvus-nodeagent.service.j2", "corvus-nodeagent.service", False),
    "netd": ("corvus-netd.service.j2", "corvus-netd.service", True),
}


def unit_filename(component: str) -> str:
    return _COMPONENTS[component][1]


def _template_text(template_name: str) -> str:
    """Load a template's text. The canonical location is package
    data inside ``corvus_admin/_systemd_templates/``; that's what
    pipx-installed copies (and any non-editable install) see. The
    same path is exposed at the repo root as a symlink called
    ``systemd/`` for operator-visibility."""

    pkg = (
        _ir.files("corvus_admin").joinpath("_systemd_templates").joinpath(template_name)
    )
    if pkg.is_file():
        return pkg.read_text(encoding="utf-8")
    # Editable / dev-checkout fallback: locate the file relative
    # to this module rather than via importlib.resources, which
    # can miss package-data on some setuptools layouts.
    here = Path(__file__).resolve()
    candidate = here.parent / "_systemd_templates" / template_name
    if candidate.is_file():
        return candidate.read_text(encoding="utf-8")
    raise FileNotFoundError(
        f"systemd template {template_name!r} not found in corvus_admin/_systemd_templates/"
    )


def render_unit(
    component: str,
    *,
    mode: InstallMode,
    binary_path: str,
    log_level: str = "info",
    database_url: str = "postgresql://localhost/corvus",
) -> str:
    """Render a systemd unit for *component* (one of ``daemon``,
    ``nodeagent``, ``netd``). The *binary_path* must be an
    absolute path; the caller resolves it via
    :func:`corvus_admin.binaries.find_all`.

    Raises :class:`ValueError` when netd is requested in user mode
    (it can't be — netd needs CAP_NET_ADMIN and root)."""

    if component not in _COMPONENTS:
        raise ValueError(f"unknown systemd component {component!r}")
    template_name, _unit, system_only = _COMPONENTS[component]
    if system_only and mode != "system":
        raise ValueError(
            f"component {component!r} can only be installed as a system service"
        )

    template = jinja2.Template(
        _template_text(template_name),
        keep_trailing_newline=True,
        autoescape=False,
    )
    return template.render(
        install_mode=mode,
        binary_path=binary_path,
        log_level=log_level,
        database_url=database_url,
    )


def install_unit(
    runner: Runner,
    *,
    component: str,
    mode: InstallMode,
    content: str,
) -> str:
    """Write the rendered unit to the right location for *mode*.
    Returns the absolute install path so callers can include it
    in progress output. In user mode the path is
    ``~/.config/systemd/user/<unit>``; in system mode
    ``/etc/systemd/system/<unit>``.

    The ``~/`` prefix is preserved in the path string so the
    runner can expand it against the right home — LocalRunner uses
    ``os.path.expanduser`` for the admin's own home; SshRunner
    queries the remote ``$HOME`` once and substitutes."""

    unit = unit_filename(component)
    user_mode = mode == "user"
    target_dir = SYSTEMD_USER_DIR if user_mode else SYSTEMD_SYSTEM_DIR
    runner.mkdir_p(target_dir, mode=0o755, sudo=not user_mode)
    target = f"{target_dir}/{unit}"
    runner.copy_bytes(
        content.encode("utf-8"),
        target,
        mode=0o644,
        sudo=not user_mode,
    )
    return target


def daemon_reload(runner: Runner, *, mode: InstallMode) -> None:
    if mode == "user":
        runner.run(["systemctl", "--user", "daemon-reload"], sudo=False)
    else:
        runner.run(["systemctl", "daemon-reload"], sudo=True)


def enable_now(runner: Runner, *, unit: str, mode: InstallMode) -> None:
    if mode == "user":
        runner.run(["systemctl", "--user", "enable", "--now", unit], sudo=False)
    else:
        runner.run(["systemctl", "enable", "--now", unit], sudo=True)


def restart(runner: Runner, *, unit: str, mode: InstallMode) -> None:
    if mode == "user":
        runner.run(["systemctl", "--user", "restart", unit], sudo=False)
    else:
        runner.run(["systemctl", "restart", unit], sudo=True)
