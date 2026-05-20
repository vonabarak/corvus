"""LocalRunner behaviour. SshRunner is shelled out to ssh/scp,
which the integration smoke test exercises; the unit tests stick
to in-process logic that doesn't touch sshd."""

from __future__ import annotations

import os
import stat
import subprocess

import pytest

from corvus_admin.runner import LocalRunner, RunnerError, for_target


def test_for_target_local_returns_local_runner():
    runner = for_target("local")
    assert isinstance(runner, LocalRunner)
    assert runner.label == "local"


def test_for_target_user_at_host_returns_ssh_runner():
    runner = for_target("root@10.0.0.21")
    assert runner.label == "ssh:root@10.0.0.21"


def test_copy_bytes_writes_with_requested_mode(tmp_path):
    runner = LocalRunner()
    dest = tmp_path / "out" / "ca.crt"
    runner.mkdir_p(str(dest.parent), mode=0o755)
    runner.copy_bytes(b"hello\n", str(dest), mode=0o644)
    assert dest.read_bytes() == b"hello\n"
    mode = dest.stat().st_mode & 0o777
    assert mode == 0o644, f"expected 0644, got {oct(mode)}"


def test_copy_bytes_overwrites_existing_file_atomically(tmp_path):
    runner = LocalRunner()
    dest = tmp_path / "x.bin"
    runner.copy_bytes(b"first", str(dest), mode=0o600)
    runner.copy_bytes(b"second", str(dest), mode=0o600)
    assert dest.read_bytes() == b"second"
    mode = dest.stat().st_mode & 0o777
    assert mode == 0o600


def test_run_returns_stdout_when_captured(tmp_path):
    runner = LocalRunner()
    r = runner.run(["printf", "%s", "hi"], capture=True)
    assert r.returncode == 0
    assert r.stdout == "hi"


def test_run_raises_runner_error_on_nonzero_exit(tmp_path):
    runner = LocalRunner()
    with pytest.raises(RunnerError):
        runner.run(["false"], capture=True)


def test_run_does_not_raise_when_check_false(tmp_path):
    runner = LocalRunner()
    r = runner.run(["false"], check=False, capture=True)
    assert r.returncode != 0


def test_mkdir_p_is_idempotent(tmp_path):
    runner = LocalRunner()
    target = tmp_path / "a" / "b" / "c"
    runner.mkdir_p(str(target), mode=0o755)
    runner.mkdir_p(str(target), mode=0o755)
    assert target.is_dir()
