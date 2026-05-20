"""CLI smoke tests via click's testing harness.

These keep the round-trip honest: argument parsing, subcommand
wiring, and exit codes. The cert generation work is covered in
test_ca / test_deploy."""

from __future__ import annotations

from click.testing import CliRunner

from corvus_admin import cli


def test_init_creates_ca_and_emits_admin_cert(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    r = runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    assert r.exit_code == 0, r.output
    assert (admin_dir / "ca.crt").is_file()
    assert (admin_dir / "ca.key").is_file()
    # Admin client cert lands in XDG dir (pinned by the fixture).
    assert (xdg_home / "corvus" / "corvus-client.crt").is_file()
    assert (xdg_home / "corvus" / "corvus-client.key").is_file()
    # And the index now knows about it.
    assert "corvus-client:alice" in r.output


def test_init_is_idempotent_without_force(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    r1 = runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    assert r1.exit_code == 0
    r2 = runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    assert r2.exit_code == 0, r2.output
    # Second invocation hits the "already initialised" notice on
    # stderr, but exits 0 (idempotent).
    assert "already initialised" in r2.output


def test_list_reports_issued_certs(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    r = runner.invoke(cli.main, ["list", "--ca-dir", str(admin_dir)])
    assert r.exit_code == 0, r.output
    assert "corvus-client:alice" in r.output


def test_list_without_init_fails_with_clear_error(tmp_path):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    r = runner.invoke(cli.main, ["list", "--ca-dir", str(admin_dir)])
    assert r.exit_code != 0
    assert "No CA" in r.output


def test_deploy_client_emits_record(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    r = runner.invoke(
        cli.main,
        [
            "deploy",
            "client",
            "--ca-dir",
            str(admin_dir),
            "bob",
        ],
    )
    assert r.exit_code == 0, r.output
    assert "corvus-client:bob" in r.output
