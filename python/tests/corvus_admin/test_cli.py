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


def test_renew_client_force(tmp_path, xdg_home):
    """Renewing a freshly-minted client cert requires --force
    (it's not due for 364 days). With --force the CLI exits 0 and
    overwrites the XDG client cert pair."""

    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    # Default: refuses
    r = runner.invoke(
        cli.main,
        ["renew", "client", "--ca-dir", str(admin_dir), "alice"],
    )
    assert r.exit_code != 0
    assert "still valid" in r.output
    # --force: succeeds
    r2 = runner.invoke(
        cli.main,
        ["renew", "client", "--ca-dir", str(admin_dir), "alice", "--force"],
    )
    assert r2.exit_code == 0, r2.output
    assert "Renewed client cert corvus-client:alice" in r2.output


def test_status_lists_client_record(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main,
        ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"],
    )
    r = runner.invoke(cli.main, ["status", "--ca-dir", str(admin_dir)])
    # exit_code == 0: only a client cert was issued (no remote
    # services to probe), so the probe loop has nothing to flag
    # as unreachable.
    assert r.exit_code == 0, r.output
    assert "corvus-client:alice" in r.output
    # Client rows are labelled `client`, not `ok` / `UNREACHABLE`.
    assert "client" in r.output


# ---------------------------------------------------------------------------
# Completion


def test_completion_bash_emits_click_script():
    runner = CliRunner()
    r = runner.invoke(cli.main, ["completion", "bash"])
    assert r.exit_code == 0, r.output
    # Click's bash completion stamps the env var name into the
    # generated script. If the wiring changes we want the test to
    # catch it.
    assert "_CORVUS_ADMIN_COMPLETE=bash_complete" in r.output


def test_completion_zsh_and_fish_emit_scripts():
    runner = CliRunner()
    for shell in ("zsh", "fish"):
        r = runner.invoke(cli.main, ["completion", shell])
        assert r.exit_code == 0, r.output
        assert "_CORVUS_ADMIN_COMPLETE" in r.output


def test_completion_rejects_unknown_shell():
    runner = CliRunner()
    r = runner.invoke(cli.main, ["completion", "powershell"])
    assert r.exit_code != 0
    # Click's choice-validation error wording is stable across 8.x.
    assert "Invalid value" in r.output


# ---------------------------------------------------------------------------
# JSON output


def test_list_output_json_is_parseable(tmp_path, xdg_home):
    import json

    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main, ["list", "--ca-dir", str(admin_dir), "--output", "json"]
    )
    assert r.exit_code == 0, r.output
    parsed = json.loads(r.output)
    assert isinstance(parsed, list)
    assert len(parsed) == 1
    assert parsed[0]["cn"] == "corvus-client:alice"


def test_status_output_json_is_parseable(tmp_path, xdg_home):
    import json

    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main, ["status", "--ca-dir", str(admin_dir), "--output", "json"]
    )
    assert r.exit_code == 0, r.output
    parsed = json.loads(r.output)
    assert isinstance(parsed, list)
    assert parsed[0]["role"] == "corvus-client"


# ---------------------------------------------------------------------------
# Revoke


def test_revoke_removes_record(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main,
        ["revoke", "--ca-dir", str(admin_dir), "corvus-client:alice", "--yes"],
    )
    assert r.exit_code == 0, r.output
    # list should be empty now
    r2 = runner.invoke(cli.main, ["list", "--ca-dir", str(admin_dir)])
    assert "corvus-client:alice" not in r2.output
    assert "(no certs issued yet)" in r2.output


def test_revoke_unknown_cn_exits_nonzero(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main, ["revoke", "--ca-dir", str(admin_dir), "corvus-node:ghost", "--yes"]
    )
    assert r.exit_code != 0
    assert "No such cert" in r.output


# ---------------------------------------------------------------------------
# Renew sweep (--due)


def test_renew_due_finds_nothing_when_all_fresh(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main,
        ["renew", "--due", "--within", "30", "--ca-dir", str(admin_dir)],
    )
    assert r.exit_code == 0, r.output
    assert "No certs due" in r.output


def test_renew_due_with_wide_window_picks_up_freshly_minted_client(tmp_path, xdg_home):
    """Freshly-minted certs are 365 days out, so --within 400 brings
    them into scope. Combined with --dry-run keeps the filesystem
    untouched while exercising the dispatch path."""

    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main,
        [
            "renew",
            "--due",
            "--within",
            "400",
            "--ca-dir",
            str(admin_dir),
            "--dry-run",
        ],
    )
    assert r.exit_code == 0, r.output
    assert "would renew corvus-client:alice" in r.output
    assert "1/1 renewed" in r.output


def test_renew_due_rejects_subcommand_combo(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main,
        ["renew", "--due", "client", "alice", "--ca-dir", str(admin_dir)],
    )
    assert r.exit_code != 0
    assert "cannot be combined" in r.output


# ---------------------------------------------------------------------------
# Multi-host deploy (UsageError paths exercised on the CLI surface;
# the actual deploy uses LocalRunner so it can run in tmp_path with
# fake_paths). The success path lives in test_deploy.py.


def test_deploy_node_conflict_target_and_targets(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main,
        [
            "deploy",
            "node",
            "--ca-dir",
            str(admin_dir),
            "alpha",
            "local",
            "--targets",
            "local",
        ],
    )
    assert r.exit_code != 0
    assert "not both" in r.output


def test_deploy_node_multi_target_without_allow_shared_cn(tmp_path, xdg_home):
    runner = CliRunner()
    admin_dir = tmp_path / "admin"
    runner.invoke(
        cli.main, ["init", "--ca-dir", str(admin_dir), "--admin-name", "alice"]
    )
    r = runner.invoke(
        cli.main,
        [
            "deploy",
            "node",
            "--ca-dir",
            str(admin_dir),
            "alpha",
            "--targets",
            "local,local",
            "--dry-run",
        ],
    )
    assert r.exit_code != 0
    assert "refusing to deploy" in r.output
