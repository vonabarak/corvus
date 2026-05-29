"""Deploy-recipe tests.

We use the LocalRunner but redirect every system path to a
per-test tmpdir, and replace ``systemctl`` with a no-op stub so
the tests don't need real systemd. The end-to-end behaviour with
a real runner is covered by the smoke step on a corvus-test-node
VM (Phase 2 verification).
"""

from __future__ import annotations

import pytest
from corvus_admin import ca, deploy
from corvus_admin.runner import LocalRunner
from cryptography import x509


@pytest.fixture()
def initialised_store(admin_store):
    ca.init_ca(admin_store)
    return admin_store


def test_deploy_daemon_drops_three_files_with_right_modes(
    initialised_store, fake_paths
):
    etc, _ = fake_paths
    runner = LocalRunner()
    plan = deploy.deploy_daemon(
        initialised_store,
        runner,
        listen_ip="127.0.0.1",
    )

    ca_pem = (etc / "ca.crt").read_bytes()
    daemon_pem = (etc / "corvus-daemon.crt").read_bytes()
    daemon_key = (etc / "corvus-daemon.key").read_bytes()

    # CA pem in the deployed file matches the admin store's CA.
    assert ca_pem == initialised_store.ca_cert_path.read_bytes()
    # Daemon cert CN starts with corvus-daemon: and ends with the
    # UUID the plan picked.
    cert = x509.load_pem_x509_certificate(daemon_pem)
    cn = cert.subject.get_attributes_for_oid(x509.NameOID.COMMON_NAME)[0].value
    assert cn == f"corvus-daemon:{plan.name}"
    # Key file is mode 0600.
    assert (etc / "corvus-daemon.key").stat().st_mode & 0o777 == 0o600
    # Cert files are mode 0644.
    assert (etc / "corvus-daemon.crt").stat().st_mode & 0o777 == 0o644
    assert (etc / "ca.crt").stat().st_mode & 0o777 == 0o644
    # Sanity: the key parses as a private key.
    from cryptography.hazmat.primitives import serialization

    _ = serialization.load_pem_private_key(daemon_key, password=None)


def test_deploy_daemon_invokes_systemctl_restart(initialised_store, fake_paths):
    _, log = fake_paths
    runner = LocalRunner()
    deploy.deploy_daemon(initialised_store, runner, listen_ip="127.0.0.1")
    lines = log.read_text().splitlines()
    # Expect at least an `enable --now corvus.service` then `restart corvus.service`.
    assert any(line == "enable --now corvus.service" for line in lines), lines
    assert any(line == "restart corvus.service" for line in lines), lines


def test_deploy_node_records_deployment_target(initialised_store, fake_paths):
    runner = LocalRunner()
    plan = deploy.deploy_node(
        initialised_store,
        runner,
        name="alpha",
        ip="10.0.0.21",
    )
    rec = next(
        r for r in initialised_store.iter_records() if r.cn == "corvus-node:alpha"
    )
    assert rec.deployed_to == "local"
    assert plan.service_unit == "corvus-nodeagent.service"


def test_deploy_client_lands_under_xdg_config_home(
    initialised_store, fake_paths, xdg_home
):
    deploy.deploy_client(initialised_store, name="alice")
    client_dir = xdg_home / "corvus"
    assert (client_dir / "ca.crt").is_file()
    assert (client_dir / "corvus-client.crt").is_file()
    assert (client_dir / "corvus-client.key").is_file()
    mode = (client_dir / "corvus-client.key").stat().st_mode & 0o777
    assert mode == 0o600


def test_deploy_client_does_not_invoke_systemctl(
    initialised_store, fake_paths, xdg_home
):
    _, log = fake_paths
    deploy.deploy_client(initialised_store, name="alice")
    # Client deploy is purely local; systemctl should never run.
    assert not log.exists() or log.read_text() == ""


class _RecordingRunner:
    """Captures every mkdir_p / copy_bytes / run call so tests can
    assert sudo was (or wasn't) requested. Behaviour-light: no FS
    side effects, no subprocess shells — just bookkeeping."""

    label = "rec"
    privesc = None

    def __init__(self) -> None:
        self.mkdirs: list[tuple[str, int, bool]] = []
        self.copies: list[tuple[str, int, bool]] = []
        self.runs: list[tuple[list[str], bool]] = []

    def mkdir_p(self, path: str, *, mode: int = 0o755, sudo: bool = False) -> None:
        self.mkdirs.append((path, mode, sudo))

    def which(self, name: str) -> str | None:
        # Deterministic stub: pretend the binary lives at
        # /opt/corvus/bin/<name>. Real LocalRunner.which would call
        # shutil.which, which would depend on the developer's
        # $PATH.
        return f"/opt/corvus/bin/{name}"

    def copy_bytes(
        self,
        data: bytes,
        remote_path: str,
        *,
        mode: int,
        sudo: bool = True,
    ) -> None:
        del data
        self.copies.append((remote_path, mode, sudo))

    def run(
        self,
        argv: list[str],
        *,
        check: bool = True,
        sudo: bool = False,
        capture: bool = False,
    ) -> object:
        del check, capture
        self.runs.append((list(argv), sudo))

        class _R:
            returncode = 0
            stdout = ""
            stderr = ""

        return _R()


def test_deploy_node_user_service_does_not_escalate(initialised_store):
    """Regression: `corvus-admin deploy node ... --user-service` must
    not invoke sudo for any step. The bug surfaced as `sudo: a
    password is required` on remote SSH targets where the operator
    expected an unprivileged drop into ~/.config/corvus/ and a
    `systemctl --user` restart."""

    runner = _RecordingRunner()
    deploy.deploy_node(
        initialised_store,
        runner,
        name="tobacco",
        ip="192.168.1.16",
        user_service=True,
    )
    # mkdir_p covers both ~/.config/corvus and ~/.config/systemd/user.
    assert runner.mkdirs, "no mkdir_p recorded"
    for path, _mode, sudo in runner.mkdirs:
        assert path.startswith("~/"), path
        assert sudo is False, f"mkdir_p on {path!r} requested sudo=True"
    # Four file drops: ca.crt, role.crt, role.key, and the unit file.
    assert len(runner.copies) == 4, runner.copies
    for path, _mode, sudo in runner.copies:
        assert sudo is False, f"copy_bytes to {path!r} requested sudo=True"
    # Every systemctl invocation must be `systemctl --user …` and
    # unprivileged.
    systemctl_calls = [
        (argv, sudo) for argv, sudo in runner.runs if argv[:1] == ["systemctl"]
    ]
    assert systemctl_calls, "no systemctl calls recorded"
    for argv, sudo in systemctl_calls:
        assert argv[1] == "--user", argv
        assert sudo is False, argv


def test_deploy_node_system_service_escalates(initialised_store):
    """Counter-check: without --user-service the system-mode deploy
    escalates for mkdir, copy, and systemctl."""

    runner = _RecordingRunner()
    deploy.deploy_node(
        initialised_store,
        runner,
        name="tobacco",
        ip="192.168.1.16",
        user_service=False,
    )
    for _path, _mode, sudo in runner.mkdirs:
        assert sudo is True
    for _path, _mode, sudo in runner.copies:
        assert sudo is True
    # systemctl (no --user) for the system path must escalate.
    assert any(
        argv[:1] == ["systemctl"] and "--user" not in argv and sudo is True
        for argv, sudo in runner.runs
    ), runner.runs


def test_deploy_node_installs_unit_file(initialised_store, fake_paths, tmp_path):
    """The deploy step writes a rendered systemd unit to the
    install directory, baking an absolute ExecStart path
    discovered via `command -v` on the runner. fake_paths puts
    fake corvus binaries on $PATH so shutil.which finds them
    deterministically."""

    runner = LocalRunner()
    deploy.deploy_node(
        initialised_store,
        runner,
        name="alpha",
        ip="10.0.0.21",
        user_service=False,
    )
    unit_path = tmp_path / "etc-systemd" / "corvus-nodeagent.service"
    assert unit_path.is_file(), f"unit not written to {unit_path}"
    content = unit_path.read_text()
    # ExecStart must be absolute (systemd does not expand ~) and
    # must point at the fake binary set up by fake_paths.
    expected_bin = str(tmp_path / "bin" / "corvus-nodeagent")
    assert f"ExecStart={expected_bin}" in content, content
    assert "WantedBy=multi-user.target" in content, content


def test_deploy_node_unit_file_picks_up_binary_path_override(
    initialised_store, fake_paths, tmp_path
):
    """--binary-path overrides the default binary location in the
    rendered unit."""

    runner = LocalRunner()
    deploy.deploy_node(
        initialised_store,
        runner,
        name="alpha",
        ip="10.0.0.21",
        user_service=False,
        binary_path="/opt/corvus/bin/corvus-nodeagent",
    )
    unit_path = tmp_path / "etc-systemd" / "corvus-nodeagent.service"
    content = unit_path.read_text()
    assert "ExecStart=/opt/corvus/bin/corvus-nodeagent" in content, content


def test_deploy_daemon_unit_file_carries_database_url(
    initialised_store, fake_paths, tmp_path
):
    runner = LocalRunner()
    deploy.deploy_daemon(
        initialised_store,
        runner,
        listen_ip="127.0.0.1",
        database_url="postgresql://db.internal/corvus",
    )
    unit_path = tmp_path / "etc-systemd" / "corvus.service"
    content = unit_path.read_text()
    assert "--database postgresql://db.internal/corvus" in content, content


# ---------------------------------------------------------------------------
# --dry-run


def test_deploy_daemon_dry_run_does_not_touch_disk(
    initialised_store, fake_paths, tmp_path
):
    """`--dry-run` short-circuits after computing the DeployPlan:
    no cert is minted, no files are written, no systemctl invoked,
    and the admin store's index gains nothing."""

    etc, log = fake_paths
    runner = LocalRunner()
    plan = deploy.deploy_daemon(
        initialised_store,
        runner,
        listen_ip="127.0.0.1",
        dry_run=True,
    )
    # Plan still describes what would happen.
    assert plan.role == "corvus-daemon"
    assert plan.service_unit == "corvus.service"
    # No cert files dropped.
    assert not (etc / "ca.crt").exists()
    assert not (etc / "corvus-daemon.crt").exists()
    assert not (etc / "corvus-daemon.key").exists()
    # No systemctl invocation.
    assert not log.exists() or log.read_text() == ""
    # No new record (admin store still only has whatever was there
    # before — none, in this fixture).
    assert list(initialised_store.iter_records()) == []


def test_deploy_node_dry_run_does_not_record(initialised_store, fake_paths):
    deploy.deploy_node(
        initialised_store, LocalRunner(), name="alpha", ip="10.0.0.1", dry_run=True
    )
    assert list(initialised_store.iter_records()) == []


def test_renew_node_dry_run_does_not_mint(initialised_store, fake_paths):
    """Set up a node record, then renew it under --dry-run and check
    no fresh cert overwrites the original in the issued/ dir."""

    # Real deploy first to give us a record to renew.
    deploy.deploy_node(initialised_store, LocalRunner(), name="alpha", ip="10.0.0.1")
    original_serial = next(
        r for r in initialised_store.iter_records() if r.cn == "corvus-node:alpha"
    ).serial
    # Renew under --dry-run with --force (the cert is freshly minted
    # so it's not yet "due" — force is needed to bypass the window
    # check).
    deploy.renew_node(
        initialised_store, name="alpha", target="local", force=True, dry_run=True
    )
    # Serial unchanged because no fresh cert was issued.
    new_serial = next(
        r for r in initialised_store.iter_records() if r.cn == "corvus-node:alpha"
    ).serial
    assert new_serial == original_serial
