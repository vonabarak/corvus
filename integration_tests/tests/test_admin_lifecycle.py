"""End-to-end coverage for ``corvus-admin``'s local-only commands.

[doc/security.md](../../doc/security.md) lists ``corvus-admin``'s
verbs (``init``, ``renew``, ``revoke``, ``list``, ``status``,
``deploy``, ``quickstart``, ``register``). Only ``quickstart`` is
covered today, by :mod:`test_quickstart`. The other verbs that
operate purely against the local admin store — CA bootstrap, cert
issuance, expiry sweeps, listing, and revocation — have no test
coverage; a refactor that breaks ``init`` idempotency, drops the
``--output json`` shape, or stops registering the admin client cert
would only surface on an operator's first onboarding.

This file boots no test node and dials no daemon — every assertion
is on the local CA store + the client-cert dir produced by the
``corvus-admin`` subprocess. Each test gets a fresh tempdir as both
``--ca-dir`` and ``$XDG_CONFIG_HOME``, so they don't see (or pollute)
the operator's real admin store.

What's covered:

* ``init`` on an empty ``--ca-dir``: CA appears, admin's client
  cert lands in ``$XDG_CONFIG_HOME/corvus/``, the index records
  exactly one ``corvus-client:<user>`` row.
* ``init`` re-run on the same dir is a no-op (exits 0 with a
  "already initialised" banner; no files change).
* ``renew --due --within N --dry-run`` — exits 0, leaves cert
  files untouched, mentions "would renew" for any due cert.
* ``list --output json`` shape: an array, each entry carrying
  ``cn``, ``expires_at``, ``role`` (and ``ip``/``deployed_to``
  where set). Catches accidental key renames.
* ``revoke <cn>`` removes the row from the admin store's index
  but leaves the cert/key files in place (the docstring at
  ``cli.py:1146-1153`` calls this out as "pure bookkeeping").

What's NOT covered here:

* ``deploy daemon`` / ``deploy node`` / ``deploy netd`` — they
  ssh into a remote target and lay down systemd units, which
  needs another VM the test fixture would have to provision.
  ``test_quickstart`` already exercises the happy path of the
  one-shot deploy.
* ``register`` — needs a running daemon; touches the daemon-side
  ``nodes.create`` RPC. Belongs in a node-focused test, not the
  admin-CLI surface here.
* ``renew --force <role>`` on a deployed cert — re-issues then
  redeploys to a remote target. Same fixture concern as the
  deploy verbs. The ``--dry-run`` sweep above exercises the
  store-side renewal logic.
* CN-prefix-validation negative test (importing a hand-crafted
  ``foo:bar`` cert and asserting rejection). ``corvus-admin init``
  doesn't accept external certs as input — the CN prefix is
  enforced at issuance time by ``ca.issue_cert``. A test would
  need to fabricate a cert with the cryptography library and
  drop it into the store, which is reaching past the CLI surface.
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path


def _admin(
    *args: str,
    ca_dir: Path,
    xdg_home: Path,
    check: bool = True,
) -> subprocess.CompletedProcess:
    """Invoke ``corvus-admin`` as a subprocess of the venv Python.

    Using ``sys.executable -m corvus_admin`` rather than the
    ``corvus-admin`` console script ensures we pick up the in-tree
    package, not an unrelated install on ``$PATH`` (the same trick
    :class:`WebGateway` uses for ``corvus-web``)."""
    env = os.environ.copy()
    env["XDG_CONFIG_HOME"] = str(xdg_home)
    # ``--ca-dir`` is a per-subcommand option (see _ca_dir_option in
    # python/corvus_admin/cli.py), so it goes AFTER the subcommand.
    # We splice it in after the first positional that's a verb (the
    # caller passes args as `("init",)`, `("list", "--output", "json")`,
    # etc., so position 1 is the verb and ca-dir slots after it).
    verb_args = list(args)
    cmd = [
        sys.executable,
        "-m",
        "corvus_admin",
        verb_args[0],
        "--ca-dir",
        str(ca_dir),
        *verb_args[1:],
    ]
    return subprocess.run(
        cmd,
        env=env,
        check=check,
        capture_output=True,
        text=True,
        timeout=30.0,
    )


class TestCaInitAndIdempotence:
    """``corvus-admin init`` + re-run idempotence."""

    def test_init_creates_ca_and_client_cert(self, tmp_path: Path):
        ca_dir = tmp_path / "admin"
        xdg = tmp_path / "xdg"
        xdg.mkdir()

        result = _admin("init", ca_dir=ca_dir, xdg_home=xdg)
        assert result.returncode == 0, result
        assert "CA initialised" in result.stdout, result
        assert "Issued client cert corvus-client:" in result.stdout, result

        # CA files appear in --ca-dir.
        assert (ca_dir / "ca.crt").is_file()
        assert (ca_dir / "ca.key").is_file()

        # Admin's client cert lands under XDG_CONFIG_HOME/corvus/
        client_dir = xdg / "corvus"
        assert (client_dir / "corvus-client.crt").is_file()
        assert (client_dir / "corvus-client.key").is_file()
        assert (client_dir / "ca.crt").is_file()

        # Index records exactly one row (the admin's own client cert).
        # Verify via `list --output json` for a structured check.
        listed = _admin("list", "--output", "json", ca_dir=ca_dir, xdg_home=xdg)
        rows = json.loads(listed.stdout)
        assert len(rows) == 1, f"expected one row, got {len(rows)}: {rows!r}"
        rec = rows[0]
        assert rec["cn"].startswith("corvus-client:"), rec

    def test_init_is_idempotent(self, tmp_path: Path):
        """Re-running ``init`` on an already-populated store
        prints a "already initialised" banner and exits 0 without
        touching files (rotation requires ``--force``)."""
        ca_dir = tmp_path / "admin"
        xdg = tmp_path / "xdg"
        xdg.mkdir()

        first = _admin("init", ca_dir=ca_dir, xdg_home=xdg)
        assert first.returncode == 0, first

        ca_crt_before = (ca_dir / "ca.crt").read_bytes()
        admin_cert_before = (xdg / "corvus" / "corvus-client.crt").read_bytes()

        # cli.py:245-250 emits the warning to stderr and exits 0 so
        # ``corvus-admin init`` can be safely re-run in idempotent
        # bootstrap scripts.
        second = _admin("init", ca_dir=ca_dir, xdg_home=xdg)
        assert second.returncode == 0, second
        assert "already initialised" in second.stderr, second

        # CA + admin client cert bytes are unchanged.
        assert (ca_dir / "ca.crt").read_bytes() == ca_crt_before
        assert (xdg / "corvus" / "corvus-client.crt").read_bytes() == admin_cert_before


class TestRenewDryRun:
    """``corvus-admin renew --due`` sweeps the store and reports."""

    def test_renew_due_dry_run_leaves_files_untouched(self, tmp_path: Path):
        """``--due --within 36500`` (≈100 years) matches every cert
        the freshly-issued admin client owns; ``--dry-run`` prints
        what it would do, leaves the cert untouched, exits 0."""
        ca_dir = tmp_path / "admin"
        xdg = tmp_path / "xdg"
        xdg.mkdir()

        _admin("init", ca_dir=ca_dir, xdg_home=xdg)
        admin_cert_before = (xdg / "corvus" / "corvus-client.crt").read_bytes()

        result = _admin(
            "renew",
            "--due",
            "--within",
            "36500",
            "--dry-run",
            ca_dir=ca_dir,
            xdg_home=xdg,
        )
        assert result.returncode == 0, result
        # The output should mention the cert that *would* be
        # renewed. The "would renew" verb comes from
        # cli.py:887-892 in the dry-run branch.
        assert "would renew" in result.stdout, result

        # The cert on disk is unchanged.
        assert (xdg / "corvus" / "corvus-client.crt").read_bytes() == admin_cert_before


class TestListAndRevoke:
    """``list --output json`` shape + ``revoke`` bookkeeping."""

    def test_list_json_shape(self, tmp_path: Path):
        """The JSON exposition is the operator's machine-readable
        view of the store. Pin the keys so a refactor that renames
        ``expires_at`` to ``expiry`` (or moves things into a nested
        sub-object) surfaces here."""
        ca_dir = tmp_path / "admin"
        xdg = tmp_path / "xdg"
        xdg.mkdir()
        _admin("init", ca_dir=ca_dir, xdg_home=xdg)

        result = _admin("list", "--output", "json", ca_dir=ca_dir, xdg_home=xdg)
        assert result.returncode == 0, result
        rows = json.loads(result.stdout)
        assert isinstance(rows, list)
        assert rows, "empty list after init — at least the admin client cert "
        rec = rows[0]
        # Required keys per the documented machine-readable shape.
        # We don't pin EVERY key (the structure carries optional
        # fields like ``deployed_to``/``ip`` that may be absent for
        # a freshly-issued local client), but the load-bearing
        # identifiers must be present.
        for required in ("cn", "expires_at", "role"):
            assert required in rec, (
                f"key {required!r} missing from list --output json row: {rec!r}"
            )
        # `role` is the full CN prefix (e.g. "corvus-client",
        # "corvus-daemon"), not the bare role name. See
        # ca.ROLE_CLIENT / ROLE_DAEMON / ROLE_NODE / ROLE_NETD.
        assert rec["role"] in (
            "corvus-client",
            "corvus-daemon",
            "corvus-node",
            "corvus-netd",
        ), f"unexpected role {rec['role']!r}"

    def test_revoke_drops_row_from_index(self, tmp_path: Path):
        """``revoke <cn>`` removes the row from the index but
        leaves the cert/key files in place. cli.py:1146-1153 calls
        this out: ``revoke`` is pure bookkeeping — Corvus does not
        consume a CRL."""
        ca_dir = tmp_path / "admin"
        xdg = tmp_path / "xdg"
        xdg.mkdir()
        _admin("init", ca_dir=ca_dir, xdg_home=xdg)

        listed = _admin("list", "--output", "json", ca_dir=ca_dir, xdg_home=xdg)
        rows = json.loads(listed.stdout)
        admin_cn = rows[0]["cn"]
        assert admin_cn.startswith("corvus-client:")

        # Cert file path before revoke.
        admin_crt = xdg / "corvus" / "corvus-client.crt"
        assert admin_crt.is_file()

        _admin("revoke", admin_cn, "--yes", ca_dir=ca_dir, xdg_home=xdg)

        # The row is gone from the index.
        listed_after = _admin("list", "--output", "json", ca_dir=ca_dir, xdg_home=xdg)
        rows_after = json.loads(listed_after.stdout)
        assert not any(r["cn"] == admin_cn for r in rows_after), (
            f"row for {admin_cn} still in index after revoke: {rows_after!r}"
        )

        # But the on-disk cert file survives (revoke is bookkeeping).
        assert admin_crt.is_file(), (
            "revoke deleted the cert file; cli.py:1146-1153 promises only "
            "the index row is dropped"
        )

    def test_revoke_unknown_cn_errors(self, tmp_path: Path):
        """Revoking a CN not in the index exits non-zero with a
        diagnostic on stderr."""
        ca_dir = tmp_path / "admin"
        xdg = tmp_path / "xdg"
        xdg.mkdir()
        _admin("init", ca_dir=ca_dir, xdg_home=xdg)

        result = _admin(
            "revoke",
            "corvus-client:does-not-exist",
            "--yes",
            ca_dir=ca_dir,
            xdg_home=xdg,
            check=False,
        )
        assert result.returncode != 0, result
        assert "No such cert" in result.stderr, result
