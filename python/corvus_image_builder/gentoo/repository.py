"""Configure Portage repositories and synchronize verified sources."""

import os
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

from ..core import BuildContext
from ._files import write
from .base import GentooStage


class RepositoryStage(GentooStage):
    """Configure builder Portage caches and signed binary and source repositories.

    Initialize trust with getuto, fetch a verified snapshot, and sync the tree.
    """

    requires = frozenset({"workspace"})
    produces = frozenset({"repository"})

    def execute(self, context: BuildContext) -> None:
        if not Path("/usr/share/openpgp-keys/gentoo-release.asc").is_file():
            raise ValueError("Builder lacks the Gentoo release verification key")
        config = Path("/etc/portage/make.conf")
        lines = [
            line
            for line in config.read_text().splitlines()
            if not line.lstrip().startswith(("FEATURES=", "DISTDIR=", "PKGDIR="))
        ]
        config.write_text(
            "\n".join(lines)
            + f'''\nDISTDIR="{self.settings.cache}/distfiles"
PKGDIR="{self.settings.cache}/packages/{self.settings.variant}"
FEATURES="-userfetch -userpriv -getbinpkg -binpkg-request-signature -binpkg-ignore-signature"
GPG_VERIFY_USER_DROP=""
GPG_VERIFY_GROUP_DROP=""
'''
        )
        write(
            Path("/etc/portage/binrepos.conf/zz-corvus-python.conf"),
            f"""[binhost]
priority = 9999
location = {self.settings.cache}/binhost/{self.settings.variant}
sync-uri = https://distfiles.gentoo.org/releases/amd64/binpackages/23.0/{self.settings.variant}
verify-signature = true
""",
        )
        write(
            Path("/etc/portage/repos.conf/zz-corvus-python.conf"),
            f"""[gentoo]
sync-webrsync-verify-signature = true
sync-rsync-verify-metamanifest = true
sync-openpgp-key-package = sec-keys/openpgp-keys-gentoo-release
sync-openpgp-key-path = /usr/share/openpgp-keys/gentoo-release.asc
sync-openpgp-key-refresh = keyserver
sync-openpgp-keyserver = {os.environ.get("PORTAGE_KEYSERVER", "hkps://keys.gentoo.org")}
""",
        )
        helper = Path("/etc/portage/corvus-python-getuto")
        write(helper, "#!/bin/sh\nexec env -u ROOT -u SYSROOT getuto\n")
        helper.chmod(0o755)
        with config.open("a") as stream:
            stream.write('PORTAGE_TRUST_HELPER="/etc/portage/corvus-python-getuto"\n')
        self.command_string(context, "getuto")
        self.sync_repository(context)
        context.artifacts["repository"] = Path("/var/db/repos/gentoo")

    def sync_repository(self, context: BuildContext) -> None:
        # Direct webrsync ignores repos.conf's key refresh policy. Scope the
        # same gemato adapter used by the shell builds to these subprocesses.
        gemato = shutil.which("gemato")
        if gemato is None:
            raise ValueError("Missing prerequisite: gemato")
        with tempfile.TemporaryDirectory(prefix="corvus-sync-") as directory:
            adapter = Path(directory) / "gemato"
            write(
                adapter,
                '#!/bin/sh\noperation=$1\nshift\nexec "$CORVUS_GEMATO" "$operation" --no-wkd --keyserver "$PORTAGE_KEYSERVER" "$@"\n',
            )
            adapter.chmod(0o755)
            variables = {
                "PATH": directory + os.pathsep + os.environ["PATH"],
                "CORVUS_GEMATO": gemato,
                "PORTAGE_KEYSERVER": os.environ.get(
                    "PORTAGE_KEYSERVER", "hkps://keys.gentoo.org"
                ),
            }
            for command in ("emerge-webrsync -k", "emerge --sync"):
                for attempt in range(1, 4):
                    try:
                        self.command_string(context, command, **variables)
                        break
                    except subprocess.CalledProcessError:
                        if attempt == 3:
                            raise
                        print(
                            f"{command} failed; retrying ({attempt + 1}/3) "
                            "with key refresh and signature verification enabled.",
                            flush=True,
                        )
                        time.sleep(5)
