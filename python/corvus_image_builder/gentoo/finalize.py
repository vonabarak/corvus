"""Clean target identities and build residue before publication."""

import json
import subprocess
import time

from ..core import BuildContext
from ._files import remove, write
from .base import GentooStage
from .mounts import Mounts


class FinalizeStage(GentooStage):
    """Reset clone identity, retire temporary mounts, clean, and record metadata.

    Override reset_identity, clean, or metadata for flavor-specific policy.
    Corvus performs final disk compaction and publication.
    """

    requires = frozenset({"bootloader"})
    produces = frozenset({"image"})

    def reset_identity(self, context: BuildContext) -> None:
        for key in (context.root / "etc/ssh").glob("ssh_host_*"):
            key.unlink()
        write(context.root / "etc/machine-id", "")
        self.chroot(context, "passwd", "-l", "root")
        remove(context.root, "var/lib/cloud")

    def clean(self, context: BuildContext) -> None:
        for relative in (
            "var/db/repos",
            "var/cache/binpkgs",
            "var/cache/binhost",
            "var/cache/distfiles",
            "var/tmp",
            "tmp",
            "var/log/portage",
            "var/lib/portage/elog",
            "var/lib/portage/failed",
            "var/lib/portage/logs",
            "usr/local/lib/corvus-build",
        ):
            remove(context.root, relative)
        for path in (context.root / "var/log").glob("emerge*"):
            remove(context.root, str(path.relative_to(context.root)))
        for relative in ("tmp", "var/tmp"):
            path = context.root / relative
            path.mkdir(parents=True, exist_ok=True)
            path.chmod(0o1777)

    def metadata(self, context: BuildContext) -> dict[str, object]:
        return {
            "image": self.settings.name,
            "kernel": context.artifacts["kernel"],
            "built": time.time(),
            "builder": "corvus-image-builder",
        }

    def execute(self, context: BuildContext) -> None:
        self.reset_identity(context)
        mounts = context.artifacts.get("chroot-mounts")
        if isinstance(mounts, Mounts):
            mounts.close()
        self.clean(context)
        write(
            context.root / "etc/corvus-image-info",
            json.dumps(self.metadata(context)) + "\n",
        )
        self.command(context, "sync")
        for filesystem in (context.root, context.root / "boot/efi"):
            try:
                self.command(context, "fstrim", str(filesystem))
            except subprocess.CalledProcessError:
                print(
                    f"Discard unavailable for {filesystem}; continuing publication.",
                    flush=True,
                )
        context.artifacts["image"] = context.root
