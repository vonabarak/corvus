"""Composable stages for native Gentoo image builds."""

from ._files import link, remove, write
from .base import GentooStage
from .bootloader import BootloaderStage
from .disk import DiskStage, ExistingImageStage
from .finalize import FinalizeStage
from .kernel import KernelStage
from .mounts import ChrootEnvironmentStage
from .openrc import OpenRCConfigurationStage
from .packages import (
    GlibcPackageInstallStage,
    MuslPackageInstallStage,
    PackageInstallStage,
)
from .repository import RepositoryStage
from .settings import Settings
from .systemd import SystemdConfigurationStage
from .workspace import WorkspaceStage

__all__ = [
    "BootloaderStage",
    "ChrootEnvironmentStage",
    "DiskStage",
    "ExistingImageStage",
    "FinalizeStage",
    "GentooStage",
    "GlibcPackageInstallStage",
    "KernelStage",
    "MuslPackageInstallStage",
    "OpenRCConfigurationStage",
    "PackageInstallStage",
    "RepositoryStage",
    "Settings",
    "SystemdConfigurationStage",
    "WorkspaceStage",
    "link",
    "remove",
    "write",
]
