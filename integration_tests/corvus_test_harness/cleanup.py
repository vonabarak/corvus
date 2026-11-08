"""Attempt all owned-resource cleanup and report unexpected teardown failures."""

from collections.abc import Callable, Iterator

import pytest
from corvus_client import DiskNotFound, NetworkNotFound, SnapshotNotFound, VmNotFound


class ResourceCleanup:
    @pytest.fixture(autouse=True)
    def _resource_cleanup_errors(self) -> Iterator[None]:
        self._cleanup_errors: list[Exception] = []
        yield
        if self._cleanup_errors:
            raise RuntimeError(
                "Owned resource cleanup failed: "
                + "; ".join(str(error) for error in self._cleanup_errors)
            ) from self._cleanup_errors[0]

    def cleanup_resource(
        self,
        operation: Callable[[], object],
        missing: type[Exception] | tuple[type[Exception], ...] = (
            DiskNotFound,
            NetworkNotFound,
            SnapshotNotFound,
            VmNotFound,
        ),
    ) -> None:
        try:
            operation()
        except missing:
            pass
        except Exception as error:
            self._cleanup_errors.append(error)
