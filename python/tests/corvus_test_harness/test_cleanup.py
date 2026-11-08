"""Unexpected cleanup errors are collected rather than silently discarded."""

from unittest.mock import Mock

from corvus_client import DiskNotFound, VmNotFound
from corvus_test_harness.cleanup import ResourceCleanup


def test_cleanup_ignores_only_the_expected_missing_type() -> None:
    cleanup = ResourceCleanup()
    cleanup._cleanup_errors = []
    cleanup.cleanup_resource(Mock(side_effect=VmNotFound("gone")), VmNotFound)
    assert cleanup._cleanup_errors == []
    failure = DiskNotFound("wrong resource")
    cleanup.cleanup_resource(Mock(side_effect=failure), VmNotFound)
    assert cleanup._cleanup_errors == [failure]


def test_cleanup_attempts_remaining_resources_after_failure() -> None:
    cleanup = ResourceCleanup()
    cleanup._cleanup_errors = []
    failure = RuntimeError("delete failed")
    later = Mock()
    cleanup.cleanup_resource(Mock(side_effect=failure), VmNotFound)
    cleanup.cleanup_resource(later, VmNotFound)
    later.assert_called_once()
    assert cleanup._cleanup_errors == [failure]
