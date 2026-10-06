"""The disk-specific selector convention is shared by every image API."""

import pytest
from corvus_client._entityref import disk_entity_ref


@pytest.mark.parametrize("value", [123, "123", "00123"])
def test_ids(value: int | str) -> None:
    assert disk_entity_ref(value).id == 123


@pytest.mark.parametrize("value", ["ubuntu", "ubuntu:24.04", "ubuntu:latest"])
def test_names(value: str) -> None:
    assert disk_entity_ref(value).name == value


@pytest.mark.parametrize(
    "value", [0, -1, 1 << 63, "123abc", "123:latest", "0", "ubuntu:", "u:a:b"]
)
def test_invalid(value: int | str) -> None:
    with pytest.raises(ValueError):
        disk_entity_ref(value)
