"""Exact byte capacity parsing and formatting shared by human clients."""

import pytest
from corvus_client.sizes import MAX_SIZE, format_size, parse_size, validate_size


@pytest.mark.parametrize("size", [1, 1024, 1537, 1536 * 1024**2, 2**53 + 1, MAX_SIZE])
def test_exact_round_trip(size: int) -> None:
    assert parse_size(format_size(size)) == size


@pytest.mark.parametrize("text", ["1", "1.5G", "0B", "-1M", "1KB", "8388608T"])
def test_invalid_input(text: str) -> None:
    with pytest.raises(ValueError):
        parse_size(text)


@pytest.mark.parametrize("value", [True, 0, -1, 1.5, "1G", MAX_SIZE + 1])
def test_api_requires_integer_bytes(value: object) -> None:
    with pytest.raises(ValueError):
        validate_size(value)  # type: ignore[arg-type]


def test_ram_alignment_and_binary_units() -> None:
    assert parse_size("1g", ram=True) == 1024**3
    assert format_size(1536 * 1024**2) == "1536M"
    assert format_size(1537) == "1537B"
    assert format_size(0) == "0B"
    assert format_size(None) == "—"
    with pytest.raises(ValueError):
        parse_size("1537B", ram=True)
