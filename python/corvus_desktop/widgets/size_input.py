"""Size entry with exact byte values and binary suffixes."""

from corvus_client.sizes import MIB, format_size, parse_size
from PySide6.QtWidgets import QLineEdit


class SizeInput(QLineEdit):
    def __init__(self, *, ram: bool = False) -> None:
        super().__init__()
        self._ram = ram
        self.setPlaceholderText("1G" if ram else "10G")

    def value(self) -> int:
        value = parse_size(self.text(), ram=self._ram)
        if self._ram and value < 64 * MIB:
            raise ValueError("RAM must be at least 64M")
        return value

    def setValue(self, value: int) -> None:
        self.setText(format_size(value))
