"""Public graphics adapter names and Cap'n Proto enum names."""

ADAPTER_TO_WIRE = {
    "virtio-vga": "virtioVga",
    "qxl-vga": "qxlVga",
    "vga": "vga",
    "virtio-gpu-pci": "virtioGpuPci",
    "virtio-vga-gl": "virtioVgaGl",
    "virtio-gpu-gl-pci": "virtioGpuGlPci",
}
WIRE_TO_ADAPTER = {wire: adapter for adapter, wire in ADAPTER_TO_WIRE.items()}


def to_wire(adapter: str) -> str:
    try:
        return ADAPTER_TO_WIRE[adapter]
    except KeyError as exc:
        raise ValueError(f"Unknown graphics adapter: {adapter}") from exc


def from_wire(wire: object) -> str:
    try:
        return WIRE_TO_ADAPTER[str(wire)]
    except KeyError as exc:
        raise ValueError(f"Unknown graphics adapter enum: {wire}") from exc
