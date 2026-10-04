"""Public QEMU device model names and Cap'n Proto enum names."""

_AUDIO = {
    "virtio-sound": "virtioSound",
    "intel-hda": "intelHda",
    "ich9-intel-hda": "ich9IntelHda",
    "AC97": "ac97",
}
_NETWORK = {
    "virtio-net-pci": "virtioNetPci",
    "virtio-net-pci-non-transitional": "virtioNetPciNonTransitional",
    "virtio-net-pci-transitional": "virtioNetPciTransitional",
    "e1000": "e1000",
}


def audio_to_wire(model: str) -> str:
    return _AUDIO[model]


def audio_from_wire(model: object) -> str:
    return {value: key for key, value in _AUDIO.items()}[str(model)]


def network_to_wire(model: str) -> str:
    return _NETWORK[model]


def network_from_wire(model: object) -> str:
    return {value: key for key, value in _NETWORK.items()}[str(model)]
