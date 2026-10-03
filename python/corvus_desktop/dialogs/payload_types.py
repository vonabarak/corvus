"""Key-specific values returned by desktop dialogs."""

from __future__ import annotations

from typing import TypedDict


class VmCreatePayload(TypedDict):
    name: str
    node: str | None
    cpu_count: int
    ram_mb: int
    description: str | None
    cpu_model: str
    graphics_adapter: str
    headless: bool
    guest_agent: bool
    tpm: bool
    cloud_init: bool
    autostart: bool
    reboot_quirk: bool


class VmEditPayload(TypedDict, total=False):
    name: str
    cpu_count: int
    ram_mb: int
    description: str
    cpu_model: str
    graphics_adapter: str
    headless: bool
    guest_agent: bool
    tpm: bool
    cloud_init: bool
    autostart: bool
    reboot_quirk: bool


class NodePayload(TypedDict):
    name: str
    host: str
    node_agent_port: int
    net_agent_port: int
    base_path: str
    description: str | None
    admin_state: str
    netd_disabled: bool


class NodeEditPayload(TypedDict, total=False):
    name: str
    host: str
    node_agent_port: int
    net_agent_port: int
    base_path: str
    description: str
    admin_state: str
    netd_disabled: bool


class NetworkCreatePayload(TypedDict):
    name: str
    subnet: str
    node: int | None
    dhcp: bool
    nat: bool
    autostart: bool


class NetworkEditPayload(TypedDict, total=False):
    subnet: str
    dhcp: bool
    nat: bool
    autostart: bool


class AttachDiskPayload(TypedDict):
    disk_ref: int
    interface: str
    media: str
    read_only: bool
    cache_type: str | None
    discard: bool


class AddNetIfPayload(TypedDict):
    type: str
    host_device: str | None
    mac_address: str | None
    network_ref: int | None


class AddSharedDirPayload(TypedDict):
    path: str
    tag: str
    cache: str | None
    read_only: bool


class EntityRefPayload(TypedDict):
    to_node_ref: int


class AttachSshKeyPayload(TypedDict):
    key_ref: int


class SshKeyAddPayload(TypedDict):
    name: str
    public_key: str


class TemplateInstantiatePayload(TypedDict):
    name: str
    node: int | None


class DiskRebasePayload(TypedDict):
    new_backing_disk_ref: int


class DiskCopyMovePayload(TypedDict):
    to_node_ref: str
    to_path: str | None
    with_backing_chain: bool
