"""Template endpoints: list, detail, create, update, instantiate, delete.

Create + update both take raw YAML and pair with the Monaco editor on
the frontend. The daemon parses YAML into TemplateDetails; the
operator iterates by editing locally or in the browser.
"""

from __future__ import annotations

from dataclasses import asdict, is_dataclass
from typing import TYPE_CHECKING, Annotated, Any

from corvus_client.exceptions import CorvusError, TemplateNotFound
from corvus_client.types import (
    CloudInitInfo,
    TemplateDetails,
    TemplateDriveInfo,
    TemplateNetIfInfo,
    TemplateSshKeyInfo,
)
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field

import yaml as yamllib

from ..deps import get_client

if TYPE_CHECKING:
    from corvus_client import AsyncClient

router = APIRouter(prefix="/templates", tags=["templates"])

ClientDep = Annotated["AsyncClient", Depends(get_client)]


def _as_dict(obj: Any) -> Any:
    if is_dataclass(obj) and not isinstance(obj, type):
        return {k: _as_dict(v) for k, v in asdict(obj).items()}
    if isinstance(obj, list | tuple):
        return [_as_dict(v) for v in obj]
    return obj


class InstantiateBody(BaseModel):
    name: str = Field(..., min_length=1, description="Name for the new VM.")
    node: str | None = Field(
        None, description="Optional node name or id to pin placement to."
    )


class TemplateYamlBody(BaseModel):
    """Wrapper for the raw YAML document. POST and PUT both use it.

    The daemon parses + validates YAML server-side; bad documents come
    back as CorvusError → 400 with the parser's message verbatim."""

    yaml: str = Field(..., min_length=1, description="Template YAML document.")


# ---- YAML round-trip helpers --------------------------------------------
#
# The daemon stores parsed TemplateDetails, not the raw YAML the operator
# submitted, so re-rendering for the edit-mode editor needs an encoder.
# Mirror the Haskell encoder at src/Corvus/Client/Commands/Template/Yaml.hs
# field-for-field — both feed the same daemon parser, so any drift between
# the two encoders would silently corrupt round-trips.


def _drive_to_dict(d: TemplateDriveInfo) -> dict[str, Any]:
    out: dict[str, Any] = {
        "interface": d.interface,
        "readOnly": d.read_only,
        "cacheType": d.cache_type,
        "discard": d.discard,
        "strategy": d.clone_strategy,
    }
    if d.disk_image_name is not None:
        out["diskImageName"] = d.disk_image_name
    if d.media is not None:
        out["media"] = d.media
    if d.size_mb is not None:
        out["sizeMb"] = d.size_mb
    if d.format is not None:
        out["format"] = d.format
    return out


def _net_if_to_dict(n: TemplateNetIfInfo) -> dict[str, Any]:
    out: dict[str, Any] = {"type": n.type}
    if n.host_device is not None:
        out["hostDevice"] = n.host_device
    return out


def _ssh_key_to_dict(k: TemplateSshKeyInfo) -> dict[str, Any]:
    # Match the Haskell encoder: {name: ...} maps, not bare strings.
    return {"name": k.name}


def _cloud_init_to_dict(c: CloudInitInfo) -> dict[str, Any]:
    out: dict[str, Any] = {"injectSshKeys": c.inject_ssh_keys}
    if c.user_data is not None:
        out["userData"] = c.user_data
    if c.network_config is not None:
        out["networkConfig"] = c.network_config
    return out


def template_details_to_yaml(t: TemplateDetails) -> str:
    """Render a TemplateDetails as YAML accepted by POST /api/templates
    and PUT /api/templates/{id}.

    Mirrors templateDetailsToYaml in src/Corvus/Client/Commands/Template/
    Yaml.hs — kept lockstep with that encoder so round-trips through the
    web UI behave identically to ``crv template show -o yaml``."""
    doc: dict[str, Any] = {
        "name": t.name,
        "cpuCount": t.cpu_count,
        "ramMb": t.ram_mb,
        "headless": t.headless,
        "cloudInit": t.cloud_init,
        "guestAgent": t.guest_agent,
        "autostart": t.autostart,
        "drives": [_drive_to_dict(d) for d in t.drives],
        "networkInterfaces": [_net_if_to_dict(n) for n in t.net_ifs],
        "sshKeys": [_ssh_key_to_dict(k) for k in t.ssh_keys],
    }
    if t.description is not None:
        doc["description"] = t.description
    if t.cloud_init_config is not None:
        doc["cloudInitConfig"] = _cloud_init_to_dict(t.cloud_init_config)
    # default_flow_style=False forces block style; sort_keys=False keeps
    # the field order above (name first, then config, then nested
    # resources).
    return yamllib.safe_dump(doc, sort_keys=False, default_flow_style=False)


@router.get("")
async def list_templates(client: ClientDep) -> list[dict[str, Any]]:
    return [_as_dict(t) for t in await client.templates.list()]


@router.get("/{template_id}")
async def get_template(template_id: int, client: ClientDep) -> dict[str, Any]:
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return _as_dict(await tmpl.show())


@router.get("/{template_id}/yaml")
async def get_template_yaml(template_id: int, client: ClientDep) -> dict[str, str]:
    """Re-render the template's stored TemplateDetails as YAML compatible
    with POST / PUT /api/templates. The web editor uses this to pre-fill
    the edit form so operators don't start from a blank slate."""
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    details = await tmpl.show()
    return {"yaml": template_details_to_yaml(details)}


@router.post("")
async def create_template(body: TemplateYamlBody, client: ClientDep) -> dict[str, Any]:
    """Create a new template from the YAML document. Returns the
    parsed TemplateDetails so the frontend can route to its detail
    page."""
    try:
        tmpl = await client.templates.create(body.yaml)
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    return _as_dict(await tmpl.show())


@router.put("/{template_id}")
async def update_template(
    template_id: int, body: TemplateYamlBody, client: ClientDep
) -> dict[str, Any]:
    """Replace the template's contents with the new YAML. Existing VMs
    instantiated from this template are not modified — the daemon
    only updates the template record itself."""
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    try:
        await tmpl.update(body.yaml)
    except CorvusError as exc:
        raise HTTPException(status_code=400, detail=str(exc)) from exc
    # Fetch the post-update details so the frontend can refresh.
    return _as_dict(await tmpl.show())


@router.post("/{template_id}/instantiate")
async def instantiate_template(
    template_id: int, body: InstantiateBody, client: ClientDep
) -> dict[str, Any]:
    """Create a new VM from the template. Returns the resulting VM's
    detail payload so the frontend can route straight to it."""
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    vm = await tmpl.instantiate(body.name, node=body.node)
    details = await vm.show()
    return _as_dict(details)


@router.delete("/{template_id}")
async def delete_template(template_id: int, client: ClientDep) -> dict[str, str]:
    try:
        tmpl = await client.templates.get(template_id)
    except TemplateNotFound as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    await tmpl.delete()
    return {"status": "deleted"}
