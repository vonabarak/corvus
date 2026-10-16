"""Offline authoring-schema checks; no client preprocessing or daemon required."""

import json
import subprocess
from pathlib import Path

import pytest
from jsonschema import Draft7Validator

import yaml

ROOT = Path(__file__).resolve().parents[2]
SCHEMA = ROOT / "schema/json/build.schema.json"


@pytest.fixture(scope="module")
def validator() -> Draft7Validator:
    schema = json.loads(SCHEMA.read_text())
    Draft7Validator.check_schema(schema)
    return Draft7Validator(schema)


def test_repository_recipes(validator: Draft7Validator) -> None:
    paths = subprocess.check_output(
        ["git", "ls-files", "--cached", "--others", "--exclude-standard", "yaml"],
        cwd=ROOT,
        text=True,
    ).splitlines()
    recipes = 0
    for relative in paths:
        path = ROOT / relative
        if not path.is_file() or path.suffix not in {".yml", ".yaml"}:
            continue
        text = path.read_text()
        document = yaml.safe_load(text)
        if not isinstance(document, dict) or "pipeline" not in document:
            continue
        recipes += 1
        errors = list(validator.iter_errors(document))
        assert not errors, f"{relative}: {[error.message for error in errors]}"
        prefix = "# yaml-language-server: $schema="
        assert text.startswith(prefix), f"Missing schema modeline: {relative}"
        target = text.splitlines()[0].removeprefix(prefix)
        assert (path.parent / target).resolve() == SCHEMA
    assert recipes > 0, "No tracked build recipes discovered"


@pytest.mark.parametrize(
    "document",
    [
        "pipeline: [{build: {name: x, template: t, strategy: offline-overlay, sourceImage: 'base:stable', target: {}}}]",
        "pipeline: [{build: {name: x, template: t, strategy: offline-overlay, sourceImage: 42, target: {size: '3G', format: raw}}}]",
        "pipeline: [{build: {name: x, template: t, strategy: '{{ strategy }}', sourceImage: '{{ source }}', target: {}}}]",
        "pipeline: []",
        "vars: null\npipeline: []",
        "vars: {url: null, enabled: true, count: 3, ratio: 1.5}\npipeline: []",
        "_defaults: &defaults {format: qcow2}\npipeline: [{build: {name: x, template: t, target: {<<: *defaults}}}]",
        "pipeline: [{upload: {name: x, from: x.raw, format: RAW, ephemeral: null}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, vm: {ram: '512M', startupTimeoutSec: 4294967}, cleanup: onSuccess}}]",
        "pipeline: [{build: {name: x, template: t, target: {format: '{{ format }}', size: '{{ size }}'}, provisioners: [{shell: 'echo hello'}, {shell: {script: scripts/setup.sh, env: {PORT: '80'}}}, {file: {from: file.txt, to: /tmp/file, mode: '0644'}}, {file: {content: aGk=, to: /tmp/file}}, {wait-for: {ping: true}}, {wait-for: {file: /tmp/ready}}, {wait-for: {port: 80}}, {wait-for: {inline: 'test -f /tmp/ready', interpreter: sh}}, {wait-for: {script: ready.sh}}, {reboot: {}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{shell: {inline: 'true', script: null}}]}}]",
        "pipeline: [{apply: {ifExists: overwrite, sshKeys: [{name: key, publicKey: 'ssh-ed25519 test'}], disks: [{name: base, size: '1g', checksum: {algorithm: SHA256, value: '{{ checksum }}'}}], networks: [{name: lan, dhcp: true}], vms: [{name: vm, cpuCount: 2, ram: '2G', sshKeys: [key], drives: [{disk: base, interface: virtio}], networkInterfaces: [{network: lan}], sharedDirs: [{path: /tmp, tag: host}], audioDevices: [{backend: pipewire}]}]}}]",
        "pipeline: [{apply: {templates: [{name: t, cpuCount: 2, ram: '2G', drives: [{diskImage: 42, interface: virtio, strategy: overlay}, {diskImage: 'base:latest', interface: scsi, strategy: direct}], networkInterfaces: [{type: user}], sshKeys: [{name: key}], cloudInitConfig: {userData: {users: [alice], arbitrary: {anything: true}}, networkConfig: {version: 2}, injectSshKeys: false}}]}}]",
        "pipeline: [{apply: {vms: [{name: vm, cpuCount: 1, ram: '1G', cloudInitConfig: {userData: '#cloud-config', networkConfig: null}}], templates: null, ifExists: null}}]",
    ],
)
def test_valid_authoring_documents(validator: Draft7Validator, document: str) -> None:
    validator.validate(yaml.safe_load(document))


@pytest.mark.parametrize(
    "document",
    [
        "pipeline: [{build: {name: x, template: t, strategy: offline-overlay, target: {}}}]",
        "pipeline: [{build: {name: x, template: t, sourceImage: base, target: {}}}]",
        "pipeline: [{build: {name: x, template: t, strategy: from-scratch, sourceImage: base, target: {}}}]",
        "pipeline: [{build: {name: x, template: t, strategy: offline-overlay, sourceImage: 0, target: {}}}]",
        "{}",
        "pipeline: {}",
        "pipline: []\npipeline: []",
        "vars: {nested: {x: 1}}\npipeline: []",
        "pipeline: [{}]",
        "pipeline: [{build: null}]",
        "pipeline: [{apply: {}, upload: {name: x, from: x, format: raw}}]",
        "pipeline: [{upload: {name: x, from: x}}]",
        "pipeline: [{build: {name: x, template: t}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, floppy: x}}]",
        "pipeline: [{build: {name: x, template: t, target: {sizeGb: 10}}}]",
        "pipeline: [{build: {name: x, template: t, target: {size: 10}}}]",
        "pipeline: [{build: {name: x, template: t, target: {size: '0G'}}}]",
        "pipeline: [{build: {name: x, template: t, target: {size: '1.5G'}}}]",
        "pipeline: [{build: {name: x, template: t, target: {format: typo}}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, vm: {cpuCount: '{{ cpus }}'}}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, vm: {startupTimeoutSec: 0}}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, vm: {startupTimeoutSec: 4294968}}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{shell: {}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{shell: {inline: x, script: x}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{shell: {inline: x, env: {PORT: 80}}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{file: {to: /tmp/file}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{file: {to: /tmp/file, from: x, content: eA==}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{wait-for: {ping: false}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{wait-for: {ping: true, port: 80}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{wait-for: {command: x}}]}}]",
        "pipeline: [{build: {name: x, template: t, target: {}, provisioners: [{shell: x, reboot: {}}]}}]",
        "pipeline: [{apply: {ifExists: update}}]",
        "pipeline: [{apply: {vms: [{name: vm, cpuCount: 1, ramMb: 1024}]}}]",
        "pipeline: [{apply: {vms: [{name: vm, cpuCount: 1, ram: '1G', networkInterfaces: [{network: lan, type: user}]}]}}]",
        "pipeline: [{apply: {templates: [{name: t, cpuCount: 1, ram: '1G', drives: [], networkInterfaces: [{}]}]}}]",
        "pipeline: [{apply: {templates: [{name: t, cpuCount: 1, ram: '1G', drives: [{interface: virtio, strategy: clone, diskImage: 0}]}]}}]",
        "pipeline: [{apply: {templates: [{name: t, cpuCount: 1, ram: '1G', drives: [], sshKeys: [key]}]}}]",
        "pipeline: [{apply: {vms: [{name: vm, cpuCount: 1, ram: '1G', cloudInitConfig: {userDate: {}}}]}}]",
        "pipeline: [{apply: {disks: [{name: x, checksum: {algorithm: sha999, value: x}}]}}]",
    ],
)
def test_invalid_authoring_documents(validator: Draft7Validator, document: str) -> None:
    assert not validator.is_valid(yaml.safe_load(document))


@pytest.mark.parametrize(
    "strategy", ["overlay", "offline-overlay", "installer", "from-scratch"]
)
@pytest.mark.parametrize("rebase", [True, False, None])
@pytest.mark.parametrize("format", ["qcow2", "raw"])
def test_target_rebase(
    validator: Draft7Validator, strategy: str, rebase: bool | None, format: str
) -> None:
    build = {
        "name": "x",
        "template": "t",
        "strategy": strategy,
        "target": {"rebase": rebase, "format": format},
    }
    if strategy == "offline-overlay":
        build["sourceImage"] = "base"
    valid = strategy in {"overlay", "offline-overlay"} and not (
        rebase is False and format == "raw"
    )
    assert validator.is_valid({"pipeline": [{"build": build}]}) == valid
