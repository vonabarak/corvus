"""Run image Makefiles in an isolated tree with recorded external commands."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path

from corvus_test_harness.host_binary import REPO_ROOT


@dataclass
class RecipeCall:
    command: str
    args: list[str]
    cwd: Path


class RecipeRunner:
    def __init__(self, directory: Path) -> None:
        self.root = directory
        self.log = directory / "calls.jsonl"
        shutil.copytree(
            REPO_ROOT / "yaml",
            directory / "yaml",
            ignore=shutil.ignore_patterns("build", "cache"),
        )
        keys = directory / "integration_tests" / "keys"
        keys.mkdir(parents=True)
        shutil.copyfile(
            REPO_ROOT / "integration_tests/keys/Makefile", keys / "Makefile"
        )
        (keys / "corvus-test-key").write_text("test private key")
        (keys / "corvus-test-key.pub").write_text("test public key")
        self.bin = directory / "bin"
        self.bin.mkdir()
        stub = Path(__file__).with_name("image_recipe_command.py")
        for name in ("crv", "curl"):
            command = self.bin / name
            shutil.copyfile(stub, command)
            command.chmod(0o755)

    def run(
        self,
        recipe: str,
        target: str,
        variables: tuple[str, ...] = (),
        env: dict[str, str] | None = None,
    ) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [
                "make",
                "--no-print-directory",
                "-j4",
                target,
                f"CRV={self.bin / 'crv'}",
                f"CURL={self.bin / 'curl'}",
                *variables,
            ],
            cwd=self.root / "yaml" / recipe,
            env={**os.environ, "RECIPE_CALL_LOG": str(self.log), **(env or {})},
            capture_output=True,
            text=True,
            check=False,
            timeout=30,
        )

    def calls(self) -> list[RecipeCall]:
        if not self.log.exists():
            return []
        result = []
        for line in self.log.read_text().splitlines():
            call = json.loads(line)
            result.append(RecipeCall(call["command"], call["args"], Path(call["cwd"])))
        return result
