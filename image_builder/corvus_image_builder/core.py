"""Sequential stage execution and owned resources."""

import os
import signal
import subprocess
from abc import ABC, abstractmethod
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from pathlib import Path


class CommandRunner:
    def run(
        self, argv: Sequence[str], *, env: dict[str, str], timeout: float = 28800
    ) -> None:
        process = subprocess.Popen(list(argv), env=env, start_new_session=True)
        try:
            result = process.wait(timeout=timeout)
        except BaseException:
            os.killpg(process.pid, signal.SIGTERM)
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait()
            raise
        if result:
            raise subprocess.CalledProcessError(result, list(argv))


@dataclass
class BuildContext:
    root: Path
    runner: CommandRunner = field(default_factory=CommandRunner)
    artifacts: dict[str, object] = field(default_factory=dict)
    resources: list[tuple[Callable[[], None], bool]] = field(default_factory=list)

    def own(
        self, release: Callable[[], None], *, preserve_on_failure: bool = False
    ) -> None:
        self.resources.append((release, preserve_on_failure))

    def close(self, *, failed: bool) -> None:
        errors = []
        for release, preserve in reversed(self.resources):
            if failed and preserve:
                continue
            try:
                release()
            except Exception as error:
                errors.append(error)
        self.resources.clear()
        if errors:
            raise RuntimeError(f"Resource cleanup failed: {errors}") from errors[0]


class Stage(ABC):
    requires: frozenset[str] = frozenset()
    produces: frozenset[str] = frozenset()

    def validate(self, context: BuildContext) -> None:
        """Override to check prerequisites without acquiring resources."""
        return None

    @abstractmethod
    def execute(self, context: BuildContext) -> None:
        pass

    def verify(self, context: BuildContext) -> None:
        missing = self.produces.difference(context.artifacts)
        if missing:
            raise RuntimeError(f"Stage did not produce {sorted(missing)}")


@dataclass
class ImageRecipe:
    stages: Sequence[Stage]


class BuildManager:
    def plan(self, recipe: ImageRecipe, context: BuildContext) -> list[str]:
        available = set(context.artifacts)
        for stage in recipe.stages:
            missing = stage.requires.difference(available)
            if missing:
                raise ValueError(f"{type(stage).__name__} requires {sorted(missing)}")
            available.update(stage.produces)
        return [type(stage).__name__ for stage in recipe.stages]

    def run(self, recipe: ImageRecipe, context: BuildContext) -> None:
        self.plan(recipe, context)
        failed = True
        try:
            for stage in recipe.stages:
                stage.validate(context)
            for stage in recipe.stages:
                print(f"Executing {type(stage).__name__}", flush=True)
                stage.execute(context)
                stage.verify(context)
            failed = False
        finally:
            context.close(failed=failed)
