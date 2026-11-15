"""Sequential stage execution and owned resources."""

import os
import signal
import subprocess
import threading
from abc import ABC, abstractmethod
from collections.abc import Callable, Sequence
from dataclasses import dataclass, field
from pathlib import Path


class CommandRunner:
    """Run commands in their own process group with timeout and interruption cleanup."""

    def run(
        self,
        argv: Sequence[str],
        *,
        env: dict[str, str],
        timeout: float = 28800,
        input: bytes | None = None,
    ) -> None:
        process = subprocess.Popen(
            list(argv),
            env=env,
            start_new_session=True,
            stdin=subprocess.PIPE if input is not None else None,
        )
        try:
            if input is None:
                process.wait(timeout=timeout)
            else:
                process.communicate(input=input, timeout=timeout)
            result = process.returncode
        except BaseException:
            try:
                os.killpg(process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                process.wait(timeout=10)
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(process.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                process.wait()
            raise
        if result:
            raise subprocess.CalledProcessError(result, list(argv))


@dataclass
class BuildContext:
    """Share the target root, command runner, artifacts, and cleanup callbacks.

    Resources are released in reverse acquisition order. Callbacks marked
    preserve_on_failure are skipped after a failed build to aid diagnosis.
    """

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
    """Define one build step with prerequisite and output artifact names.

    Subclasses implement execute, optionally adding validation and verification.
    Execution must publish the declared outputs in context.artifacts.
    """

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
    """Hold an explicitly ordered sequence of stages for one image flavor."""

    stages: Sequence[Stage]


class BuildManager:
    """Check stage ordering, validate prerequisites, and execute a recipe.

    Each stage is verified immediately after execution. Resource cleanup runs
    on success, failure, and interruption; stages are never reordered or resumed.
    """

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
        handlers = {}

        def interrupted(signum: int, frame: object) -> None:
            raise KeyboardInterrupt(f"Interrupted by signal {signum}")

        if threading.current_thread() is threading.main_thread():
            for sig in (signal.SIGTERM, signal.SIGHUP):
                handlers[sig] = signal.signal(sig, interrupted)
        try:
            for stage in recipe.stages:
                stage.validate(context)
            for stage in recipe.stages:
                print(f"Executing {type(stage).__name__}", flush=True)
                stage.execute(context)
                stage.verify(context)
            failed = False
        finally:
            try:
                context.close(failed=failed)
            finally:
                for sig, handler in handlers.items():
                    signal.signal(sig, handler)
