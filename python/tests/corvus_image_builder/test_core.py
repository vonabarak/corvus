from pathlib import Path

import pytest
from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe, Stage


class Producer(Stage):
    produces = frozenset({"root"})

    def execute(self, context: BuildContext) -> None:
        context.artifacts["root"] = context.root


class Consumer(Stage):
    requires = frozenset({"root"})

    def execute(self, context: BuildContext) -> None:
        assert context.artifacts["root"] == context.root


def test_dependency_order() -> None:
    context = BuildContext(Path("/unused"))
    with pytest.raises(ValueError, match="requires"):
        BuildManager().plan(ImageRecipe([Consumer(), Producer()]), context)
    BuildManager().run(ImageRecipe([Producer(), Consumer()]), context)


def test_cleanup_continues_after_error() -> None:
    context = BuildContext(Path("/unused"))
    released: list[str] = []
    context.own(lambda: released.append("first"))

    def fail() -> None:
        raise OSError("busy")

    context.own(fail)
    with pytest.raises(RuntimeError, match="cleanup failed"):
        context.close(failed=False)
    assert released == ["first"]


def test_failure_preserves_mount_but_releases_lock() -> None:
    context = BuildContext(Path("/unused"))
    released: list[str] = []
    context.own(lambda: released.append("mount"), preserve_on_failure=True)
    context.own(lambda: released.append("lock"))
    context.close(failed=True)
    assert released == ["lock"]


def test_run_restores_signal_handlers_on_interrupt(tmp_path: Path) -> None:
    import signal

    import pytest
    from corvus_image_builder.core import BuildContext, BuildManager, ImageRecipe, Stage

    released: list[bool] = []

    class Interrupted(Stage):
        def execute(self, context: BuildContext) -> None:
            context.own(lambda: released.append(True))
            signal.raise_signal(signal.SIGTERM)

    before = {sig: signal.getsignal(sig) for sig in (signal.SIGTERM, signal.SIGHUP)}
    with pytest.raises(KeyboardInterrupt):
        BuildManager().run(ImageRecipe([Interrupted()]), BuildContext(tmp_path))
    assert released == [True]
    assert all(signal.getsignal(sig) == handler for sig, handler in before.items())


def test_command_input_and_timeout_cleanup(tmp_path: Path) -> None:
    import os
    import subprocess
    import sys

    from corvus_image_builder.core import CommandRunner

    destination = tmp_path / "stdin"
    CommandRunner().run(
        [
            sys.executable,
            "-c",
            "import pathlib, sys; pathlib.Path(sys.argv[1]).write_bytes(sys.stdin.buffer.read())",
            str(destination),
        ],
        env=dict(os.environ),
        input=b"corvus:corvus\n",
    )
    assert destination.read_bytes() == b"corvus:corvus\n"
    with pytest.raises(subprocess.TimeoutExpired):
        CommandRunner().run(
            [sys.executable, "-c", "import time; time.sleep(30)"],
            env=dict(os.environ),
            input=b"data",
            timeout=0.1,
        )
