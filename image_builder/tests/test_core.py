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
