"""Command strings retain argument boundaries without executing shell syntax."""

from pathlib import Path
from shlex import quote
from unittest.mock import Mock, patch

import pytest
from corvus_image_builder.core import BuildContext, CommandRunner
from corvus_image_builder.gentoo import GentooStage, Settings


class CommandStage(GentooStage):
    """Expose command helpers without performing image operations."""

    def execute(self, context: BuildContext) -> None:
        pass


@pytest.mark.parametrize("chroot", [False, True])
def test_command_string_quoting_and_environment(tmp_path: Path, chroot: bool) -> None:
    runner = Mock(spec=CommandRunner)
    context = BuildContext(tmp_path / "image root", runner=runner)
    stage = CommandStage(Settings(Path("/dev/vdb"), tmp_path, None))
    argument = "spaces 'quotes' $(touch unwanted); *"
    quoted_argument = quote(argument)
    command = f"printf '%s' {quoted_argument}"
    with patch.dict("os.environ", ROOT="/wrong", PORTAGE_TMPDIR="/cache"):
        if chroot:
            stage.chroot_string(context, command, input=b"input")
        else:
            stage.command_string(context, command, CUSTOM="value")
    call = runner.run.call_args
    prefix = ("chroot", str(context.root)) if chroot else ()
    assert call.args[0] == (*prefix, "printf", "%s", argument)
    assert "ROOT" not in call.kwargs["env"]
    if chroot:
        assert "PORTAGE_TMPDIR" not in call.kwargs["env"]
        assert call.kwargs["input"] == b"input"
    else:
        assert call.kwargs["env"]["CUSTOM"] == "value"


@pytest.mark.parametrize("method", ["command_string", "chroot_string"])
def test_unbalanced_quotes_rejected(tmp_path: Path, method: str) -> None:
    runner = Mock(spec=CommandRunner)
    stage = CommandStage(Settings(Path("/dev/vdb"), tmp_path, None))
    with pytest.raises(ValueError, match="closing quotation"):
        getattr(stage, method)(
            BuildContext(tmp_path, runner=runner), "printf 'unfinished"
        )
    runner.run.assert_not_called()


@pytest.mark.parametrize("chroot", [False, True])
def test_variadic_arguments_are_literal(tmp_path: Path, chroot: bool) -> None:
    runner = Mock(spec=CommandRunner)
    context = BuildContext(tmp_path / "image root", runner=runner)
    stage = CommandStage(Settings(Path("/dev/vdb"), tmp_path, None))
    argument = "spaces 'quotes' $(touch unwanted); *"
    if chroot:
        stage.chroot(context, "printf", "%s", argument, input=b"input")
    else:
        stage.command(context, "printf", "%s", argument, CUSTOM="value")
    prefix = ("chroot", str(context.root)) if chroot else ()
    assert runner.run.call_args.args[0] == (*prefix, "printf", "%s", argument)
    if chroot:
        assert runner.run.call_args.kwargs["input"] == b"input"
    else:
        assert runner.run.call_args.kwargs["env"]["CUSTOM"] == "value"


@pytest.mark.parametrize(
    "disk", ["/dev/vdb", "/dev/sda", "/dev/nvme0n1", "/dev/mmcblk0"]
)
def test_direct_disk_names(disk: str) -> None:
    from corvus_image_builder.gentoo._validation import validate_disk

    validate_disk(Path(disk))


@pytest.mark.parametrize(
    "disk",
    ["/dev/bad name", "/dev/sda;echo", "/dev/disk/by-id/name", "sda", "/dev/-bad"],
)
def test_invalid_disk_rejected_before_commands(tmp_path: Path, disk: str) -> None:
    from corvus_image_builder.gentoo.disk import DiskStage

    runner = Mock(spec=CommandRunner)
    stage = DiskStage(Settings(Path(disk), tmp_path, None))
    with pytest.raises(ValueError, match="Invalid target disk"):
        stage.validate(BuildContext(tmp_path, runner=runner))
    runner.run.assert_not_called()


@pytest.mark.parametrize("kernel", ["", "../kernel", "6.12 bad", "6.12;echo", "-6.12"])
def test_invalid_kernel_identifier(kernel: str) -> None:
    from corvus_image_builder.gentoo._validation import validate_kernel

    with pytest.raises(ValueError, match="Invalid kernel"):
        validate_kernel(kernel)


def test_kernel_identifier() -> None:
    from corvus_image_builder.gentoo._validation import validate_kernel

    validate_kernel("6.12.58-gentoo-r1+")
