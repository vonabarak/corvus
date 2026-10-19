"""Install the real AlmaLinux desktop recipe, then boot its captured disk twice.

Prepare only the official media with `make image-ensure IMAGE=almalinux-10-media`.
The test must perform its own fresh installation, not reuse a prebuilt desktop.
"""

from __future__ import annotations

import secrets
import shlex
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

import pytest
from corvus_client._sync.vm import SyncVm
from corvus_client.types import BuildPipelineEnd
from corvus_test_harness import SingleNodeCase
from corvus_test_harness.host_binary import REPO_ROOT
from corvus_test_harness.outer import Crv

import yaml

pytestmark = [pytest.mark.slow, pytest.mark.timeout(5400)]

_RECIPE = REPO_ROOT / "yaml/almalinux-10-desktop"

_DESKTOP_PROBE = r"""
set -eu
. /etc/os-release
test "$ID" = almalinux
test "${VERSION_ID%%.*}" = 10
test -f /var/lib/corvus/installer-complete
! rpm -q cloud-init
systemctl is-active qemu-guest-agent gdm
test "$(systemctl get-default)" = graphical.target
pgrep -u corvus -x gnome-shell
graphical_session=false
for session in $(loginctl list-sessions --no-legend | awk '$3 == "corvus" {print $1}'); do
    case $(loginctl show-session "$session" -p Type --value) in
        wayland|x11)
            test "$(loginctl show-session "$session" -p Active --value)" = yes
            graphical_session=true ;;
    esac
done
$graphical_session
ls -d /sys/bus/virtio/drivers/virtio_gpu/virtio*
ls /sys/class/drm/card*/dev
ls /sys/bus/virtio/drivers/virtio_snd/virtio*/sound/card*/id
cat /proc/asound/cards
export XDG_RUNTIME_DIR=/run/user/$(id -u)
wpctl inspect @DEFAULT_AUDIO_SINK@
pw-play /usr/share/sounds/alsa/Front_Center.wav
"""


def _create_answer_media(directory: Path) -> Path:
    """Build a temporary Kickstart ISO on the test host for recipe upload."""
    answer_dir = directory / "answer-media"
    answer_dir.mkdir()
    shutil.copyfile(_RECIPE / "ks.cfg", answer_dir / "ks.cfg")
    answer_iso = directory / "kickstart.iso"
    iso_tool = shutil.which("mkisofs") or shutil.which("genisoimage")
    assert iso_tool, "mkisofs or genisoimage is required for answer media"
    subprocess.run(
        [iso_tool, "-R", "-J", "-V", "OEMDRV", "-o", str(answer_iso), str(answer_dir)],
        check=True,
        capture_output=True,
    )
    return answer_iso


class TestBuildAlmaLinuxInstaller(SingleNodeCase):
    @pytest.fixture(scope="class", autouse=True)
    @classmethod
    def _almalinux_media_present(cls, crv: Crv) -> None:
        # Read-only precondition: pytest workers never publish shared fixtures.
        names = [disk["name"] for disk in crv.disk_list()]
        assert "almalinux-10-dvd-iso" in names, (
            "Run `make image-ensure IMAGE=almalinux-10-media` before this test"
        )

    def test_unattended_desktop_installation(self) -> None:
        images = self.register_base_images()
        assert "almalinux-10-dvd-iso" in images
        prefix = f"almalinux-it-{secrets.token_hex(4)}"
        network = self.client.networks.create(
            prefix, "192.168.184.0/24", dhcp=True, nat=False
        )
        network.start()
        doc = yaml.safe_load((_RECIPE / "almalinux-10-desktop.yml").read_text())
        # Isolate recipe names so concurrent test runs can upload and build
        # without replacing one another's disks and templates.
        names = {
            name: f"{prefix}-{suffix}"
            for name, suffix in (
                ("almalinux-10-kickstart", "kickstart"),
                ("almalinux-10-desktop-installer", "installer"),
                ("almalinux-10-desktop-base", "base"),
                ("almalinux-10-desktop", "desktop"),
            )
        }
        names["almalinux-10-dvd-iso"] = images["almalinux-10-dvd-iso"]
        names["corvus"] = prefix

        def isolate(value: object) -> object:
            if isinstance(value, str):
                return names.get(value, value)
            if isinstance(value, list):
                return [isolate(item) for item in value]
            if isinstance(value, dict):
                return {key: isolate(item) for key, item in value.items()}
            return value

        doc = isolate(doc)
        assert isinstance(doc, dict)
        steps = doc["pipeline"]
        assert isinstance(steps, list)
        upload = steps[0]["upload"]
        assert isinstance(upload, dict)
        vm: SyncVm | None = None
        try:
            end: BuildPipelineEnd | None = None
            with tempfile.TemporaryDirectory(prefix="corvus-almalinux-") as temp_dir:
                answer_iso = _create_answer_media(Path(temp_dir))
                upload["from"] = str(answer_iso)
                pipeline_path = Path(temp_dir) / "almalinux-10-desktop.yml"
                pipeline_path.write_text(yaml.safe_dump(doc), encoding="utf-8")
                for event in self.client.build_stream(str(pipeline_path)):
                    print(event, flush=True)
                    if isinstance(event, BuildPipelineEnd):
                        end = event
            assert end is not None, "Pipeline did not report completion"
            assert not [build for build in end.builds if build.error_message], end
            artifact = next(
                build
                for build in end.builds
                if build.name == names["almalinux-10-desktop-base"]
            )
            assert artifact.artifact_disk_id is not None
            placement = (
                self.client.disks.get(artifact.artifact_disk_id).show().placements
            )
            assert len(placement) == 1
            assert placement[0].node.name == self.node.short_name
            assert placement[0].file_path.startswith(
                "/home/corvus/VMs/BaseImages/AlmaLinux10/"
            )

            assert not [
                item.name
                for item in self.client.vms.list()
                if item.name.startswith("__build_") and prefix in item.name
            ]
            template = self.client.templates.get(names["almalinux-10-desktop"])
            details = template.show()
            assert details.guest_agent and not details.cloud_init
            assert len(details.drives) == 1, "Runtime must not retain installer media"
            vm = template.instantiate(f"{prefix}-verify")
            for _ in range(2):
                vm.start(wait=True)
                self._assert_desktop(vm)
                vm.stop(wait=True)
        except BaseException:
            # Preserve the bake/runtime and topology on failure for SPICE access.
            for unit in ("corvus-nodeagent", "corvus"):
                result = self.node.run(
                    f"sudo journalctl -u {unit} --no-pager -n 150", check=False
                )
                print(result.stdout.decode(errors="replace"), flush=True)
            raise
        else:
            # Successful resources belong to the inner test daemon only.
            assert vm is not None
            vm.delete()
            for template_name in (
                names["almalinux-10-desktop"],
                names["almalinux-10-desktop-installer"],
            ):
                self.client.templates.get(template_name).delete()
            for disk in reversed(self.client.disks.list()):
                if disk.name.startswith(prefix):
                    self.client.disks.get(disk.id).delete()
            network.stop()
            network.delete()

    def _assert_desktop(self, vm: SyncVm) -> None:
        # QGA becomes ready before GDM and the user's PipeWire session.
        deadline = time.monotonic() + 180
        last_output = ""
        while True:
            result = vm.guest_exec(
                f"runuser -u corvus -- /bin/sh -c {shlex.quote(_DESKTOP_PROBE)}"
            )
            if result.exit_code == 0:
                return
            last_output = (
                f"exit={result.exit_code}\n"
                f"stdout={result.stdout}\n"
                f"stderr={result.stderr}"
            )
            assert time.monotonic() < deadline, f"Desktop not ready: {last_output}"
            time.sleep(2)
