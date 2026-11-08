"""Exact service-log counts, reviewed updates, and controller-only pytest hooks."""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from corvus_test_harness.host_binary import REPO_ROOT
from corvus_test_harness.log_expectations import (
    Expected,
    Pattern,
    count_log,
    differences,
    load_baseline,
    save_baseline,
)

_PREFIX = "2026-11-08T10:00:00.000000+00:00 host corvus[42]: "


def test_severity_and_regex_count_physical_lines(tmp_path: Path) -> None:
    path = tmp_path / "corvus.log"
    path.write_text(
        _PREFIX
        + "[Warn] expected failure failure\n"
        + _PREFIX
        + "[Warn#qga] warning\n"
        + _PREFIX
        + "[Error] error\n"
        + _PREFIX
        + "[Error#rpc] error\n"
        + _PREFIX
        + "[Info] quoted [Warn] and [Error]\n"
        + "continuation [Warn] failure\n"
        + _PREFIX
        + "systemd error without a level\n"
    )
    assert count_log(
        path, (Pattern("failure", 0), Pattern("^continuation", 0), Pattern("absent", 1))
    ) == Expected(
        2, 2, (Pattern("failure", 2), Pattern("^continuation", 1), Pattern("absent", 0))
    )
    path.write_bytes(b"")
    assert count_log(path) == Expected(0, 0)


def test_differences_include_decreases_and_overlapping_regexes(tmp_path: Path) -> None:
    path = tmp_path / "corvus.log"
    path.write_text(_PREFIX + "[Debug] hello hello\n")
    wanted = Expected(2, 0, (Pattern("hello", 0), Pattern("hello hello", 2)))
    actual = count_log(path, wanted.patterns)
    assert differences(wanted, actual) == [
        "warnings: expected 2, actual 0 (-2)",
        "regex 'hello': expected 0, actual 1 (+1)",
        "regex 'hello hello': expected 2, actual 1 (-1)",
    ]


@pytest.mark.parametrize(
    "text",
    [
        "version: 2\nclasses: {}",
        "version: true\nclasses: {}",
        "version: 1\nversion: 1\nclasses: {}",
        "version: 1\nclasses: []",
        "version: 1\nclasses: {}\nextra: true",
        "version: 1\nclasses: {Test: {nodes: {single: {other.log: {warnings: 0, errors: 0}}}}}",
        "version: 1\nclasses: {Test: {nodes: {single: {corvus.log: {warnings: -1, errors: 0}}}}}",
        "version: 1\nclasses: {Test: {nodes: {single: {corvus.log: {warnings: true, errors: 0}}}}}",
        "version: 1\nclasses: {Test: {nodes: {single: {corvus.log: {warnings: 0}}}}}",
        "version: 1\nclasses: {Test: {nodes: {single: {corvus.log: {warnings: 0, errors: 0, patterns: [{regex: '[', count: 0}]}}}}}}",
    ],
)
def test_invalid_baseline_is_rejected(tmp_path: Path, text: str) -> None:
    import re

    import yaml

    path = tmp_path / "expectations.yaml"
    path.write_text(text)
    with pytest.raises((ValueError, yaml.YAMLError, re.error)):
        load_baseline(path)


def test_atomic_roundtrip_is_deterministic(tmp_path: Path) -> None:
    path = tmp_path / "expectations.yaml"
    baseline = {
        "TestZ": {"single": {"corvus.log": Expected(1, 0, (Pattern("failure", 1),))}}
    }
    save_baseline(path, baseline)
    assert load_baseline(path) == baseline
    before = path.read_bytes()
    save_baseline(path, baseline)
    assert path.read_bytes() == before
    assert list(tmp_path.iterdir()) == [path]


@pytest.fixture
def project(tmp_path: Path) -> Path:
    shutil.copyfile(
        REPO_ROOT / "integration_tests/conftest.py", tmp_path / "conftest.py"
    )
    (tmp_path / "records.txt").write_text(_PREFIX + "[Warn] expected failure\n")
    (tmp_path / "test_fake.py").write_text(
        "from pathlib import Path\n"
        "import pytest\n"
        "from corvus_test_harness import logs\n"
        "ROOT = Path(__file__).parent\n"
        "@pytest.fixture(scope='class', autouse=True)\n"
        "def export(request):\n"
        "    name = request.cls.__name__\n"
        "    outer = f'corvus-it-{name}-12345678-single'\n"
        "    logs.register_node(name, outer, 'single')\n"
        "    yield\n"
        "    directory = logs.run_directory() / name / outer\n"
        "    directory.mkdir(parents=True, exist_ok=True)\n"
        "    for service in logs.SERVICES:\n"
        "        text = (ROOT / 'records.txt').read_text() if service == 'corvus' else ''\n"
        "        (directory / f'{service}.log').write_text(text)\n"
        "    if (ROOT / 'partial').exists():\n"
        "        (directory / 'corvus.collection-error.txt').write_text('partial')\n"
        "    if (ROOT / 'missing').exists():\n"
        "        (directory / 'corvus-netd.log').unlink()\n"
        "class TestLogs:\n"
        "    def test_first(self):\n"
        "        if (ROOT / 'skip').exists(): pytest.skip('intentional')\n"
        "        assert not (ROOT / 'fail').exists()\n"
        "    def test_second(self): pass\n"
    )
    return tmp_path


def run_project(project: Path, *args: str) -> subprocess.CompletedProcess[str]:
    environment = dict(os.environ, CORVUS_TEST_LOG_DIR=str(project / "logs"))
    targets = (
        []
        if any(arg.startswith(str(project)) and "::" in arg for arg in args)
        else [str(project)]
    )
    return subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "-c",
            str(REPO_ROOT / "pyproject.toml"),
            *targets,
            "--log-expectations",
            str(project / "baseline.yaml"),
            "-q",
            *args,
        ],
        capture_output=True,
        text=True,
        timeout=90,
        env=environment,
    )


def test_new_classes_are_saved_once_by_xdist_controller(project: Path) -> None:
    source = project / "test_fake.py"
    source.write_text(source.read_text() + "\nclass TestOther(TestLogs): pass\n")
    result = run_project(project, "-n", "2")
    assert result.returncode == 0, result.stdout + result.stderr
    assert result.stdout.count("ADDED new test class TestLogs ") == 1
    assert result.stdout.count("ADDED new test class TestOther ") == 1
    baseline = load_baseline(project / "baseline.yaml")
    assert set(baseline) == {"TestLogs", "TestOther"}
    assert baseline["TestLogs"]["single"]["corvus.log"] == Expected(1, 0)
    assert baseline["TestOther"]["single"]["corvus-netd.log"] == Expected(0, 0)


def test_mismatch_warns_without_changing_baseline_and_update_preserves_rules(
    project: Path,
) -> None:
    path = project / "baseline.yaml"
    baseline = {
        "TestLogs": {
            "single": {
                "corvus.log": Expected(0, 1, (Pattern("failure", 0),)),
                "corvus-nodeagent.log": Expected(0, 0),
                "corvus-netd.log": Expected(0, 0),
            }
        },
        "TestUnselected": {"unused": {"corvus.log": Expected(9, 9)}},
    }
    save_baseline(path, baseline)
    before = path.read_bytes()
    result = run_project(project)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "warnings: expected 0, actual 1 (+1)" in result.stdout
    assert "errors: expected 1, actual 0 (-1)" in result.stdout
    assert "regex 'failure': expected 0, actual 1 (+1)" in result.stdout
    assert path.read_bytes() == before
    result = run_project(project, "--update-log-expectations")
    assert result.returncode == 0, result.stdout + result.stderr
    updated = load_baseline(path)
    assert updated["TestLogs"]["single"]["corvus.log"] == Expected(
        1, 0, (Pattern("failure", 1),)
    )
    assert updated["TestUnselected"] == baseline["TestUnselected"]


@pytest.mark.parametrize(
    "args", [("-k", "first"), ("--deselect", "test_fake.py::TestLogs::test_second")]
)
def test_filtered_class_is_not_compared_or_saved(
    project: Path, args: tuple[str, ...]
) -> None:
    result = run_project(project, *args)
    assert result.returncode == 0, result.stdout + result.stderr
    assert "only part of the class was selected" in result.stdout
    assert not (project / "baseline.yaml").exists()


@pytest.mark.parametrize("marker", ["skip", "fail", "partial", "missing"])
def test_incomplete_class_never_initializes_baseline(
    project: Path, marker: str
) -> None:
    (project / marker).touch()
    result = run_project(project)
    assert result.returncode == (1 if marker == "fail" else 0), (
        result.stdout + result.stderr
    )
    assert "SKIPPED TestLogs" in result.stdout
    assert not (project / "baseline.yaml").exists()


def test_invalid_config_warns_without_being_overwritten(project: Path) -> None:
    path = project / "baseline.yaml"
    path.write_text("version: 17\nclasses: {}\n")
    before = path.read_bytes()
    result = run_project(project, "--update-log-expectations")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "WARNING invalid log baseline" in result.stdout
    assert path.read_bytes() == before


def test_selecting_one_method_is_not_a_complete_class(project: Path) -> None:
    result = run_project(
        project, str(project / "test_fake.py") + "::TestLogs::test_first"
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert "only part of the class was selected" in result.stdout
    assert not (project / "baseline.yaml").exists()


def test_class_name_filter_can_initialize_a_complete_class(project: Path) -> None:
    result = run_project(project, "-k", "TestLogs")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "ADDED new test class TestLogs " in result.stdout


def test_unknown_logs_in_existing_class_require_explicit_update(project: Path) -> None:
    path = project / "baseline.yaml"
    save_baseline(path, {"TestLogs": {"single": {"corvus.log": Expected(1, 0)}}})
    before = path.read_bytes()
    result = run_project(project, "-n", "2")
    assert result.returncode == 0, result.stdout + result.stderr
    assert (
        "TestLogs/single/corvus-netd.log: new or missing service log" in result.stdout
    )
    assert path.read_bytes() == before


def test_failed_class_does_not_block_adding_another_successful_class(
    project: Path,
) -> None:
    source = project / "test_fake.py"
    source.write_text(
        source.read_text()
        + "\nclass TestBroken:\n    def test_fail(self): assert False\n"
    )
    result = run_project(project, "-n", "2")
    assert result.returncode == 1, result.stdout + result.stderr
    assert "ADDED new test class TestLogs " in result.stdout
    assert set(load_baseline(project / "baseline.yaml")) == {"TestLogs"}


def test_interruption_never_initializes_baseline(project: Path) -> None:
    source = project / "test_fake.py"
    source.write_text(
        source.read_text().replace(
            "def test_second(self): pass",
            "def test_second(self): raise KeyboardInterrupt",
        )
    )
    result = run_project(project)
    assert result.returncode == 2, result.stdout + result.stderr
    assert "session interrupted or worker crashed" in result.stdout
    assert not (project / "baseline.yaml").exists()


def test_failed_atomic_write_preserves_previous_file(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    path = tmp_path / "expectations.yaml"
    save_baseline(path, {})
    before = path.read_bytes()

    def fail_replace(source: str, destination: Path) -> None:
        raise PermissionError("cannot replace baseline")

    monkeypatch.setattr(os, "replace", fail_replace)
    with pytest.raises(PermissionError):
        save_baseline(path, {"TestLogs": {}})
    assert path.read_bytes() == before
    assert list(tmp_path.iterdir()) == [path]


def test_worker_crash_never_initializes_baseline(project: Path) -> None:
    source = project / "test_fake.py"
    source.write_text(
        source.read_text().replace(
            "def test_second(self): pass",
            "def test_second(self):\n        import os\n        os._exit(3)",
        )
    )
    result = run_project(project, "-n", "2", "--max-worker-restart=0")
    assert result.returncode != 0, result.stdout + result.stderr
    assert "session interrupted or worker crashed; baseline unchanged" in result.stdout
    assert not (project / "baseline.yaml").exists()
