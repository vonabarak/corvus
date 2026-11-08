"""Controller-owned pytest reporting and updates for service log baselines."""

from __future__ import annotations

import re
from collections.abc import Iterator
from pathlib import Path
from typing import TYPE_CHECKING, TypedDict, cast

import pytest

import yaml

from . import logs
from .cases import IntegrationTestCase
from .log_expectations import (
    Baseline,
    ClassLogs,
    count_log,
    differences,
    load_baseline,
    save_baseline,
    variable_differences,
)

if TYPE_CHECKING:
    from _pytest.terminal import TerminalReporter

    from .xdist_types import WorkerController

_KEY = "corvus_log_expectations"


class _ClassInfo(TypedDict):
    name: str
    items: list[str]
    selected: list[str]
    managed: bool
    restricted: bool


class _Snapshot(TypedDict):
    classes: dict[str, _ClassInfo]
    nodes: dict[str, dict[str, str]]


class LogExpectationsPlugin:
    def __init__(self, config: pytest.Config) -> None:
        self.config = config
        self.worker = hasattr(config, "workerinput")
        self.path = Path(config.getoption("log_expectations")).expanduser().resolve()
        self.update = bool(config.getoption("update_log_expectations"))
        self.classes: dict[str, _ClassInfo] = {}
        self.snapshots: list[_Snapshot] = []
        self.reports: dict[str, dict[str, bool]] = {}
        self.messages: list[str] = []
        self.crashed = False
        self.baseline: Baseline = {}
        self.valid = True
        if not self.worker:
            try:
                self.baseline = load_baseline(self.path)
            except FileNotFoundError:
                pass  # A first run can initialize a missing baseline.
            except (OSError, ValueError, yaml.YAMLError, re.error) as error:
                self.valid = False
                self.messages.append(
                    f"WARNING invalid log baseline {self.path}: {error}"
                )

    @pytest.hookimpl(hookwrapper=True, tryfirst=True)
    def pytest_collection_modifyitems(self, items: list[pytest.Item]) -> Iterator[None]:
        # Enter before selection plugins mutate items, and leave after them.
        for item in items:
            if not isinstance(item, pytest.Function) or item.cls is None:
                continue
            parent = item.getparent(pytest.Class)
            if parent is None:
                continue
            cls = item.cls
            info = self.classes.setdefault(
                parent.nodeid,
                {
                    "name": cls.__name__,
                    "items": [],
                    "selected": [],
                    "managed": issubclass(cls, IntegrationTestCase),
                    # Method node IDs restrict collection before modifyitems.
                    "restricted": any(
                        arg.count("::") >= 2 and arg.split("::")[1] == cls.__name__
                        for arg in self.config.args
                    ),
                },
            )
            info["items"].append(item.nodeid)
        yield
        selected = {item.nodeid for item in items}
        for info in self.classes.values():
            info["selected"] = [
                nodeid for nodeid in info["items"] if nodeid in selected
            ]

    def pytest_runtest_logreport(self, report: pytest.TestReport) -> None:
        if not self.worker:
            self.reports.setdefault(report.nodeid, {})[report.when] = (
                report.passed and not hasattr(report, "wasxfail")
            )

    def pytest_testnodedown(self, node: WorkerController, error: object) -> None:
        if error is not None:
            self.crashed = True
        output = cast(dict[str, object], getattr(node, "workeroutput", {}))
        snapshot = output.get(_KEY)
        if isinstance(snapshot, dict):
            # This is our own serializable payload, transported by xdist.
            self.snapshots.append(cast(_Snapshot, snapshot))

    def pytest_sessionfinish(self, exitstatus: int) -> None:
        if self.worker:
            output = cast(dict[str, object], getattr(self.config, "workeroutput", {}))
            output[_KEY] = {"classes": self.classes, "nodes": logs.node_names()}
            return
        if not self.snapshots:
            self.snapshots.append({"classes": self.classes, "nodes": logs.node_names()})
        if not self.valid:
            return
        interrupted = exitstatus in {
            pytest.ExitCode.INTERRUPTED,
            pytest.ExitCode.INTERNAL_ERROR,
        }
        self._compare(interrupted or self.crashed)

    def _inventory(
        self,
    ) -> tuple[dict[str, list[_ClassInfo]], dict[str, dict[str, str]]]:
        classes: dict[str, dict[str, _ClassInfo]] = {}
        nodes: dict[str, dict[str, str]] = {}
        for snapshot in self.snapshots:
            for identity, info in snapshot["classes"].items():
                classes.setdefault(info["name"], {})[identity] = info
            for name, registered in snapshot["nodes"].items():
                nodes.setdefault(name, {}).update(registered)
        return {name: list(infos.values()) for name, infos in classes.items()}, nodes

    def _skip_reason(self, infos: list[_ClassInfo]) -> str | None:
        if len(infos) != 1:
            return "ambiguous or unavailable test-class identity"
        info = infos[0]
        if info["restricted"] or set(info["selected"]) != set(info["items"]):
            return "only part of the class was selected"
        if not info["selected"] or any(
            self.reports.get(nodeid) != {"setup": True, "call": True, "teardown": True}
            for nodeid in info["selected"]
        ):
            return "tests failed, skipped, or did not complete"
        return None

    def _observe(self, name: str, nodes: dict[str, str]) -> ClassLogs | None:
        observed: ClassLogs = {}
        valid = True
        if not nodes:
            self.messages.append(f"WARNING {name}: no node log exports were registered")
            return None
        for outer_name, short_name in sorted(nodes.items()):
            directory = logs.run_directory() / name / outer_name
            if short_name in observed:
                self.messages.append(
                    f"WARNING {name}/{short_name}: duplicate node identity"
                )
                valid = False
                continue
            observed[short_name] = {}
            diagnostics = sorted(directory.glob("*collection-error.txt"))
            if diagnostics:
                self.messages.append(
                    f"WARNING {name}/{short_name}: incomplete log collection: "
                    + ", ".join(str(path) for path in diagnostics)
                )
                valid = False
            for service in logs.SERVICES:
                filename = f"{service}.log"
                path = directory / filename
                expected = self.baseline.get(name, {}).get(short_name, {}).get(filename)
                try:
                    observed[short_name][filename] = count_log(
                        path,
                        expected.patterns if expected else (),
                        expected.variable_warnings if expected else (),
                    )
                except OSError as error:
                    self.messages.append(
                        f"WARNING {name}/{short_name}/{filename}: {error}"
                    )
                    valid = False
        return observed if valid else None

    def _compare_class(
        self, name: str, observed: ClassLogs, nodes: dict[str, str]
    ) -> None:
        expected = self.baseline[name]
        for node in sorted(expected.keys() | observed.keys()):
            if node not in expected or node not in observed:
                kind = (
                    "new node without expectations"
                    if node in observed
                    else "missing node"
                )
                self.messages.append(f"WARNING {name}/{node}: {kind}")
                continue
            for filename in sorted(expected[node].keys() | observed[node].keys()):
                label = f"{name}/{node}/{filename}"
                if filename not in expected[node] or filename not in observed[node]:
                    self.messages.append(f"WARNING {label}: new or missing service log")
                    continue
                outer = next(outer for outer, short in nodes.items() if short == node)
                path = logs.run_directory() / name / outer / filename
                for difference in differences(
                    expected[node][filename], observed[node][filename]
                ):
                    self.messages.append(f"WARNING {label}: {difference}; {path}")
                for difference in variable_differences(
                    expected[node][filename], observed[node][filename]
                ):
                    self.messages.append(f"INFO {label}: {difference}; {path}")

    def _compare(self, interrupted: bool) -> None:
        classes, registered = self._inventory()
        names = set(registered) | {
            name
            for name, infos in classes.items()
            if any(info["managed"] and info["selected"] for info in infos)
        }
        changes: list[tuple[str, bool, ClassLogs]] = []
        checked = 0
        if interrupted:
            self.messages.append(
                "WARNING session interrupted or worker crashed; baseline unchanged"
            )
        for name in sorted(names):
            infos = classes.get(name, [])
            reason = (
                "session interrupted or worker crashed"
                if interrupted
                else self._skip_reason(infos)
            )
            if reason:
                self.messages.append(
                    f"SKIPPED {name}: log comparison skipped: {reason}"
                )
                continue
            nodes = registered.get(name, {})
            observed = self._observe(name, nodes)
            if observed is None:
                self.messages.append(
                    f"SKIPPED {name}: incomplete log exports; baseline unchanged"
                )
                continue
            checked += 1
            new = name not in self.baseline
            if not new:
                self._compare_class(name, observed, nodes)
            if (new or self.update) and self.baseline.get(name) != observed:
                self.baseline[name] = observed
                changes.append((name, new, observed))
        if changes:
            try:
                save_baseline(self.path, self.baseline)
            except OSError as error:
                self.messages.append(
                    f"WARNING could not save log baseline {self.path}: {error}"
                )
            else:
                for name, new, observed in changes:
                    verb = "ADDED new test class" if new else "UPDATED"
                    self.messages.append(f"{verb} {name} in {self.path}")
                    self._describe(name, observed)
        if names:
            self.messages.append(
                f"Log expectations: compared {checked}/{len(names)} classes; {self.path}"
            )

    def _describe(self, name: str, observed: ClassLogs) -> None:
        for node, files in sorted(observed.items()):
            for filename, counts in sorted(files.items()):
                self.messages.append(
                    f"  {name}/{node}/{filename}: warnings={counts.warnings}, errors={counts.errors}"
                )
                for kind, rules in (
                    ("regex", counts.patterns),
                    ("variable warning regex", counts.variable_warnings),
                ):
                    for rule in rules:
                        self.messages.append(
                            f"    {kind} {rule.regex!r}: count={rule.count}"
                        )

    def pytest_terminal_summary(self, terminalreporter: TerminalReporter) -> None:
        if self.messages:
            terminalreporter.write_sep("-", "service log expectations")
            for message in self.messages:
                terminalreporter.write_line(
                    message, yellow=message.startswith("WARNING")
                )
