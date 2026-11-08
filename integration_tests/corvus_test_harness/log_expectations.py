"""Validated log baselines and streaming, exact-count comparisons."""

from __future__ import annotations

import os
import re
import tempfile
from dataclasses import dataclass
from pathlib import Path

import yaml

from .logs import SERVICES

# Only the logger prefix of a journal record is a severity marker. Quoted
# messages, SQL parameters, and continuation lines must not inflate totals.
_SEVERITY = re.compile(r"^\S+\s+\S+\s+[^:]+:\s+\[(Warn|Error)(?:#[^\]]+)?\]")


class _UniqueLoader(yaml.SafeLoader):
    pass


def _mapping(loader: yaml.SafeLoader, node: yaml.MappingNode) -> dict[str, object]:
    result: dict[str, object] = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if not isinstance(key, str):
            raise ValueError("all baseline mapping keys must be strings")
        if key in result:
            raise ValueError(f"duplicate baseline key: {key}")
        result[key] = loader.construct_object(value_node)
    return result


_UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _mapping)


def _object(value: object, label: str) -> dict[str, object]:
    if not isinstance(value, dict) or any(not isinstance(key, str) for key in value):
        raise ValueError(f"{label} must be a mapping with string keys")
    return dict(value)


def _keys(value: dict[str, object], required: set[str], optional: set[str]) -> None:
    if not required <= value.keys() or value.keys() - required - optional:
        raise ValueError(
            f"expected keys {sorted(required)}; optional {sorted(optional)}"
        )


def _count(value: object) -> int:
    if type(value) is not int or value < 0:
        raise ValueError("counts must be nonnegative integers")
    return value


@dataclass(frozen=True)
class Pattern:
    regex: str
    count: int


@dataclass(frozen=True)
class Expected:
    warnings: int
    errors: int
    patterns: tuple[Pattern, ...] = ()

    def as_dict(self) -> dict[str, object]:
        result: dict[str, object] = {"warnings": self.warnings, "errors": self.errors}
        if self.patterns:
            result["patterns"] = [
                {"regex": rule.regex, "count": rule.count} for rule in self.patterns
            ]
        return result


ClassLogs = dict[str, dict[str, Expected]]
Baseline = dict[str, ClassLogs]


def load_baseline(path: Path) -> Baseline:
    raw: object = yaml.load(path.read_text(), Loader=_UniqueLoader)
    root = _object(raw, "baseline")
    _keys(root, {"version", "classes"}, set())
    if type(root["version"]) is not int or root["version"] != 1:
        raise ValueError("unsupported log baseline version (expected 1)")
    result: Baseline = {}
    for class_name, class_value in _object(root["classes"], "classes").items():
        entry = _object(class_value, class_name)
        _keys(entry, {"nodes"}, set())
        nodes: ClassLogs = {}
        for node, files_value in _object(entry["nodes"], "nodes").items():
            files: dict[str, Expected] = {}
            for filename, counts_value in _object(files_value, node).items():
                if filename not in {f"{service}.log" for service in SERVICES}:
                    raise ValueError(f"unknown service log: {filename}")
                counts = _object(counts_value, filename)
                _keys(counts, {"warnings", "errors"}, {"patterns"})
                rules = counts.get("patterns", [])
                if not isinstance(rules, list):
                    raise ValueError("patterns must be a list")
                patterns: list[Pattern] = []
                for rule in rules:
                    pattern = _object(rule, "pattern")
                    _keys(pattern, {"regex", "count"}, set())
                    regex = pattern["regex"]
                    if not isinstance(regex, str):
                        raise ValueError("regex must be a string")
                    re.compile(regex)
                    patterns.append(Pattern(regex, _count(pattern["count"])))
                files[filename] = Expected(
                    _count(counts["warnings"]),
                    _count(counts["errors"]),
                    tuple(patterns),
                )
            nodes[node] = files
        result[class_name] = nodes
    return result


def save_baseline(path: Path, baseline: Baseline) -> None:
    classes = {
        name: {
            "nodes": {
                node: {filename: logs[filename].as_dict() for filename in sorted(logs)}
                for node, logs in sorted(nodes.items())
            }
        }
        for name, nodes in sorted(baseline.items())
    }
    text = yaml.safe_dump({"version": 1, "classes": classes}, sort_keys=False)
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    try:
        with os.fdopen(descriptor, "w") as output:
            output.write(text)
        os.replace(temporary, path)
    finally:
        Path(temporary).unlink(missing_ok=True)


def count_log(path: Path, patterns: tuple[Pattern, ...] = ()) -> Expected:
    warnings = errors = 0
    compiled = [re.compile(pattern.regex) for pattern in patterns]
    matches = [0] * len(patterns)
    with path.open(errors="replace") as log:
        for line in log:
            severity = _SEVERITY.match(line)
            if severity:
                warnings += severity[1] == "Warn"
                errors += severity[1] == "Error"
            for index, regex in enumerate(compiled):
                if regex.search(line.rstrip("\r\n")) is not None:
                    matches[index] += 1
    return Expected(
        warnings,
        errors,
        tuple(
            Pattern(rule.regex, count)
            for rule, count in zip(patterns, matches, strict=True)
        ),
    )


def differences(expected: Expected, actual: Expected) -> list[str]:
    metrics = [
        ("warnings", expected.warnings, actual.warnings),
        ("errors", expected.errors, actual.errors),
    ] + [
        (f"regex {rule.regex!r}", rule.count, observed.count)
        for rule, observed in zip(expected.patterns, actual.patterns, strict=True)
    ]
    return [
        f"{label}: expected {wanted}, actual {observed} ({observed - wanted:+d})"
        for label, wanted, observed in metrics
        if wanted != observed
    ]
