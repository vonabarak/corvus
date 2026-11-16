"""Regression coverage for the project-specific string formatter."""

import ast
import importlib.util
from pathlib import Path

import pytest

SCRIPT = Path(__file__).resolve().parents[2] / "scripts/python_style.py"
spec = importlib.util.spec_from_file_location("python_style", SCRIPT)
assert spec is not None and spec.loader is not None
style = importlib.util.module_from_spec(spec)
spec.loader.exec_module(style)


@pytest.mark.parametrize(
    "expression",
    [
        repr(
            "first\n"
            "second\n"
        ),
        repr(
            "first\n"
            "\n"
        ),
        repr(
            b'first\n'
            b'second\n'
        ),
        'r"""first' + chr(10) + 'second"""',
        'f"""first {name!r}' + chr(10) + 'second {{literal}}"""',
        'f"""héllo {name}' + chr(10) + 'second"""',
        'f"""{ "yes" if name else "no" }' + chr(10) + 'second"""',
        '("first\\nsecond" f"{name}")',
    ],
)
def test_normalize_preserves_ast_and_is_idempotent(expression: str) -> None:
    source = "value = " + expression + chr(10)
    result = style.normalize(source)
    assert ast.dump(ast.parse(result)) == ast.dump(ast.parse(source))
    assert style.normalize(result) == result
    assert result != source


def test_docstrings_and_single_trailing_newlines_are_exempt() -> None:
    source = chr(10).join(
        [
            '"""Module documentation.',
            'Second line."""',
            "class Example:",
            '    """Class documentation.',
            '    Second line."""',
            "    def method(self):",
            '        """Method documentation.',
            '        Second line."""',
            '        return "one\\n"',
            "",
        ]
    )
    assert style.normalize(source) == source


def test_comment_in_canonical_concatenation_is_preserved() -> None:
    source = chr(10).join(
        ["value = (", '    "first\\n"  # keep this', '    "second"', ")", ""]
    )
    assert style.normalize(source) == source


def test_expression_in_conditional_is_not_a_docstring() -> None:
    source = 'if True: "one\\ntwo"' + chr(10)
    assert style.normalize(source) != source


@pytest.mark.parametrize("value", [(
    "one\n"
    "two\n"
), (
    "one\n"
    "option=\"yes\"\n"
)])
def test_wrapper_check_is_read_only_and_format_is_stable(
    tmp_path: Path, value: str
) -> None:
    import subprocess
    import sys

    path = tmp_path / "example.py"
    source = "value = " + repr(value) + chr(10)
    path.write_text(source)
    command = [sys.executable, str(SCRIPT)]
    result = subprocess.run([*command, "--check", str(path)], capture_output=True)
    assert result.returncode == 1
    assert b"CML001" in result.stdout
    assert path.read_text() == source
    subprocess.run([*command, str(path)], check=True, capture_output=True)
    formatted = path.read_text()
    assert formatted != source
    subprocess.run([*command, "--check", str(path)], check=True, capture_output=True)
    assert path.read_text() == formatted


def test_nested_continuation_indentation() -> None:
    source = chr(10).join(["values = [", '    call("one\\ntwo"),', "]", ""])
    result = style.normalize(source)
    assert '        "one\\n"' in result.splitlines()
    assert '        "two"' in result.splitlines()
