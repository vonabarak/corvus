"""Run Ruff followed by the project's multiline data-string normalization."""

import argparse
import ast
import codecs
import difflib
import json
import subprocess
import sys
from pathlib import Path

import libcst as cst


class StringStyle(cst.CSTTransformer):
    """Split multiline literal content while preserving docstrings and expressions."""

    def __init__(self, positions, lines) -> None:
        self.docstrings: set[int] = set()
        self.positions = positions
        self.lines = lines

    def indentation(self, node) -> str:
        position = self.positions[node]
        line = self.lines[position.start.line - 1]
        return line[: len(line) - len(line.lstrip())]

    def leave_SimpleString(
        self, original: cst.SimpleString, updated: cst.SimpleString
    ) -> cst.BaseExpression:
        if id(original) in self.docstrings:
            return updated
        value = original.evaluated_value
        newline = b"\n" if isinstance(value, bytes) else "\n"
        if newline not in value[:-1]:
            return updated
        parts = value.splitlines(keepends=True)
        return concatenate(
            [
                cst.SimpleString(
                    repr(part)
                    if isinstance(part, bytes)
                    else json.dumps(part, ensure_ascii=False)
                )
                for part in parts
            ],
            self.indentation(original),
        )

    def visit_ConcatenatedString(self, node: cst.ConcatenatedString) -> bool:
        return False

    def leave_ConcatenatedString(
        self, original: cst.ConcatenatedString, updated: cst.ConcatenatedString
    ) -> cst.BaseExpression:
        if id(original) in self.docstrings:
            return updated
        leaves: list[cst.BaseString] = []

        def collect(node: cst.BaseString) -> None:
            if isinstance(node, cst.ConcatenatedString):
                collect(node.left)
                collect(node.right)
            else:
                leaves.append(node)

        collect(updated)
        # Already line-oriented concatenations retain their whitespace and comments.
        if all(
            (
                self.leave_FormattedString(leaf, leaf)
                if isinstance(leaf, cst.FormattedString)
                else self.leave_SimpleString(leaf, leaf)
            )
            is leaf
            for leaf in leaves
        ):
            return updated

        class Comments(cst.CSTVisitor):
            found = False

            def visit_Comment(self, node: cst.Comment) -> None:
                self.found = True

        comments = Comments()
        updated.visit(comments)
        if comments.found:
            raise ValueError("Split multiline literals around comments manually")
        value = updated.evaluated_value
        if value is not None:
            literal = cst.SimpleString(repr(value))
            self.positions[literal] = self.positions[original]
            result = self.leave_SimpleString(literal, literal)
            return result if result is not literal else updated
        fragments: list[cst.BaseString] = []

        def flatten(node: cst.BaseString) -> None:
            if isinstance(node, cst.ConcatenatedString):
                flatten(node.left)
                flatten(node.right)
            else:
                result = (
                    self.leave_FormattedString(node, node)
                    if isinstance(node, cst.FormattedString)
                    else self.leave_SimpleString(node, node)
                )
                if isinstance(result, cst.ConcatenatedString):
                    flatten_result(result)
                else:
                    fragments.append(result.with_changes(lpar=[], rpar=[]))

        def flatten_result(node: cst.BaseString) -> None:
            if isinstance(node, cst.ConcatenatedString):
                flatten_result(node.left)
                flatten_result(node.right)
            else:
                fragments.append(node.with_changes(lpar=[], rpar=[]))

        flatten(updated)
        return concatenate(fragments, self.indentation(original))

    def leave_FormattedString(
        self, original: cst.FormattedString, updated: cst.FormattedString
    ) -> cst.BaseExpression:
        delimiter = updated.start[-1]
        if len(updated.start) > 2:
            expressions = " ".join(
                cst.Module([]).code_for_node(part)
                for part in updated.parts
                if isinstance(part, cst.FormattedStringExpression)
            )
            delimiter = "'" if '"' in expressions else '"'
        groups: list[list[cst.BaseFormattedStringContent]] = [[]]
        for part in updated.parts:
            if isinstance(part, cst.FormattedStringText):
                # Decode literal text only; never evaluate interpolation expressions.
                value = (
                    part.value
                    if "r" in updated.prefix.lower()
                    else codecs.decode(
                        part.value.encode("raw_unicode_escape"), "unicode_escape"
                    )
                )
                value = value.replace("{{", "{").replace("}}", "}")
                assert isinstance(value, str)
                for line in value.splitlines(keepends=True):
                    escaped = json.dumps(line, ensure_ascii=False)[1:-1]
                    if delimiter == "'":
                        escaped = escaped.replace('\\"', '"').replace("'", "\\'")
                    escaped = escaped.replace("{", "{{").replace("}", "}}")
                    groups[-1].append(cst.FormattedStringText(escaped))
                    if line.endswith("\n"):
                        groups.append([])
            else:
                groups[-1].append(part)
        if not groups[-1]:
            groups.pop()
        if len(groups) < 2:
            return updated
        return concatenate(
            [
                cst.FormattedString(parts=group, start="f" + delimiter, end=delimiter)
                for group in groups
            ],
            self.indentation(original),
        )


def concatenate(parts: list[cst.BaseString], indentation: str) -> cst.BaseExpression:
    """Place literal fragments on separate indented lines."""
    result = parts[-1]
    for part in reversed(parts[:-1]):
        result = cst.ConcatenatedString(
            part,
            result,
            whitespace_between=cst.ParenthesizedWhitespace(
                indent=False, last_line=cst.SimpleWhitespace(indentation + "    ")
            ),
        )
    return result.with_changes(
        lpar=[
            cst.LeftParen(
                whitespace_after=cst.ParenthesizedWhitespace(
                    indent=False, last_line=cst.SimpleWhitespace(indentation + "    ")
                )
            )
        ],
        rpar=[
            cst.RightParen(
                whitespace_before=cst.ParenthesizedWhitespace(
                    indent=False, last_line=cst.SimpleWhitespace(indentation)
                )
            )
        ],
    )


def normalize(source: str) -> str:
    """Normalize data literals, exempting conventional documentation strings."""
    wrapper = cst.MetadataWrapper(cst.parse_module(source))
    module = wrapper.module
    transformer = StringStyle(
        dict(wrapper.resolve(cst.metadata.PositionProvider)), source.splitlines()
    )

    class Docs(cst.CSTVisitor):
        def on_visit(self, node: cst.CSTNode) -> bool:
            if not isinstance(node, (cst.Module, cst.FunctionDef, cst.ClassDef)):
                return True
            body = node.body if isinstance(node, cst.Module) else node.body.body
            if not body:
                return True
            first = body[0]
            if isinstance(first, cst.SimpleStatementLine):
                first = first.body[0]
            if isinstance(first, cst.Expr) and isinstance(
                first.value, (cst.SimpleString, cst.ConcatenatedString)
            ):
                transformer.docstrings.add(id(first.value))
            return True

    module.visit(Docs())
    output = module.visit(transformer).code
    if ast.dump(ast.parse(source)) != ast.dump(ast.parse(output)):
        raise ValueError("String normalization changed the AST")
    return output


def main() -> int:
    """Format files or check the combined canonical representation without writes."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    parser.add_argument(
        "paths", nargs="*", default=["python", "integration_tests", "scripts", "yaml"]
    )
    args = parser.parse_args()
    ruff = [sys.executable, "-m", "ruff"]
    lint = subprocess.run(
        [*ruff, "check", *([] if args.check else ["--fix"]), *args.paths]
    )
    files = subprocess.check_output(
        [*ruff, "check", "--show-files", *args.paths], text=True
    ).splitlines()
    failed = lint.returncode != 0
    for filename in files:
        path = Path(filename)
        source = path.read_text()
        formatted = subprocess.check_output(
            [*ruff, "format", "--stdin-filename", filename, "-"],
            input=source,
            text=True,
            stderr=subprocess.DEVNULL,
        )
        try:
            normalized = normalize(formatted)
            output = subprocess.check_output(
                [
                    *ruff,
                    "check",
                    "--select",
                    "UP034,W293",
                    "--fix",
                    "--stdin-filename",
                    filename,
                    "-",
                ],
                input=normalized,
                text=True,
                stderr=subprocess.DEVNULL,
            )
        except Exception as error:
            print(f"{filename}:1:1: CML001 {error}", file=sys.stderr)
            failed = True
            continue
        if output != source:
            if args.check:
                print(f"{filename}:1:1: CML001 noncanonical formatting")
                print(
                    "".join(
                        difflib.unified_diff(
                            source.splitlines(True),
                            output.splitlines(True),
                            fromfile=filename,
                            tofile=filename,
                        )
                    ),
                    end="",
                )
                failed = True
            else:
                path.write_text(output)
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
