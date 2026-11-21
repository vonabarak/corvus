"""Build a deterministic executable bundle without third-party dependencies."""

import argparse
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile, ZipInfo


def bundle(
    script: Path, destination: Path, *, include_modules: tuple[str, ...] = ()
) -> None:
    source = Path(__file__).resolve().parent
    files = {
        f"corvus_image_builder/{p.relative_to(source).as_posix()}": p.read_bytes()
        for p in sorted(source.rglob("*.py"))
        if p != source / "bundle.py"
    }
    files["__main__.py"] = script.read_bytes()
    for name in include_modules:
        path = Path(name)
        if (
            path.name != name
            or path.suffix != ".py"
            or not path.stem.isidentifier()
            or path.stem == "corvus_image_builder"
            or name in files
        ):
            raise ValueError(f"Invalid or duplicate recipe module: {name}")
        files[name] = script.with_name(name).read_bytes()
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_suffix(".tmp")
    with ZipFile(temporary, "w", compression=ZIP_DEFLATED) as archive:
        for name, content in sorted(files.items()):
            info = ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
            info.compress_type = ZIP_DEFLATED
            info.external_attr = 0o100644 << 16
            archive.writestr(info, content)
    temporary.replace(destination)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("script", type=Path)
    parser.add_argument("destination", type=Path)
    parser.add_argument(
        "--include-module",
        action="append",
        default=[],
        help="Include a sibling .py module",
    )
    args = parser.parse_args()
    bundle(args.script, args.destination, include_modules=tuple(args.include_module))
