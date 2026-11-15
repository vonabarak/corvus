"""Build a deterministic executable bundle without third-party dependencies."""

import argparse
from pathlib import Path
from zipfile import ZIP_DEFLATED, ZipFile, ZipInfo


def bundle(destination: Path) -> None:
    source = Path(__file__).parent / "corvus_image_builder"
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary = destination.with_suffix(".tmp")
    with ZipFile(temporary, "w", compression=ZIP_DEFLATED) as archive:
        files = {
            f"corvus_image_builder/{p.name}": p.read_bytes()
            for p in sorted(source.glob("*.py"))
        }
        files["__main__.py"] = b"from corvus_image_builder.cli import main\nmain()\n"
        for name, content in sorted(files.items()):
            info = ZipInfo(name, date_time=(1980, 1, 1, 0, 0, 0))
            info.compress_type = ZIP_DEFLATED
            info.external_attr = 0o100644 << 16
            archive.writestr(info, content)
    temporary.replace(destination)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("destination", type=Path)
    bundle(parser.parse_args().destination)
