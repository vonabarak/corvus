"""File and symlink helpers shared by Gentoo stages."""

from pathlib import Path


def write(path: Path, value: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(value)


def link(path: Path, target: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.unlink(missing_ok=True)
    path.symlink_to(target)


def safe_path(root: Path, relative: str) -> Path:
    """Reject traversal and symlink ancestors before accessing an image path."""
    part = Path(relative)
    if part.is_absolute() or ".." in part.parts:
        raise ValueError(f"Unsafe image path: {relative}")
    target = root / part
    if target == root:
        return target
    for parent in target.parents:
        if parent == root:
            break
        if root not in parent.parents:
            raise ValueError(f"Path is outside image: {target}")
        if parent.is_symlink() or (parent.exists() and not parent.is_dir()):
            raise RuntimeError(f"Unsafe residue path: {target}")
    return target


def remove(root: Path, relative: str) -> None:
    """Delete image contents without following links or crossing child mounts."""
    import re
    import shutil

    target = safe_path(root, relative)
    if target == root:
        raise ValueError("Refusing to remove the image root")
    # mountinfo escapes spaces, tabs, newlines and backslashes as octal codes.
    for line in Path("/proc/self/mountinfo").read_text().splitlines():
        mount = Path(
            re.sub(
                r"\\([0-7]{3})", lambda match: chr(int(match[1], 8)), line.split()[4]
            )
        )
        if mount == target or target in mount.parents:
            raise RuntimeError(f"Refusing to clean mounted image directory: {target}")
    if target.is_symlink():
        target.unlink()
    elif target.is_dir():
        shutil.rmtree(target)
    else:
        target.unlink(missing_ok=True)
