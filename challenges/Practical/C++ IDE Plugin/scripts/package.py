#!/usr/bin/env python3
"""Create a distributable .sublime-package archive."""

from pathlib import Path
import zipfile

ROOT = Path(__file__).resolve().parents[1]
DIST = ROOT / "dist"
PACKAGE_NAME = "C++IDEPlugin.sublime-package"

FILES = [
    "c_cpp_ide_plugin.py",
    "C++IDEPlugin.sublime-settings",
    "C++ IDE Plugin.sublime-commands",
    "Main.sublime-menu",
]


def main() -> None:
    DIST.mkdir(exist_ok=True)
    archive_path = DIST / PACKAGE_NAME
    with zipfile.ZipFile(
        archive_path, "w", compression=zipfile.ZIP_DEFLATED
    ) as archive:
        for relative in FILES:
            archive.write(ROOT / relative, arcname=relative)
    print(f"Wrote {archive_path}")


if __name__ == "__main__":
    main()
