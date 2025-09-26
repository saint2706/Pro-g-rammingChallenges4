# Installation & Packaging Guide

This document describes how to install the Sublime Text C++ IDE Plugin, configure its dependencies, and distribute it as a `.sublime-package` archive.

## Prerequisites

1. **Sublime Text 3 or 4** installed on your system.
2. **LLVM/Clang** including the `libclang` shared library.
   - macOS (Homebrew): `brew install llvm`
   - Ubuntu/Debian: `sudo apt install clang llvm python3-clang`
   - Windows: install [LLVM official builds](https://releases.llvm.org/) and ensure `libclang.dll` is available.
3. **Python bindings** for clang (`clang.cindex`). Most package managers install them alongside clang (e.g., `python3-clang`).

## Installing the Plugin (Developer Mode)

1. Locate Sublime's `Packages` directory:
   - macOS: `~/Library/Application Support/Sublime Text/Packages`
   - Linux: `~/.config/sublime-text/Packages`
   - Windows: `%AppData%\Sublime Text\Packages`
2. Clone or copy the `C++ IDE Plugin` folder into `Packages/User/`.
3. Restart Sublime Text (or use `Preferences → Browse Packages…` and copy the folder while Sublime is open; it auto-reloads plugins).
4. Open the command palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and run **“C++ IDE Plugin: Open Settings”** to configure paths.

## Installing the Plugin (Packaged)

1. From the repository root, run the packaging helper:

   ```bash
   python3 Practical/C++\ IDE\ Plugin/scripts/package.py
   ```

   This creates `C++IDEPlugin.sublime-package` under the `dist/` directory.

2. Copy the generated `.sublime-package` into Sublime's `Installed Packages/` directory:
   - macOS: `~/Library/Application Support/Sublime Text/Installed Packages`
   - Linux: `~/.config/sublime-text/Installed Packages`
   - Windows: `%AppData%\Sublime Text\Installed Packages`

3. Restart Sublime Text. The plugin loads from the packaged archive.

## Configuration Checklist

- **libclang path**: Set `libclang_path` directly if auto-discovery fails. On macOS with Homebrew: `/usr/local/opt/llvm/lib/libclang.dylib`.
- **Compilation arguments**: Add include directories (`-I`) and standard flags (`-std=c++17`) to match your project.
- **compile_commands.json**: Point `compile_commands_dir` to your build directory (e.g., `build/`) for compile-command aware parsing.
- **Cache directory**: Optional—specify a project-specific cache folder if you want caches inside the workspace.
- **clangd binary**: Update `clangd_binary` if the executable is not on your `PATH` (e.g., `/usr/lib/llvm-16/bin/clangd`).
- **clangd flags**: Populate `clangd_additional_flags` (e.g., `--background-index`) for extra diagnostics.

## scripts/package.py

A simple packaging script is included to automate zip creation:

```python
#!/usr/bin/env python3
"""Create a distributable .sublime-package archive."""

import os
import zipfile
from pathlib import Path

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
    with zipfile.ZipFile(archive_path, "w", compression=zipfile.ZIP_DEFLATED) as archive:
        for relative in FILES:
            archive.write(ROOT / relative, arcname=relative)
    print(f"Wrote {archive_path}")


if __name__ == "__main__":
    main()
```

The script intentionally omits documentation and test files to keep the distributed package lean. Adjust `FILES` if you wish to include additional resources.

## Troubleshooting

| Symptom | Possible Cause | Remedy |
| ------- | -------------- | ------ |
| `Unable to import clang.cindex` | Python bindings missing | Install `python3-clang` (Linux) or `pip install clang` pointing to your LLVM install. |
| `Failed to load libclang` popup | `libclang_path` incorrect | Update `libclang_path` to the correct shared library. |
| No completions appear | Missing compile flags | Ensure `clang_arguments` include relevant `-I` paths and `-std=` flags. |
| Index command reports zero symbols | File not compiled by clang | Confirm the file uses supported C/C++ syntax and compile arguments. |

## Uninstallation

Delete the plugin folder from `Packages/User/` or remove `C++IDEPlugin.sublime-package` from `Installed Packages/` and restart Sublime Text.

