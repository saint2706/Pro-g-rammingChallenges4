# File Compression Utility

A desktop-friendly archiving tool with both a reusable backend API and a Tkinter-based GUI. The app focuses on common archive formats (ZIP, TAR, TAR.GZ, TAR.BZ2, TAR.XZ) and provides progress callbacks, error handling, and optional drag-and-drop support for batch operations.

## Features

- **Multiple formats:** ZIP and TAR variants (plain, gzip, bzip2, xz) powered by Python's `zipfile` and `tarfile` modules.
- **Progress reporting:** The backend exposes structured progress updates for both compression and extraction workflows.
- **Robust error handling:** Clear `ArchiveOperationError` exceptions wrap underlying issues (missing files, permission errors, unsupported formats).
- **GUI convenience:** Drag-and-drop (when `tkinterdnd2` is installed), manual file pickers, compression presets, destination chooser, and live progress bar.
- **Reusable backend:** The GUI is thin; you can script the `backend` module for automation or integrate it elsewhere.
- **Automated tests:** Archive creation/extraction are covered to prevent regressions.

## Installing

The project is stdlib-only unless you want drag-and-drop. Optional dependency:

```bash
pip install tkinterdnd2
```

On Linux you may need Tk (`sudo apt install python3-tk`). macOS and Windows ship Tk with the CPython installers.

## Running the GUI

```bash
python gui.py
```

From here you can:

1. Drag files (or use **Add Files**) into the compression list.
2. Choose an archive preset (ZIP Deflated, ZIP Stored, TAR GZip, etc.).
3. Pick a destination folder and archive name.
4. Click **Create Archive** and watch the progress bar.
5. For extraction, drop/choose an archive on the right pane, set the output folder, and hit **Extract**.

If `tkinterdnd2` is unavailable, the interface falls back to manual selection with a status banner explaining how to enable drag-and-drop.

## Backend Usage Example

```python
from pathlib import Path
from backend import ArchiveFormat, ArchiveManager, ProgressEvent

def report(event: ProgressEvent) -> None:
    print(f"[{event.phase}] {event.filename} ({event.current}/{event.total})")

ArchiveManager.create_archive(
    sources=[Path("docs"), Path("notes.txt")],
    destination=Path("backup.zip"),
    archive_format=ArchiveFormat.ZIP_DEFLATED,
    progress_callback=report,
)

ArchiveManager.extract_archive(
    archive_path=Path("backup.zip"),
    destination=Path("restored"),
    progress_callback=report,
)
```

## Tests

Run the backend tests with:

```bash
python -m unittest discover -s tests -p "test_*.py"
```

The suite uses temporary directories and cleans up after itself.

## Roadmap Ideas

- Threaded worker that updates the GUI without blocking during large archives.
- Support for password-protected ZIP files via `pyzipper` (opt-in dependency).
- Contextual logging view for advanced troubleshooting.
- CLI wrapper around the backend for headless automation.
