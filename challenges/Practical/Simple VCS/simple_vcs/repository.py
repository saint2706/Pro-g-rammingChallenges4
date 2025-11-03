"""Simple version control repository management."""

from __future__ import annotations

import json
import shutil
import time
import uuid
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional

CONFIG_DIR_NAME = ".svc"
CONFIG_FILE_NAME = "config.json"
STORE_DIR_NAME = "store"
DEFAULT_REVISION_LIMIT = 5


@dataclass
class Revision:
    """Metadata about a single revision."""

    revision_id: str
    message: str
    timestamp: float

    def to_dict(self) -> Dict[str, object]:
        return {
            "revision_id": self.revision_id,
            "message": self.message,
            "timestamp": self.timestamp,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, object]) -> "Revision":
        return cls(
            revision_id=str(data["revision_id"]),
            message=str(data.get("message", "")),
            timestamp=float(data.get("timestamp", 0.0)),
        )


@dataclass
class FileEntry:
    """Configuration and revisions for a tracked file."""

    revision_limit: int = DEFAULT_REVISION_LIMIT
    locked: bool = False
    revisions: List[Revision] = field(default_factory=list)

    def to_dict(self) -> Dict[str, object]:
        return {
            "revision_limit": self.revision_limit,
            "locked": self.locked,
            "revisions": [rev.to_dict() for rev in self.revisions],
        }

    @classmethod
    def from_dict(cls, data: Dict[str, object]) -> "FileEntry":
        revisions = [Revision.from_dict(item) for item in data.get("revisions", [])]
        return cls(
            revision_limit=int(data.get("revision_limit", DEFAULT_REVISION_LIMIT)),
            locked=bool(data.get("locked", False)),
            revisions=revisions,
        )


class Repository:
    """Encapsulates repository operations."""

    def __init__(self, root: Path) -> None:
        self.root = Path(root).resolve()
        self.config_dir = self.root / CONFIG_DIR_NAME
        self.config_path = self.config_dir / CONFIG_FILE_NAME
        self.store_dir = self.config_dir / STORE_DIR_NAME
        self._config: Dict[str, object] = {}

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------
    def _load_config(self) -> None:
        if not self.config_path.exists():
            raise RuntimeError("Repository not initialized. Run 'init' first.")
        with self.config_path.open("r", encoding="utf8") as fh:
            data = json.load(fh)
        files = {
            path: FileEntry.from_dict(entry)
            for path, entry in data.get("files", {}).items()
        }
        self._config = {"files": files}

    def _save_config(self) -> None:
        files: Dict[str, FileEntry] = self._config.get("files", {})  # type: ignore[assignment]
        serializable = {
            "version": 1,
            "files": {path: entry.to_dict() for path, entry in files.items()},
        }
        self.config_dir.mkdir(parents=True, exist_ok=True)
        with self.config_path.open("w", encoding="utf8") as fh:
            json.dump(serializable, fh, indent=2, sort_keys=True)

    def _ensure_loaded(self) -> None:
        if not self._config:
            self._load_config()

    def _get_relative_path(self, file_path: Path) -> str:
        try:
            relative = file_path.resolve().relative_to(self.root)
        except ValueError as exc:  # pragma: no cover - guard clause
            raise RuntimeError("File must reside inside the repository root") from exc
        return relative.as_posix()

    def _get_file_entry(self, relative_path: str) -> FileEntry:
        self._ensure_loaded()
        files: Dict[str, FileEntry] = self._config.setdefault("files", {})  # type: ignore[assignment]
        if relative_path not in files:
            files[relative_path] = FileEntry()
        return files[relative_path]

    def _store_revision_path(self, relative_path: str, revision_id: str) -> Path:
        safe_path = Path(relative_path)
        storage_path = self.store_dir / safe_path
        storage_path.mkdir(parents=True, exist_ok=True)
        return storage_path / revision_id

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------
    def init(self, force: bool = False) -> None:
        """Initialise a repository."""

        if self.config_path.exists() and not force:
            raise RuntimeError(
                "Repository already initialised. Use force=True to overwrite."
            )
        self.config_dir.mkdir(parents=True, exist_ok=True)
        self.store_dir.mkdir(parents=True, exist_ok=True)
        self._config = {"files": {}}
        self._save_config()

    def set_revision_limit(self, file_path: Path, limit: int) -> None:
        if limit < 1:
            raise ValueError("Revision limit must be at least 1")
        relative = self._get_relative_path(file_path)
        entry = self._get_file_entry(relative)
        entry.revision_limit = limit
        self._save_config()

    def lock_file(self, file_path: Path) -> None:
        relative = self._get_relative_path(file_path)
        entry = self._get_file_entry(relative)
        entry.locked = True
        self._save_config()

    def unlock_file(self, file_path: Path) -> None:
        relative = self._get_relative_path(file_path)
        entry = self._get_file_entry(relative)
        entry.locked = False
        self._save_config()

    def get_file_entry(self, file_path: Path) -> FileEntry:
        relative = self._get_relative_path(file_path)
        self._ensure_loaded()
        files: Dict[str, FileEntry] = self._config.get("files", {})  # type: ignore[assignment]
        if relative not in files:
            raise RuntimeError(f"File '{relative}' is not tracked yet.")
        return files[relative]

    def commit(self, files: List[Path], message: str) -> List[Revision]:
        if not message:
            raise ValueError("Commit message must not be empty")
        self._ensure_loaded()
        committed: List[Revision] = []
        for file_path in files:
            absolute = file_path.resolve()
            if not absolute.exists():
                raise FileNotFoundError(f"Cannot commit missing file: {file_path}")
            relative = self._get_relative_path(absolute)
            entry = self._get_file_entry(relative)
            if entry.locked:
                raise RuntimeError(
                    f"File '{relative}' is locked and cannot be committed."
                )
            revision_id = uuid.uuid4().hex
            storage_path = self._store_revision_path(relative, revision_id)
            shutil.copy2(absolute, storage_path)
            revision = Revision(
                revision_id=revision_id, message=message, timestamp=time.time()
            )
            entry.revisions.append(revision)
            # Enforce revision limit
            while len(entry.revisions) > entry.revision_limit:
                oldest = entry.revisions.pop(0)
                old_path = self._store_revision_path(relative, oldest.revision_id)
                if old_path.exists():
                    old_path.unlink()
            committed.append(revision)
        self._save_config()
        return committed

    def list_revisions(self, file_path: Path) -> List[Revision]:
        entry = self.get_file_entry(file_path)
        return list(entry.revisions)

    def checkout(self, file_path: Path, revision_id: Optional[str] = None) -> Revision:
        entry = self.get_file_entry(file_path)
        if not entry.revisions:
            raise RuntimeError("No revisions recorded for this file.")
        revision: Revision
        if revision_id is None:
            revision = entry.revisions[-1]
        else:
            matches = [rev for rev in entry.revisions if rev.revision_id == revision_id]
            if not matches:
                raise RuntimeError(f"Revision '{revision_id}' not found.")
            revision = matches[0]
        relative = self._get_relative_path(Path(file_path))
        stored = self._store_revision_path(relative, revision.revision_id)
        if not stored.exists():
            raise RuntimeError("Stored revision is missing from the repository store.")
        destination = self.root / relative
        shutil.copy2(stored, destination)
        return revision

    def diff(self, file_path: Path, revision_id: Optional[str] = None) -> str:
        import difflib

        entry = self.get_file_entry(file_path)
        if not entry.revisions:
            raise RuntimeError("No revisions available for diff.")
        revision: Revision
        if revision_id is None:
            revision = entry.revisions[-1]
        else:
            matches = [rev for rev in entry.revisions if rev.revision_id == revision_id]
            if not matches:
                raise RuntimeError(f"Revision '{revision_id}' not found.")
            revision = matches[0]
        relative = self._get_relative_path(Path(file_path))
        stored = self._store_revision_path(relative, revision.revision_id)
        if not stored.exists():
            raise RuntimeError("Stored revision is missing from disk.")
        with stored.open("r", encoding="utf8", errors="replace") as fh:
            stored_lines = fh.readlines()
        working_path = self.root / relative
        if working_path.exists():
            with working_path.open("r", encoding="utf8", errors="replace") as fh:
                working_lines = fh.readlines()
        else:
            working_lines = []
        diff = difflib.unified_diff(
            stored_lines,
            working_lines,
            fromfile=f"revision:{revision.revision_id}",
            tofile="working",  # emphasise diff direction
        )
        return "".join(diff)

    def show_status(self) -> Dict[str, FileEntry]:
        self._ensure_loaded()
        return dict(self._config.get("files", {}))  # type: ignore[return-value]
