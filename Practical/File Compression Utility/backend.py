"""Backend utilities for the File Compression Utility project.

The module exposes a thin abstraction on top of :mod:`zipfile` and
:mod:`tarfile` that adds:

* Friendly enums and presets for supported archive formats.
* Structured progress events for compression and extraction.
* Predictable error handling via :class:`ArchiveOperationError`.

The functions are synchronous (matching the stdlib behaviour) but the
progress callbacks make it easy to integrate with GUIs or CLIs that need
feedback. Each callback receives a :class:`ProgressEvent` dataclass with
context about the current step.
"""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Callable, Iterable, List, Optional
import tarfile
import zipfile


class ArchiveOperationError(RuntimeError):
    """Wraps lower-level archive exceptions with context."""


class ArchivePhase(str, Enum):
    """Lifecycle phase for a progress event."""

    SCANNING = "scanning"
    COMPRESSING = "compressing"
    EXTRACTING = "extracting"


@dataclass(frozen=True)
class ProgressEvent:
    """Represents progress during an archive operation."""

    phase: ArchivePhase
    current: int
    total: int
    filename: str

    @property
    def fraction(self) -> float:
        """Return a 0.0-1.0 fraction if ``total`` is non-zero."""

        if self.total == 0:
            return 0.0
        return self.current / self.total


ProgressCallback = Callable[[ProgressEvent], None]


def _default_callback(event: ProgressEvent) -> None:
    """Fallback progress callback when none is provided."""

    # Intentionally no-op; users can supply their own callback.
    return None


class ArchiveFormat(Enum):
    """Supported archive formats and compression strategies."""

    ZIP_STORED = (".zip", "zip", zipfile.ZIP_STORED)
    ZIP_DEFLATED = (".zip", "zip", zipfile.ZIP_DEFLATED)
    ZIP_BZIP2 = (".zip", "zip", zipfile.ZIP_BZIP2)
    ZIP_LZMA = (".zip", "zip", zipfile.ZIP_LZMA)
    TAR = (".tar", "tar", "tar")
    TAR_GZ = (".tar.gz", "tar.gz", "gztar")
    TAR_BZ2 = (".tar.bz2", "tar.bz2", "bztar")
    TAR_XZ = (".tar.xz", "tar.xz", "xztar")

    def extension(self) -> str:
        return self.value[0]

    def label(self) -> str:
        return self.value[1]

    def tar_mode(self) -> Optional[str]:
        mode = self.value[2]
        return mode if isinstance(mode, str) and mode != "zip" else None

    def zip_compression(self) -> Optional[int]:
        mode = self.value[2]
        return mode if isinstance(mode, int) else None

    @property
    def is_zip(self) -> bool:
        return self.zip_compression() is not None

    @property
    def is_tar(self) -> bool:
        return self.tar_mode() is not None


class ArchiveManager:
    """High level operations for creating and extracting archives."""

    @staticmethod
    def supported_formats() -> List[ArchiveFormat]:
        return list(ArchiveFormat)

    @staticmethod
    def detect_format(path: Path) -> ArchiveFormat:
        """Infer the format from a file extension.

        Raises
        ------
        ArchiveOperationError
            If the format is not recognised.
        """

        lower = path.name.lower()
        for fmt in ArchiveFormat:
            if lower.endswith(fmt.extension()):
                return fmt
        raise ArchiveOperationError(f"Unsupported archive format for '{path}'.")

    @staticmethod
    def create_archive(
        sources: Iterable[Path],
        destination: Path,
        archive_format: ArchiveFormat,
        progress_callback: ProgressCallback = _default_callback,
    ) -> None:
        """Create a new archive from ``sources``.

        Parameters
        ----------
        sources:
            Files or directories to include. Directories are added recursively.
        destination:
            The resulting archive path (must include the extension).
        archive_format:
            One of :class:`ArchiveFormat`.
        progress_callback:
            Called with :class:`ProgressEvent` instances during the process.
        """

        paths = [Path(src) for src in sources]
        if not paths:
            raise ArchiveOperationError("At least one source path is required.")
        for path in paths:
            if not path.exists():
                raise ArchiveOperationError(f"Source path '{path}' does not exist.")

        destination = Path(destination)
        destination.parent.mkdir(parents=True, exist_ok=True)

        try:
            members: List[tuple[Path, str]] = []
            for path in paths:
                if path.is_file():
                    members.append((path, path.name))
                else:
                    base = path.name
                    for child in path.rglob("*"):
                        if child.is_file():
                            arcname = Path(base) / child.relative_to(path)
                            members.append((child, arcname.as_posix()))
            total = len(members)
            progress_callback(ProgressEvent(ArchivePhase.SCANNING, 0, total, ""))

            if archive_format.is_zip:
                ArchiveManager._create_zip(members, destination, archive_format, progress_callback)
            elif archive_format.is_tar:
                ArchiveManager._create_tar(members, destination, archive_format, progress_callback)
            else:
                raise ArchiveOperationError(f"Unsupported format: {archive_format}")
        except (OSError, zipfile.BadZipFile, tarfile.TarError) as exc:
            raise ArchiveOperationError(str(exc)) from exc

    @staticmethod
    def extract_archive(
        archive_path: Path,
        destination: Path,
        progress_callback: ProgressCallback = _default_callback,
    ) -> None:
        """Extract an archive to ``destination`` with progress reporting."""

        archive_path = Path(archive_path)
        if not archive_path.exists():
            raise ArchiveOperationError(f"Archive '{archive_path}' does not exist.")
        destination = Path(destination)
        destination.mkdir(parents=True, exist_ok=True)

        try:
            fmt = ArchiveManager.detect_format(archive_path)
            if fmt.is_zip:
                ArchiveManager._extract_zip(archive_path, destination, progress_callback)
            elif fmt.is_tar:
                ArchiveManager._extract_tar(archive_path, destination, progress_callback)
            else:
                raise ArchiveOperationError(f"Unsupported archive format for '{archive_path}'.")
        except (OSError, zipfile.BadZipFile, tarfile.TarError) as exc:
            raise ArchiveOperationError(str(exc)) from exc

    @staticmethod
    def _create_zip(
        members: List[tuple[Path, str]],
        destination: Path,
        archive_format: ArchiveFormat,
        progress_callback: ProgressCallback,
    ) -> None:
        compression = archive_format.zip_compression()
        assert compression is not None
        with zipfile.ZipFile(destination, mode="w", compression=compression) as zf:
            for index, (member, arcname) in enumerate(members, start=1):
                progress_callback(
                    ProgressEvent(ArchivePhase.COMPRESSING, index, len(members), arcname)
                )
                zf.write(member, arcname=arcname)

    @staticmethod
    def _create_tar(
        members: List[tuple[Path, str]],
        destination: Path,
        archive_format: ArchiveFormat,
        progress_callback: ProgressCallback,
    ) -> None:
        mode_map = {
            ArchiveFormat.TAR: "w",
            ArchiveFormat.TAR_GZ: "w:gz",
            ArchiveFormat.TAR_BZ2: "w:bz2",
            ArchiveFormat.TAR_XZ: "w:xz",
        }
        mode = mode_map.get(archive_format)
        if mode is None:
            raise ArchiveOperationError(f"Unsupported TAR format: {archive_format}")

        with tarfile.open(destination, mode) as tf:
            total = len(members)
            for index, (path, arcname) in enumerate(members, start=1):
                progress_callback(
                    ProgressEvent(ArchivePhase.COMPRESSING, index, total, arcname)
                )
                tf.add(path, arcname=arcname)

    @staticmethod
    def _extract_zip(
        archive_path: Path,
        destination: Path,
        progress_callback: ProgressCallback,
    ) -> None:
        with zipfile.ZipFile(archive_path, mode="r") as zf:
            names = zf.namelist()
            total = len(names)
            progress_callback(ProgressEvent(ArchivePhase.SCANNING, 0, total, ""))
            for index, name in enumerate(names, start=1):
                progress_callback(
                    ProgressEvent(ArchivePhase.EXTRACTING, index, total, name)
                )
                zf.extract(member=name, path=destination)

    @staticmethod
    def _extract_tar(
        archive_path: Path,
        destination: Path,
        progress_callback: ProgressCallback,
    ) -> None:
        with tarfile.open(archive_path, mode="r:*") as tf:
            members = [m for m in tf.getmembers() if m.isfile() or m.isdir()]
            total = len(members)
            progress_callback(ProgressEvent(ArchivePhase.SCANNING, 0, total, ""))
            for index, member in enumerate(members, start=1):
                progress_callback(
                    ProgressEvent(ArchivePhase.EXTRACTING, index, total, member.name)
                )
                tf.extract(member=member, path=destination)
