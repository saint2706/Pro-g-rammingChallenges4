"""Automated tests for the File Compression Utility backend."""
from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

MODULE_DIR = Path(__file__).resolve().parents[1] / "Practical" / "File Compression Utility"
if str(MODULE_DIR) not in sys.path:
    sys.path.insert(0, str(MODULE_DIR))

from backend import (  # type: ignore  # noqa: E402
    ArchiveFormat,
    ArchiveManager,
    ArchiveOperationError,
    ArchivePhase,
)


class ArchiveManagerTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        self.tmp_path = Path(self._tmp.name)

    # ------------------------------------------------------------------
    def test_zip_round_trip(self) -> None:
        sample_root = self._create_sample_tree("zipdata")
        destination = self.tmp_path / "sample.zip"
        progress: list = []

        ArchiveManager.create_archive(
            sources=[sample_root],
            destination=destination,
            archive_format=ArchiveFormat.ZIP_DEFLATED,
            progress_callback=progress.append,
        )

        self.assertTrue(destination.exists(), "Archive should be written")
        self._assert_progress_sequence(progress, ArchivePhase.COMPRESSING)

        output_dir = self.tmp_path / "zip_extract"
        extract_progress: list = []
        ArchiveManager.extract_archive(
            archive_path=destination,
            destination=output_dir,
            progress_callback=extract_progress.append,
        )

        self._assert_progress_sequence(extract_progress, ArchivePhase.EXTRACTING)
        self._assert_tree_restored(output_dir / sample_root.name)

    # ------------------------------------------------------------------
    def test_tar_round_trip(self) -> None:
        sample_root = self._create_sample_tree("tardata")
        destination = self.tmp_path / "sample.tar.gz"

        ArchiveManager.create_archive(
            sources=[sample_root],
            destination=destination,
            archive_format=ArchiveFormat.TAR_GZ,
        )

        self.assertTrue(destination.exists(), "TAR archive should be created")

        output_dir = self.tmp_path / "tar_extract"
        ArchiveManager.extract_archive(archive_path=destination, destination=output_dir)
        self._assert_tree_restored(output_dir / sample_root.name)

    # ------------------------------------------------------------------
    def test_missing_source_raises(self) -> None:
        destination = self.tmp_path / "broken.zip"
        with self.assertRaises(ArchiveOperationError):
            ArchiveManager.create_archive(
                sources=[self.tmp_path / "does-not-exist.txt"],
                destination=destination,
                archive_format=ArchiveFormat.ZIP_DEFLATED,
            )

    # ------------------------------------------------------------------
    def test_detect_format_invalid(self) -> None:
        with self.assertRaises(ArchiveOperationError):
            ArchiveManager.detect_format(Path("archive.unknown"))

    # Helpers -----------------------------------------------------------
    def _create_sample_tree(self, name: str) -> Path:
        root = self.tmp_path / name
        (root / "nested").mkdir(parents=True)
        (root / "nested" / "file.txt").write_text("hello", encoding="utf-8")
        (root / "root.txt").write_text("world", encoding="utf-8")
        return root

    def _assert_tree_restored(self, root: Path) -> None:
        self.assertTrue((root / "nested" / "file.txt").exists(), "Nested file missing")
        self.assertEqual((root / "nested" / "file.txt").read_text(encoding="utf-8"), "hello")
        self.assertEqual((root / "root.txt").read_text(encoding="utf-8"), "world")

    def _assert_progress_sequence(self, events: list, phase: ArchivePhase) -> None:
        self.assertGreaterEqual(len(events), 1, "Should emit at least scanning event")
        scanning = events[0]
        self.assertEqual(scanning.phase, ArchivePhase.SCANNING)
        totals = scanning.total
        self.assertGreaterEqual(totals, 1)
        for index, event in enumerate(events[1:], start=1):
            self.assertEqual(event.phase, phase)
            self.assertEqual(event.current, index)
            self.assertEqual(event.total, totals)
            self.assertGreaterEqual(event.fraction, 0.0)
            self.assertLessEqual(event.fraction, 1.0)


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
