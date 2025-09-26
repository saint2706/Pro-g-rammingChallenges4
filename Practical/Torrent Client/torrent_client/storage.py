"""Disk storage primitives for torrent downloads."""
from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional


@dataclass
class FileStorage:
    """Persist torrent data to disk with simple resume support."""

    path: Path
    total_size: int
    piece_length: int
    info_hash: bytes
    resume: bool = True
    bitfield: List[bool] = field(default_factory=list)

    def __post_init__(self) -> None:
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.resume_path = self.path.with_suffix(self.path.suffix + ".resume.json")
        piece_count = (self.total_size + self.piece_length - 1) // self.piece_length
        if not self.bitfield:
            self.bitfield = [False] * piece_count
        if self.path.exists():
            self._prepare_existing_file(piece_count)
        else:
            self._allocate_file()
        if self.resume and self.resume_path.exists():
            self._load_resume(piece_count)

    def _prepare_existing_file(self, piece_count: int) -> None:
        size = self.path.stat().st_size
        if size < self.total_size:
            with self.path.open("r+b") as handle:
                handle.truncate(self.total_size)
        elif size > self.total_size:
            with self.path.open("r+b") as handle:
                handle.truncate(self.total_size)
        # ensure bitfield length matches piece count
        if len(self.bitfield) != piece_count:
            self.bitfield = [False] * piece_count

    def _allocate_file(self) -> None:
        with self.path.open("wb") as handle:
            if self.total_size:
                handle.seek(self.total_size - 1)
                handle.write(b"\0")

    def _load_resume(self, piece_count: int) -> None:
        try:
            data = json.loads(self.resume_path.read_text())
        except json.JSONDecodeError:
            return
        if data.get("info_hash") != self.info_hash.hex():
            return
        stored_size = data.get("total_size")
        if stored_size != self.total_size:
            return
        bitfield = data.get("bitfield")
        if isinstance(bitfield, list) and len(bitfield) == piece_count:
            self.bitfield = [bool(x) for x in bitfield]

    def _save_resume(self) -> None:
        if not self.resume:
            return
        data = {
            "info_hash": self.info_hash.hex(),
            "total_size": self.total_size,
            "piece_length": self.piece_length,
            "bitfield": self.bitfield,
        }
        self.resume_path.write_text(json.dumps(data))

    def has_piece(self, index: int, expected_hash: bytes) -> bool:
        if index < 0 or index >= len(self.bitfield):
            raise IndexError("Piece index out of range")
        if self.bitfield[index]:
            return True
        data = self.read_piece(index)
        if data is None:
            return False
        digest = hashlib.sha1(data).digest()
        if digest == expected_hash:
            self.bitfield[index] = True
            self._save_resume()
            return True
        return False

    def read_piece(self, index: int) -> Optional[bytes]:
        offset = index * self.piece_length
        if offset >= self.total_size:
            return None
        length = min(self.piece_length, self.total_size - offset)
        with self.path.open("rb") as handle:
            handle.seek(offset)
            data = handle.read(length)
        if len(data) != length:
            return None
        return data

    def write_piece(self, index: int, data: bytes, expected_hash: bytes) -> bool:
        if index < 0 or index >= len(self.bitfield):
            raise IndexError("Piece index out of range")
        digest = hashlib.sha1(data).digest()
        if digest != expected_hash:
            return False
        offset = index * self.piece_length
        length = min(self.piece_length, self.total_size - offset)
        if len(data) != length:
            return False
        with self.path.open("r+b") as handle:
            handle.seek(offset)
            handle.write(data)
        self.bitfield[index] = True
        self._save_resume()
        return True

    @property
    def completed_pieces(self) -> int:
        return sum(self.bitfield)

    @property
    def bytes_completed(self) -> int:
        total = 0
        for index, have in enumerate(self.bitfield):
            if not have:
                continue
            offset = index * self.piece_length
            if offset >= self.total_size:
                continue
            length = min(self.piece_length, self.total_size - offset)
            total += length
        return min(total, self.total_size)


__all__ = ["FileStorage"]
