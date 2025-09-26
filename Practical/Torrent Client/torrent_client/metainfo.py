"""Torrent metadata loading and minimal bencode parsing utilities."""
from __future__ import annotations

from dataclasses import dataclass
import hashlib
from pathlib import Path
from typing import Dict, List, Tuple, Union, Any

BValue = Union[int, bytes, List["BValue"], Dict[bytes, "BValue"]]


class BencodeError(ValueError):
    """Raised when bencoded data cannot be parsed."""


def _read_int(data: bytes, index: int) -> Tuple[int, int]:
    end = data.index(b"e", index)
    num = int(data[index:end])
    return num, end + 1


def _read_bytes(data: bytes, index: int) -> Tuple[bytes, int]:
    colon = data.index(b":", index)
    length = int(data[index:colon])
    start = colon + 1
    end = start + length
    return data[start:end], end


def bdecode(data: bytes, index: int = 0) -> Tuple[BValue, int]:
    """Decode bencoded ``data`` starting at ``index``.

    Returns a tuple of (value, next_index).
    """

    prefix = data[index:index + 1]
    if not prefix:
        raise BencodeError("Unexpected end of data")

    if prefix == b"i":
        return _read_int(data, index + 1)
    if prefix == b"l":
        items: List[BValue] = []
        i = index + 1
        while data[i:i + 1] != b"e":
            value, i = bdecode(data, i)
            items.append(value)
        return items, i + 1
    if prefix == b"d":
        result: Dict[bytes, BValue] = {}
        i = index + 1
        while data[i:i + 1] != b"e":
            key, i = bdecode(data, i)
            if not isinstance(key, bytes):
                raise BencodeError("Dictionary keys must be bytes")
            value, i = bdecode(data, i)
            result[key] = value
        return result, i + 1
    if prefix.isdigit():
        return _read_bytes(data, index)
    raise BencodeError(f"Invalid bencode prefix: {prefix!r}")


@dataclass
class TorrentMetaInfo:
    """Simplified representation of a single-file torrent."""

    announce: str
    piece_length: int
    pieces: List[bytes]
    length: int
    name: str
    info_hash: bytes
    raw_info: Dict[str, Any]

    @classmethod
    def from_file(cls, path: Union[str, Path]) -> "TorrentMetaInfo":
        raw = Path(path).read_bytes()
        value, index = bdecode(raw)
        if index != len(raw):
            raise BencodeError("Trailing data after torrent metainfo")
        if not isinstance(value, dict):
            raise BencodeError("Top-level torrent structure must be a dict")
        announce = value.get(b"announce")
        info = value.get(b"info")
        if not isinstance(announce, bytes) or not isinstance(info, dict):
            raise BencodeError("Missing announce or info dictionary")

        info_hash = hashlib.sha1(bencode(info)).digest()
        pieces_blob = info.get(b"pieces")
        if not isinstance(pieces_blob, bytes) or len(pieces_blob) % 20:
            raise BencodeError("Pieces blob must be 20 byte aligned")
        pieces = [pieces_blob[i:i + 20] for i in range(0, len(pieces_blob), 20)]
        piece_length = info.get(b"piece length")
        length = info.get(b"length")
        name = info.get(b"name")
        if not all(isinstance(v, int) for v in (piece_length, length)):
            raise BencodeError("piece length and length must be integers")
        if not isinstance(name, bytes):
            raise BencodeError("name must be bytes")

        raw_info = {k.decode("utf-8", "ignore"): v for k, v in info.items()}
        return cls(
            announce=announce.decode("utf-8", "ignore"),
            piece_length=int(piece_length),
            pieces=pieces,
            length=int(length),
            name=name.decode("utf-8", "ignore"),
            info_hash=info_hash,
            raw_info=raw_info,
        )


def bencode(value: BValue) -> bytes:
    """Encode a Python object into bencode.

    Only the subset used for computing the ``info_hash`` is implemented.
    """

    if isinstance(value, int):
        return b"i" + str(value).encode() + b"e"
    if isinstance(value, bytes):
        return str(len(value)).encode() + b":" + value
    if isinstance(value, list):
        return b"l" + b"".join(bencode(v) for v in value) + b"e"
    if isinstance(value, dict):
        # Keys must be sorted lexicographically
        encoded_items = []
        for key in sorted(value.keys()):
            if not isinstance(key, bytes):
                raise BencodeError("Dictionary keys must be bytes")
            encoded_items.append(bencode(key))
            encoded_items.append(bencode(value[key]))
        return b"d" + b"".join(encoded_items) + b"e"
    raise TypeError(f"Unsupported type for bencode: {type(value)!r}")


__all__ = ["TorrentMetaInfo", "bdecode", "bencode", "BencodeError"]
