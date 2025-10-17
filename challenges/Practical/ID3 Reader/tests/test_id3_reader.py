from __future__ import annotations

import importlib.util
import sys
import json
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[1] / "id3_reader.py"
SPEC = importlib.util.spec_from_file_location("id3_reader", MODULE_PATH)
id3_reader = importlib.util.module_from_spec(SPEC)
assert SPEC and SPEC.loader
sys.modules[SPEC.name] = id3_reader
SPEC.loader.exec_module(id3_reader)  # type: ignore[arg-type]

parse_id3 = id3_reader.parse_id3
export_json = id3_reader.export_json
export_csv = id3_reader.export_csv


def _write_id3v1_file(path: Path) -> None:
    title = "Test Title".ljust(30, " ")
    artist = "Test Artist".ljust(30, " ")
    album = "Test Album".ljust(30, " ")
    year = "1999"
    comment = ("Comment" + "\x00" * 20).ljust(30, "\x00")
    genre = bytes([17])  # Rock
    data = (
        b"TAG"
        + title.encode("latin-1")
        + artist.encode("latin-1")
        + album.encode("latin-1")
    )
    data += year.encode("latin-1") + comment.encode("latin-1") + genre
    path.write_bytes(data)


def _syncsafe(size: int) -> bytes:
    return bytes(
        [
            (size >> 21) & 0x7F,
            (size >> 14) & 0x7F,
            (size >> 7) & 0x7F,
            size & 0x7F,
        ]
    )


def _text_frame(frame_id: str, text: str) -> bytes:
    payload = b"\x00" + text.encode("latin-1")
    header = frame_id.encode("ascii") + len(payload).to_bytes(4, "big") + b"\x00\x00"
    return header + payload


def _comment_frame(text: str) -> bytes:
    payload = b"\x00eng" + b"\x00" + text.encode("latin-1")
    header = b"COMM" + len(payload).to_bytes(4, "big") + b"\x00\x00"
    return header + payload


def _write_id3v2_file(path: Path) -> None:
    frames = b"".join(
        [
            _text_frame("TIT2", "V2 Title"),
            _text_frame("TPE1", "V2 Artist"),
            _text_frame("TALB", "V2 Album"),
            _text_frame("TDRC", "2024"),
            _text_frame("TCON", "Electronic"),
            _text_frame("TRCK", "4"),
            _comment_frame("ID3v2 comment"),
        ]
    )
    header = b"ID3" + bytes([3, 0, 0]) + _syncsafe(len(frames))
    path.write_bytes(header + frames)


def test_parse_id3v1(tmp_path: Path) -> None:
    mp3_path = tmp_path / "id3v1.mp3"
    _write_id3v1_file(mp3_path)

    metadata = parse_id3(mp3_path)

    assert "ID3v1" in metadata.versions[0]
    assert metadata.tags["title"] == "Test Title"
    assert metadata.tags["genre"] == "Rock"


def test_parse_id3v2(tmp_path: Path) -> None:
    mp3_path = tmp_path / "id3v2.mp3"
    _write_id3v2_file(mp3_path)

    metadata = parse_id3(mp3_path)

    assert "ID3v2" in metadata.versions[0]
    assert metadata.tags["title"] == "V2 Title"
    assert metadata.tags["artist"] == "V2 Artist"
    assert metadata.tags["year"] == "2024"
    assert metadata.tags["comment"] == "ID3v2 comment"


def test_export_json_and_csv(tmp_path: Path) -> None:
    mp3_path = tmp_path / "sample.mp3"
    _write_id3v2_file(mp3_path)

    metadata = parse_id3(mp3_path)
    json_path = tmp_path / "tags.json"
    csv_path = tmp_path / "tags.csv"

    export_json([metadata], json_path)
    export_csv([metadata], csv_path)

    json_content = json.loads(json_path.read_text(encoding="utf-8"))
    assert json_content[0]["tags"]["title"] == "V2 Title"

    csv_lines = csv_path.read_text(encoding="utf-8").splitlines()
    assert csv_lines[0].startswith("file_path,versions")
    assert "V2 Title" in csv_lines[1]
