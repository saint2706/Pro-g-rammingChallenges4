"""ID3 Reader CLI utilities.

Parses ID3v1/v1.1 and ID3v2 metadata from MP3 files and offers helpers for
formatting and exporting the discovered tags. The module is importable for use
in other scripts but can also be executed directly to inspect one or more MP3
files.
"""

from __future__ import annotations

import argparse
import csv
import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence

from mutagen.id3 import ID3, ID3NoHeaderError

SUPPORTED_VERSIONS = ("ID3v1", "ID3v1.1", "ID3v2.2", "ID3v2.3", "ID3v2.4")

_FRAME_ID_TO_FIELD = {
    "TIT1": "grouping",
    "TIT2": "title",
    "TIT3": "subtitle",
    "TALB": "album",
    "TPE1": "artist",
    "TPE2": "album_artist",
    "TPE3": "conductor",
    "TPE4": "remixer",
    "TRCK": "track",
    "TPOS": "disc",
    "TDRC": "year",
    "TYER": "year",
    "TDAT": "date",
    "TCON": "genre",
    "COMM": "comment",
    "USLT": "lyrics",
}

_ID3V1_GENRES = [
    "Blues",
    "Classic Rock",
    "Country",
    "Dance",
    "Disco",
    "Funk",
    "Grunge",
    "Hip-Hop",
    "Jazz",
    "Metal",
    "New Age",
    "Oldies",
    "Other",
    "Pop",
    "R&B",
    "Rap",
    "Reggae",
    "Rock",
    "Techno",
    "Industrial",
    "Alternative",
    "Ska",
    "Death Metal",
    "Pranks",
    "Soundtrack",
    "Euro-Techno",
    "Ambient",
    "Trip-Hop",
    "Vocal",
    "Jazz+Funk",
    "Fusion",
    "Trance",
    "Classical",
    "Instrumental",
    "Acid",
    "House",
    "Game",
    "Sound Clip",
    "Gospel",
    "Noise",
    "AlternRock",
    "Bass",
    "Soul",
    "Punk",
    "Space",
    "Meditative",
    "Instrumental Pop",
    "Instrumental Rock",
    "Ethnic",
    "Gothic",
    "Darkwave",
    "Techno-Industrial",
    "Electronic",
    "Pop-Folk",
    "Eurodance",
    "Dream",
    "Southern Rock",
    "Comedy",
    "Cult",
    "Gangsta",
    "Top 40",
    "Christian Rap",
    "Pop/Funk",
    "Jungle",
    "Native American",
    "Cabaret",
    "New Wave",
    "Psychadelic",
    "Rave",
    "Showtunes",
    "Trailer",
    "Lo-Fi",
    "Tribal",
    "Acid Punk",
    "Acid Jazz",
    "Polka",
    "Retro",
    "Musical",
    "Rock & Roll",
    "Hard Rock",
]


@dataclass
class ID3Metadata:
    """Normalised metadata from one MP3 file."""

    file_path: Path
    versions: List[str] = field(default_factory=list)
    tags: Dict[str, str] = field(default_factory=dict)
    raw_frames: Dict[str, List[str]] = field(default_factory=dict)

    def to_dict(self) -> Dict[str, object]:
        """Convert to a JSON-serialisable dictionary."""

        return {
            "file_path": str(self.file_path),
            "versions": self.versions,
            "tags": self.tags,
            "raw_frames": self.raw_frames,
        }


def parse_id3(file_path: Path | str) -> ID3Metadata:
    """Parse ID3 metadata from *file_path*.

    Args:
        file_path: Path to an MP3 file.

    Returns:
        An :class:`ID3Metadata` instance.

    Raises:
        FileNotFoundError: If *file_path* does not exist.
        ValueError: If no ID3 metadata is found.
    """

    path = Path(file_path)
    if not path.exists():
        raise FileNotFoundError(f"{path} does not exist")

    metadata = ID3Metadata(file_path=path)

    _parse_id3v2(path, metadata)
    _parse_id3v1(path, metadata)

    if not metadata.versions:
        raise ValueError(f"No ID3 tags found in {path}")

    # Deduplicate versions while preserving order.
    seen = set()
    ordered_versions = []
    for version in metadata.versions:
        if version not in seen:
            ordered_versions.append(version)
            seen.add(version)
    metadata.versions = ordered_versions

    return metadata


def _parse_id3v2(path: Path, metadata: ID3Metadata) -> None:
    try:
        tag = ID3(path)
    except ID3NoHeaderError:
        return

    version_tuple = getattr(tag, "version", None)
    if version_tuple and len(version_tuple) >= 2:
        version_name = f"ID3v{version_tuple[0]}.{version_tuple[1]}"
        metadata.versions.append(version_name)

    for frame in tag.values():
        frame_id = getattr(frame, "FrameID", None) or getattr(
            frame, "__name__", "Unknown"
        )
        values = _extract_frame_text(frame)
        if not values:
            continue
        metadata.raw_frames.setdefault(frame_id, []).extend(values)

        field = _FRAME_ID_TO_FIELD.get(frame_id)
        if field:
            joined = "; ".join(values)
            metadata.tags[field] = joined


def _parse_id3v1(path: Path, metadata: ID3Metadata) -> None:
    if path.stat().st_size < 128:
        return

    with path.open("rb") as fh:
        fh.seek(-128, 2)
        footer = fh.read(128)

    if len(footer) != 128 or not footer.startswith(b"TAG"):
        return

    title = footer[3:33].rstrip(b"\x00 ").decode("latin-1", errors="replace")
    artist = footer[33:63].rstrip(b"\x00 ").decode("latin-1", errors="replace")
    album = footer[63:93].rstrip(b"\x00 ").decode("latin-1", errors="replace")
    year = footer[93:97].rstrip(b"\x00 ").decode("latin-1", errors="replace")
    comment = footer[97:127]
    genre_index = footer[127]

    track = None
    if comment[-2] == 0:
        track = comment[-1]
        comment = comment[:-2]
    comment_text = comment.rstrip(b"\x00 ").decode("latin-1", errors="replace")

    version = "ID3v1.1" if track else "ID3v1"
    metadata.versions.append(version)

    id3v1_tags = {
        "title": title,
        "artist": artist,
        "album": album,
        "year": year,
        "comment": comment_text,
    }
    if track:
        id3v1_tags["track"] = str(track)

    if genre_index < len(_ID3V1_GENRES):
        id3v1_tags["genre"] = _ID3V1_GENRES[genre_index]
    else:
        id3v1_tags["genre"] = str(genre_index)

    metadata.raw_frames.setdefault("ID3v1", []).append(json.dumps(id3v1_tags))

    for key, value in id3v1_tags.items():
        metadata.tags.setdefault(key, value)


def _extract_frame_text(frame) -> List[str]:
    values: List[str] = []
    if hasattr(frame, "text"):
        texts = frame.text
        if isinstance(texts, (list, tuple)):
            for entry in texts:
                values.append(str(entry))
        else:
            values.append(str(texts))
    elif frame.FrameID == "COMM":
        values.append(str(getattr(frame, "text", "")))
    elif hasattr(frame, "url"):
        values.append(str(frame.url))
    return [value for value in (v.strip() for v in values) if value]


def export_json(metadata: Sequence[ID3Metadata], output_path: Path | str) -> None:
    path = Path(output_path)
    data = [item.to_dict() for item in metadata]
    path.write_text(json.dumps(data, indent=2, ensure_ascii=False), encoding="utf-8")


def export_csv(
    metadata: Sequence[ID3Metadata],
    output_path: Path | str,
    *,
    fields: Optional[Sequence[str]] = None,
) -> None:
    path = Path(output_path)
    if fields is None:
        fields = _collect_all_fields(metadata)

    rows = []
    for item in metadata:
        row = {field: item.tags.get(field, "") for field in fields}
        row["file_path"] = str(item.file_path)
        row["versions"] = ", ".join(item.versions)
        rows.append(row)

    fieldnames = ["file_path", "versions", *fields]

    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(rows)


def _collect_all_fields(metadata: Iterable[ID3Metadata]) -> List[str]:
    fields = []
    for item in metadata:
        for key in item.tags:
            if key not in fields:
                fields.append(key)
    return fields


def _format_for_cli(
    metadata: Sequence[ID3Metadata], fields: Optional[Sequence[str]]
) -> str:
    if not metadata:
        return "No files parsed."

    if fields is None:
        fields = _collect_all_fields(metadata)

    column_widths: Dict[str, int] = {
        "file_path": max(len("File"), *(len(str(item.file_path)) for item in metadata)),
        "versions": max(
            len("Versions"), *(len(", ".join(item.versions)) for item in metadata)
        ),
    }
    for field in fields:
        column_widths[field] = max(
            len(field), *(len(item.tags.get(field, "")) for item in metadata)
        )

    headers = [
        ("File", column_widths["file_path"]),
        ("Versions", column_widths["versions"]),
        *[(field, column_widths[field]) for field in fields],
    ]

    lines = []
    header_line = "  ".join(title.ljust(width) for title, width in headers)
    lines.append(header_line)
    lines.append("  ".join("-" * width for _, width in headers))

    for item in metadata:
        row_values = [
            str(item.file_path).ljust(column_widths["file_path"]),
            ", ".join(item.versions).ljust(column_widths["versions"]),
        ]
        for field in fields:
            row_values.append(item.tags.get(field, "").ljust(column_widths[field]))
        lines.append("  ".join(row_values))

    return "\n".join(lines)


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Inspect ID3 metadata and export results."
    )
    parser.add_argument("files", nargs="+", help="One or more MP3 files to inspect.")
    parser.add_argument(
        "--json", dest="json_path", help="Export parsed tags to this JSON file."
    )
    parser.add_argument(
        "--csv", dest="csv_path", help="Export parsed tags to this CSV file."
    )
    parser.add_argument(
        "--fields",
        nargs="*",
        help="Limit console output to these normalised tag fields (defaults to all discovered fields).",
    )
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    metadata_items: List[ID3Metadata] = []
    for file_arg in args.files:
        try:
            metadata_items.append(parse_id3(file_arg))
        except (FileNotFoundError, ValueError) as exc:
            parser.error(str(exc))

    print(_format_for_cli(metadata_items, args.fields))

    if args.json_path:
        export_json(metadata_items, args.json_path)
    if args.csv_path:
        export_csv(metadata_items, args.csv_path, fields=args.fields)

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
