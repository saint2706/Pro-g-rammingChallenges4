"""Visualization helpers for the educational MBR parser.

This module transforms :class:`MBRParseResult` objects produced by
``mbr.parse_mbr`` into normalized layout descriptions and offers convenience
helpers for rendering the result.  Plotly is used when available for rich,
interactive HTML output while Matplotlib provides a lightweight fallback for
static imagery.  The layout metadata is intentionally JSON serialisable so the
unit tests can assert the geometry without relying on image comparisons.
"""

from __future__ import annotations

import json
import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional

from . import mbr

try:  # Plotly is optional at runtime but available in the project deps.
    import plotly.graph_objects as go
except Exception:  # pragma: no cover - exercised only when plotly missing.
    go = None  # type: ignore[assignment]

try:  # Matplotlib is used both as renderer and as a safe PNG fallback.
    import matplotlib

    matplotlib.use("Agg")  # Ensure headless rendering for CI environments.
    import matplotlib.pyplot as plt
except Exception:  # pragma: no cover - exercised only when matplotlib missing.
    plt = None  # type: ignore[assignment]


@dataclass(slots=True)
class SegmentDescriptor:
    """Describes a contiguous portion of the disk in normalised units."""

    kind: str  # ``"partition"`` or ``"gap"``
    label: str
    start_lba: int
    sectors: int
    start_ratio: float
    end_ratio: float
    length_ratio: float
    bootable: bool
    type_code: str
    type_description: str
    index: Optional[int] = None

    def to_dict(self) -> dict:
        return {
            "kind": self.kind,
            "label": self.label,
            "start_lba": self.start_lba,
            "sectors": self.sectors,
            "start_ratio": self.start_ratio,
            "end_ratio": self.end_ratio,
            "length_ratio": self.length_ratio,
            "bootable": self.bootable,
            "type_code": self.type_code,
            "type_description": self.type_description,
            "index": self.index,
        }


def _normalise_segments(partitions: Iterable[mbr.PartitionEntry]) -> List[SegmentDescriptor]:
    """Convert partition entries into gap/partition descriptors.

    The returned descriptors are sorted by their ``start_lba`` and each entry
    contains the normalised start/end ratios relative to the furthest sector
    touched by any partition.  Unallocated regions preceding partitions are
    explicitly represented as ``gap`` segments to preserve disk geometry.
    """

    sorted_parts = sorted(partitions, key=lambda p: p.start_lba)
    segments: List[SegmentDescriptor] = []
    cursor = 0

    for part in sorted_parts:
        if part.start_lba > cursor:
            gap_sectors = part.start_lba - cursor
            segments.append(
                SegmentDescriptor(
                    kind="gap",
                    label="Unallocated",
                    start_lba=cursor,
                    sectors=gap_sectors,
                    start_ratio=0.0,  # Placeholder, corrected below.
                    end_ratio=0.0,
                    length_ratio=0.0,
                    bootable=False,
                    type_code="0x00",
                    type_description="Unallocated",
                    index=None,
                )
            )
        segments.append(
            SegmentDescriptor(
                kind="partition",
                label=(
                    f"#{part.index} {'(boot)' if part.bootable else ''} "
                    f"{part.type_description}"
                ).strip(),
                start_lba=part.start_lba,
                sectors=part.sectors,
                start_ratio=0.0,  # Placeholder, corrected below.
                end_ratio=0.0,
                length_ratio=0.0,
                bootable=part.bootable,
                type_code=f"0x{part.type_code:02x}",
                type_description=part.type_description,
                index=part.index,
            )
        )
        cursor = max(cursor, part.start_lba + part.sectors)

    if not segments:
        # With no partitions assume a single unit-length gap to keep ratios
        # meaningful for downstream consumers.
        segments.append(
            SegmentDescriptor(
                kind="gap",
                label="Unallocated",
                start_lba=0,
                sectors=1,
                start_ratio=0.0,
                end_ratio=1.0,
                length_ratio=1.0,
                bootable=False,
                type_code="0x00",
                type_description="Unallocated",
                index=None,
            )
        )
        return segments

    total_sectors = max(seg.start_lba + seg.sectors for seg in segments)
    if total_sectors <= 0:
        total_sectors = 1

    for seg in segments:
        seg.start_ratio = seg.start_lba / total_sectors
        seg.end_ratio = (seg.start_lba + seg.sectors) / total_sectors
        seg.length_ratio = seg.sectors / total_sectors

    return segments


def describe_mbr(
    raw: bytes, *, sector_bytes: int = mbr.SECTOR_BYTES_DEFAULT
) -> dict:
    """Parse raw MBR bytes and return serialisable layout metadata."""

    result = mbr.parse_mbr(raw, sector_bytes=sector_bytes)
    segments = _normalise_segments(result.partitions)
    total_sectors = max(seg.start_lba + seg.sectors for seg in segments)

    return {
        "sector_bytes": result.sector_bytes,
        "signature_valid": result.signature_valid,
        "disk_sectors": total_sectors,
        "partitions": len(result.partitions),
        "segments": [seg.to_dict() for seg in segments],
    }


def _ensure_directory(path: Path) -> None:
    if path.parent:
        path.parent.mkdir(parents=True, exist_ok=True)


def _build_plotly_figure(segments: List[SegmentDescriptor], title: str):
    if go is None:
        raise RuntimeError("Plotly is not available")

    fig = go.Figure()
    for seg in segments:
        hover = (
            f"{seg.label}<br>Start LBA: {seg.start_lba:,}<br>Sectors: {seg.sectors:,}"
        )
        fig.add_trace(
            go.Bar(
                x=[seg.length_ratio],
                y=["Disk"],
                base=seg.start_ratio,
                orientation="h",
                name=seg.label,
                text=[seg.label],
                hovertext=[hover],
                hoverinfo="text",
                marker=dict(
                    color="#4caf50" if seg.kind == "partition" else "#e0e0e0",
                ),
            )
        )

    fig.update_layout(
        title=title,
        barmode="stack",
        showlegend=False,
        xaxis=dict(range=[0, 1], tickformat=".0%", title="Disk Space"),
        yaxis=dict(showticklabels=False),
        height=200,
    )
    return fig


def _build_matplotlib_figure(segments: List[SegmentDescriptor], title: str):
    if plt is None:
        raise RuntimeError("Matplotlib is not available")

    fig, ax = plt.subplots(figsize=(8, 1.5))
    for seg in segments:
        ax.barh(
            [0],
            [seg.length_ratio],
            left=seg.start_ratio,
            height=0.6,
            color="#4caf50" if seg.kind == "partition" else "#e0e0e0",
            edgecolor="black",
        )
        ax.text(
            seg.start_ratio + seg.length_ratio / 2,
            0,
            seg.label,
            ha="center",
            va="center",
            fontsize=8,
        )
    ax.set_xlim(0, 1)
    ax.set_ylim(-1, 1)
    ax.set_yticks([])
    ax.set_xticks([0.0, 0.25, 0.5, 0.75, 1.0])
    ax.set_xticklabels(["0%", "25%", "50%", "75%", "100%"])
    ax.set_title(title)
    fig.tight_layout()
    return fig


def visualise_mbr(
    raw: bytes,
    *,
    sector_bytes: int = mbr.SECTOR_BYTES_DEFAULT,
    renderer: str = "plotly",
    title: str = "MBR Partition Layout",
    export_png: Optional[Path] = None,
    export_html: Optional[Path] = None,
    metadata_path: Optional[Path] = None,
) -> dict:
    """Build a visualisation for ``raw`` MBR bytes and return metadata."""

    metadata = describe_mbr(raw, sector_bytes=sector_bytes)
    segments = [
        SegmentDescriptor(
            kind=segment["kind"],
            label=segment["label"],
            start_lba=segment["start_lba"],
            sectors=segment["sectors"],
            start_ratio=segment["start_ratio"],
            end_ratio=segment["end_ratio"],
            length_ratio=segment["length_ratio"],
            bootable=segment["bootable"],
            type_code=segment["type_code"],
            type_description=segment["type_description"],
            index=segment["index"],
        )
        for segment in metadata["segments"]
    ]

    figure = None
    renderer = renderer.lower()
    if renderer == "plotly":
        figure = _build_plotly_figure(segments, title)
    elif renderer in {"mpl", "matplotlib"}:
        figure = _build_matplotlib_figure(segments, title)
    else:
        raise ValueError(f"Unsupported renderer '{renderer}'")

    if export_png:
        export_png = Path(export_png)
        _ensure_directory(export_png)
        if renderer == "plotly":
            try:
                figure.write_image(str(export_png))  # type: ignore[call-arg]
            except Exception:
                # Fallback to Matplotlib when Plotly image export is unavailable.
                mpl_fig = _build_matplotlib_figure(segments, title)
                mpl_fig.savefig(export_png, dpi=150)
        else:
            figure.savefig(export_png, dpi=150)  # type: ignore[call-arg]

    if export_html:
        export_html = Path(export_html)
        _ensure_directory(export_html)
        if renderer == "plotly":
            figure.write_html(str(export_html), include_plotlyjs="cdn")  # type: ignore[call-arg]
        else:
            # Create an inline PNG and embed it into a minimal HTML scaffold.
            from io import BytesIO
            import base64

            buffer = BytesIO()
            figure.savefig(buffer, format="png", dpi=150)  # type: ignore[call-arg]
            encoded = base64.b64encode(buffer.getvalue()).decode("ascii")
            html = (
                "<html><body><img src='data:image/png;base64,"
                + encoded
                + "' alt='MBR layout'></body></html>"
            )
            export_html.write_text(html, encoding="utf-8")

    if metadata_path:
        metadata_path = Path(metadata_path)
        _ensure_directory(metadata_path)
        metadata_path.write_text(json.dumps(metadata, indent=2), encoding="utf-8")

    metadata["renderer"] = renderer
    return metadata


def _build_cli_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Visualise an MBR layout")
    parser.add_argument("file", type=Path, help="Path to a 512-byte MBR image")
    parser.add_argument(
        "--renderer",
        choices=["plotly", "matplotlib"],
        default="plotly",
        help="Rendering backend (default: plotly)",
    )
    parser.add_argument("--png", type=Path, help="Optional PNG output path")
    parser.add_argument("--html", type=Path, help="Optional HTML output path")
    parser.add_argument(
        "--metadata",
        type=Path,
        help="Path to write the JSON segment metadata",
    )
    parser.add_argument(
        "--sector-size",
        type=int,
        default=mbr.SECTOR_BYTES_DEFAULT,
        help="Logical sector size in bytes (default: 512)",
    )
    return parser


def _main(argv: Optional[Iterable[str]] = None) -> int:
    parser = _build_cli_parser()
    args = parser.parse_args(argv)

    raw = mbr.read_mbr(str(args.file))
    metadata = visualise_mbr(
        raw,
        sector_bytes=args.sector_size,
        renderer=args.renderer,
        export_png=args.png,
        export_html=args.html,
        metadata_path=args.metadata,
    )
    print(json.dumps(metadata, indent=2))
    return 0


__all__ = ["SegmentDescriptor", "describe_mbr", "visualise_mbr"]


if __name__ == "__main__":  # pragma: no cover - CLI passthrough
    raise SystemExit(_main())

