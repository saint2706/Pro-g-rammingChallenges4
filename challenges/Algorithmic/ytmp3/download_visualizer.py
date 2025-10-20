"""Utilities for visualising ``cringe.batch_download`` summaries.

The downloader already exposes a JSON summary that captures per-item
success/failure metadata.  This module provides tooling to take that output
and compute richer statistics (timelines, success ratios, bitrate
distributions) and to turn them into charts without requiring an interactive
display.

Example
-------
    python download_visualizer.py summary.json --output-dir reports/

The command above will:

* load ``summary.json`` (as produced by ``cringe.batch_download``),
* compute aggregate metrics which are also persisted as JSON for tests, and
* render a status bar chart, a duration line chart, and a per-item timeline as
  PNGs alongside a small self-contained HTML report embedding those images.

The functions exposed here are intentionally import-friendly so unit tests can
exercise the metric calculation logic without hitting the filesystem or
matplotlib.
"""

from __future__ import annotations

import argparse
import base64
import json
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from io import BytesIO
from pathlib import Path
from typing import Dict, Iterable, List, MutableMapping, Optional, Sequence, Tuple, Union

import matplotlib

# Force a headless backend so tests/CI can run without an X server.
matplotlib.use("Agg")

import matplotlib.pyplot as plt


SummaryLike = Union[str, Path, MutableMapping[str, object]]


# ---------------------------------------------------------------------------
# Data models
# ---------------------------------------------------------------------------


@dataclass
class TimelineEntry:
    """A single download's timing information."""

    url: str
    success: bool
    start: datetime
    end: datetime
    duration: timedelta
    bitrate: Optional[int]
    error: Optional[str]

    def to_json(self) -> Dict[str, object]:
        return {
            "url": self.url,
            "success": self.success,
            "start": self.start.isoformat(),
            "end": self.end.isoformat(),
            "duration_seconds": self.duration.total_seconds(),
            "bitrate": self.bitrate,
            "error": self.error,
        }


# ---------------------------------------------------------------------------
# Loading helpers
# ---------------------------------------------------------------------------


def _parse_timestamp(value: Optional[str]) -> Optional[datetime]:
    if not value:
        return None
    try:
        return datetime.fromisoformat(value)
    except ValueError:
        if value.endswith("Z"):
            # Support RFC3339 style timestamps.
            try:
                return datetime.fromisoformat(value[:-1])
            except ValueError:
                pass
        raise


def _coerce_summary(data: SummaryLike) -> MutableMapping[str, object]:
    if isinstance(data, MutableMapping):
        return data
    if isinstance(data, (str, Path)):
        text = Path(data).read_text(encoding="utf-8")
        return json.loads(text)
    raise TypeError(f"Unsupported summary input: {type(data)!r}")


# ---------------------------------------------------------------------------
# Metric computation
# ---------------------------------------------------------------------------


def compute_metrics(summary: SummaryLike) -> Dict[str, object]:
    """Compute high-level metrics from a batch download summary.

    Parameters
    ----------
    summary:
        Mapping or path to the JSON result of ``batch_download``.

    Returns
    -------
    dict
        Contains aggregate totals, success ratio, bitrate distribution, and
        serialised timeline entries suitable for writing to disk as JSON.
    """

    data = _coerce_summary(summary)
    items: Sequence[MutableMapping[str, object]] = data.get("items", [])  # type: ignore[assignment]
    total = int(data.get("total", len(items)))
    success = int(data.get("success", sum(1 for item in items if item.get("success"))))
    failed = int(data.get("failed", total - success))

    # Construct timeline entries, gracefully handling missing timestamps by
    # falling back to synthetic sequential slots.
    now = datetime.now(tz=UTC)
    default_increment = timedelta(seconds=30)
    current = None
    timeline: List[TimelineEntry] = []
    durations: List[float] = []
    bitrate_counter: Dict[int, int] = {}

    for index, item in enumerate(items):
        url = str(item.get("url", f"item-{index}"))
        ok = bool(item.get("success", False))
        bitrate_value: Optional[int] = None
        if "bitrate" in item and item["bitrate"] is not None:
            try:
                bitrate_value = int(item["bitrate"])  # type: ignore[arg-type]
            except (TypeError, ValueError):
                bitrate_value = None

        start = _parse_timestamp(item.get("started_at") if isinstance(item, dict) else None)  # type: ignore[arg-type]
        end = _parse_timestamp(item.get("finished_at") if isinstance(item, dict) else None)  # type: ignore[arg-type]

        if start is None:
            if current is None:
                current = now
            else:
                current = current + default_increment
            start = current
        if end is None:
            duration_seconds = item.get("duration") if isinstance(item, dict) else None  # type: ignore[arg-type]
            try:
                seconds = float(duration_seconds) if duration_seconds is not None else default_increment.total_seconds()
            except (TypeError, ValueError):
                seconds = default_increment.total_seconds()
            end = start + timedelta(seconds=seconds)
        duration = end - start
        durations.append(duration.total_seconds())
        if bitrate_value is not None:
            bitrate_counter[bitrate_value] = bitrate_counter.get(bitrate_value, 0) + 1
        timeline.append(
            TimelineEntry(
                url=url,
                success=ok,
                start=start,
                end=end,
                duration=duration,
                bitrate=bitrate_value,
                error=item.get("error") if isinstance(item, dict) else None,  # type: ignore[arg-type]
            )
        )

    overall = {
        "backend": data.get("backend"),
        "format": data.get("format"),
        "quality": data.get("quality"),
        "total": total,
        "success": success,
        "failed": failed,
        "success_ratio": float(success / total) if total else 0.0,
        "timeline": [entry.to_json() for entry in timeline],
        "durations_seconds": durations,
        "bitrate_distribution": bitrate_counter,
    }
    if timeline:
        overall["started_at"] = min(entry.start for entry in timeline).isoformat()
        overall["finished_at"] = max(entry.end for entry in timeline).isoformat()
    return overall


# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------


def _build_status_chart(metrics: Dict[str, object]) -> plt.Figure:
    fig, ax = plt.subplots(figsize=(4, 3))
    counts = [metrics.get("success", 0), metrics.get("failed", 0)]
    labels = ["Success", "Failed"]
    ax.bar(labels, counts, color=["#4caf50", "#f44336"])
    ax.set_ylabel("Downloads")
    ax.set_title("Download Outcomes")
    for idx, value in enumerate(counts):
        ax.text(idx, value, f"{int(value)}", ha="center", va="bottom")
    fig.tight_layout()
    return fig


def _build_duration_chart(metrics: Dict[str, object]) -> plt.Figure:
    fig, ax = plt.subplots(figsize=(5, 3))
    durations = metrics.get("durations_seconds", [])
    xs = list(range(1, len(durations) + 1))
    ax.plot(xs, durations, marker="o", color="#2196f3")
    ax.set_xlabel("Item Index")
    ax.set_ylabel("Duration (s)")
    ax.set_title("Download Durations")
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    return fig


def _build_timeline_chart(metrics: Dict[str, object]) -> plt.Figure:
    fig, ax = plt.subplots(figsize=(6, 4))
    timeline_data = metrics.get("timeline", [])
    if not timeline_data:
        ax.set_axis_off()
        ax.set_title("Timeline (no data)")
        return fig

    start_times = [datetime.fromisoformat(entry["start"]) for entry in timeline_data]
    end_times = [datetime.fromisoformat(entry["end"]) for entry in timeline_data]
    durations = [end - start for start, end in zip(start_times, end_times)]
    labels = [entry.get("url", "") for entry in timeline_data]

    base = min(start_times)
    starts = [(start - base).total_seconds() for start in start_times]
    length = [duration.total_seconds() for duration in durations]

    ax.barh(range(len(labels)), length, left=starts, color="#9c27b0", alpha=0.7)
    ax.set_yticks(range(len(labels)))
    ax.set_yticklabels(labels)
    ax.set_xlabel("Seconds since first start")
    ax.set_title("Per-item Timeline")
    fig.tight_layout()
    return fig


def build_figures(metrics: Dict[str, object]) -> Dict[str, plt.Figure]:
    return {
        "status": _build_status_chart(metrics),
        "durations": _build_duration_chart(metrics),
        "timeline": _build_timeline_chart(metrics),
    }


# ---------------------------------------------------------------------------
# Export helpers
# ---------------------------------------------------------------------------


def _figure_to_png_bytes(fig: plt.Figure) -> bytes:
    with BytesIO() as buffer:
        fig.savefig(buffer, format="png", bbox_inches="tight")
        return buffer.getvalue()


def export_visualisation(
    summary: SummaryLike,
    output_dir: Union[str, Path],
    *,
    prefix: str = "ytmp3",
    emit_png: bool = True,
    emit_html: bool = True,
    emit_metrics: bool = True,
) -> Dict[str, object]:
    """Generate charts and write them (optionally) to the given directory."""

    metrics = compute_metrics(summary)
    figures = build_figures(metrics)

    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    png_paths: Dict[str, str] = {}
    if emit_png:
        for name, figure in figures.items():
            png_bytes = _figure_to_png_bytes(figure)
            png_file = output_path / f"{prefix}_{name}.png"
            png_file.write_bytes(png_bytes)
            png_paths[name] = str(png_file)

    metrics_path: Optional[str] = None
    if emit_metrics:
        metrics_file = output_path / f"{prefix}_metrics.json"
        metrics_file.write_text(json.dumps(metrics, indent=2), encoding="utf-8")
        metrics_path = str(metrics_file)

    html_path: Optional[str] = None
    if emit_html:
        parts = [
            "<html><head><meta charset='utf-8'><title>ytmp3 Report</title>",
            "<style>body{font-family:sans-serif;margin:2rem;}h2{margin-top:2rem;}img{max-width:100%;}</style>",
            "</head><body>",
            "<h1>ytmp3 Batch Download Report</h1>",
            f"<p>Total: {metrics['total']} Success: {metrics['success']} Failed: {metrics['failed']} (ratio {metrics['success_ratio']:.2f})</p>",
        ]
        for name, figure in figures.items():
            png_bytes = _figure_to_png_bytes(figure)
            encoded = base64.b64encode(png_bytes).decode("ascii")
            parts.append(f"<h2>{name.title()}</h2>")
            parts.append(f"<img alt='{name}' src='data:image/png;base64,{encoded}'/>")
        parts.append("</body></html>")
        html_content = "".join(parts)
        html_file = output_path / f"{prefix}_report.html"
        html_file.write_text(html_content, encoding="utf-8")
        html_path = str(html_file)

    return {
        "metrics": metrics,
        "figures": list(figures.keys()),
        "png_paths": png_paths,
        "metrics_path": metrics_path,
        "html_path": html_path,
    }


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Visualise ytmp3 batch results")
    parser.add_argument("summary", help="Path to JSON output from cringe.batch_download")
    parser.add_argument("--output-dir", default="visualiser_output", help="Directory to write reports to")
    parser.add_argument("--prefix", default="ytmp3", help="Prefix for output filenames")
    parser.add_argument("--no-png", action="store_true", help="Skip writing PNG files")
    parser.add_argument("--no-html", action="store_true", help="Skip writing HTML file")
    parser.add_argument("--no-metrics", action="store_true", help="Skip writing metrics JSON")
    return parser


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = _build_parser()
    args = parser.parse_args(argv)
    export_visualisation(
        args.summary,
        args.output_dir,
        prefix=args.prefix,
        emit_png=not args.no_png,
        emit_html=not args.no_html,
        emit_metrics=not args.no_metrics,
    )
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI glue
    raise SystemExit(main())

