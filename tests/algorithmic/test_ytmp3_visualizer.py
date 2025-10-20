from __future__ import annotations

import json
from datetime import datetime, timedelta
from pathlib import Path

import pytest

from challenges.Algorithmic.ytmp3 import download_visualizer as viz


@pytest.fixture()
def sample_summary() -> dict:
    start = datetime(2024, 1, 1, 12, 0, 0)
    return {
        "backend": "yt-dlp",
        "format": "mp3",
        "quality": "192",
        "total": 3,
        "success": 2,
        "failed": 1,
        "items": [
            {
                "url": "https://youtu.be/ok1",
                "success": True,
                "started_at": (start).isoformat(),
                "finished_at": (start + timedelta(seconds=45)).isoformat(),
                "bitrate": 192,
                "error": None,
            },
            {
                "url": "https://youtu.be/ok2",
                "success": True,
                "started_at": (start + timedelta(minutes=1)).isoformat(),
                "finished_at": (start + timedelta(minutes=2)).isoformat(),
                "bitrate": 160,
                "error": None,
            },
            {
                "url": "https://youtu.be/fail",
                "success": False,
                "started_at": (start + timedelta(minutes=2, seconds=30)).isoformat(),
                "finished_at": (start + timedelta(minutes=3)).isoformat(),
                "bitrate": None,
                "error": "network",
            },
        ],
    }


def test_compute_metrics(sample_summary: dict) -> None:
    metrics = viz.compute_metrics(sample_summary)
    assert pytest.approx(metrics["success_ratio"], rel=1e-6) == 2 / 3
    assert metrics["bitrate_distribution"] == {192: 1, 160: 1}
    assert metrics["durations_seconds"] == pytest.approx([45.0, 60.0, 30.0])
    timeline = metrics["timeline"]
    assert len(timeline) == 3
    assert timeline[0]["url"] == "https://youtu.be/ok1"
    assert timeline[2]["error"] == "network"


def test_export_visualisation(tmp_path: Path, sample_summary: dict) -> None:
    summary_path = tmp_path / "summary.json"
    summary_path.write_text(json.dumps(sample_summary), encoding="utf-8")

    report = viz.export_visualisation(summary_path, tmp_path, prefix="sample")

    metrics_path = Path(report["metrics_path"])  # type: ignore[index]
    assert metrics_path.exists()
    metrics_data = json.loads(metrics_path.read_text(encoding="utf-8"))
    assert pytest.approx(metrics_data["success_ratio"], rel=1e-6) == 2 / 3

    assert Path(report["html_path"]).exists()  # type: ignore[index]
    png_paths = report["png_paths"]  # type: ignore[index]
    assert set(png_paths.keys()) == {"status", "durations", "timeline"}
    for path in png_paths.values():
        file = Path(path)
        assert file.exists()
        assert file.stat().st_size > 0
