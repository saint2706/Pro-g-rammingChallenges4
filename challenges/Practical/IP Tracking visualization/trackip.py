"""trackip.py - IP geolocation visualization tool

Features Added / Modernization:
  * Dataclasses for Config & Location entries
  * Concurrency with ThreadPoolExecutor for faster lookups
  * Retry logic with exponential backoff & jitter per IP
  * Optional progress bar (disable with --no-progress)
  * Input via command line IPs or file (--file)
  * JSON & CSV export of raw location data
  * Summary stats (success, failures, unique countries)
  * Exit codes: 0 success (>=1 success), 1 no successes, 2 usage/config error
  * Adjustable timeout, retries, backoff base, and worker pool size
  * Graceful handling of rate limits / HTTP errors
  * Structured map generation separated from data fetch

Examples:
  python trackip.py 8.8.8.8 1.1.1.1 -o map.html
  python trackip.py --file ips.txt --json results.json --csv results.csv --summary
  python trackip.py 8.8.8.8 --retry 5 --backoff 0.8 --timeout 8 --max-workers 10
"""

from __future__ import annotations

import argparse
import csv
import io
import json
import random
import sys
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path
from threading import Lock
from typing import Any, Dict, List, Optional, Sequence

import requests
import pandas as pd
import plotly.graph_objects as go

try:
    from tqdm import tqdm
except ImportError:  # pragma: no cover
    tqdm = None  # type: ignore

API_URL_TEMPLATE = "https://ipapi.co/{ip}/json/"

_CACHE_LOCK = Lock()
_FETCH_CACHE: Dict[str, Optional["Location"]] = {}

# ----------------------------- Data Models ----------------------------- #


@dataclass(slots=True)
class Location:
    ip: str
    latitude: float
    longitude: float
    country: str
    city: str
    org: str

    def as_dict(self) -> Dict[str, Any]:
        return {
            "IP": self.ip,
            "Latitude": self.latitude,
            "Longitude": self.longitude,
            "Country": self.country,
            "City": self.city,
            "Org": self.org,
        }


@dataclass(slots=True)
class Config:
    ips: List[str]
    file: Optional[Path]
    output_html: Path
    json_path: Optional[Path]
    csv_path: Optional[Path]
    timeout: float
    retries: int
    backoff: float
    max_workers: int
    show_progress: bool
    summary: bool

    def validate(self) -> None:
        if not self.ips and not self.file:
            raise ValueError("Provide IPs or --file")
        if self.retries < 0:
            raise ValueError("--retry must be >= 0")
        if self.max_workers < 1:
            raise ValueError("--max-workers must be >= 1")
        if self.timeout <= 0:
            raise ValueError("--timeout must be > 0")


# ----------------------------- Fetch Logic ----------------------------- #


def exponential_backoff(base: float, attempt: int) -> float:
    return base * (2**attempt) + random.uniform(0, base)


def _fetch_single_uncached(ip: str, cfg: Config) -> Optional[Location]:
    url = API_URL_TEMPLATE.format(ip=ip)
    attempt = 0
    while attempt <= cfg.retries:
        try:
            resp = requests.get(url, timeout=cfg.timeout)
            if resp.status_code == 429:  # rate limit
                attempt += 1
                if attempt > cfg.retries:
                    return None
                time.sleep(min(exponential_backoff(cfg.backoff, attempt), 30))
                continue
            resp.raise_for_status()
            data = resp.json()
            if data.get("error"):
                return None
            if not all(k in data for k in ("latitude", "longitude", "country_name")):
                return None
            return Location(
                ip=ip,
                latitude=data["latitude"],
                longitude=data["longitude"],
                country=data.get("country_name", "N/A"),
                city=data.get("city", "N/A") or "N/A",
                org=data.get("org", "N/A") or "N/A",
            )
        except requests.RequestException:
            attempt += 1
            if attempt > cfg.retries:
                return None
            time.sleep(min(exponential_backoff(cfg.backoff, attempt), 30))
        except Exception:
            return None
    return None


def fetch_single(ip: str, cfg: Config) -> Optional[Location]:
    with _CACHE_LOCK:
        if ip in _FETCH_CACHE:
            return _FETCH_CACHE[ip]

    result = _fetch_single_uncached(ip, cfg)

    with _CACHE_LOCK:
        _FETCH_CACHE[ip] = result

    return result


def fetch_locations(ips: Sequence[str], cfg: Config) -> List[Location]:
    locations: List[Location] = []
    iterable = ips
    if cfg.show_progress and tqdm:
        progress = tqdm(total=len(ips), desc="Fetching IP data")
    else:
        progress = None
    with ThreadPoolExecutor(max_workers=cfg.max_workers) as pool:
        future_map = {pool.submit(fetch_single, ip, cfg): ip for ip in iterable}
        for fut in as_completed(future_map):
            loc = fut.result()
            if loc:
                locations.append(loc)
            if progress:
                progress.update(1)
    if progress:
        progress.close()
    return locations


# ----------------------------- Map Creation ----------------------------- #


def build_map_figure(locations: Sequence[Location]) -> go.Figure:
    df = pd.DataFrame([l.as_dict() for l in locations])
    fig = go.Figure(
        data=go.Scattergeo(
            lon=df["Longitude"],
            lat=df["Latitude"],
            text=df["IP"]
            + "<br>"
            + df["City"]
            + ", "
            + df["Country"]
            + "<br>"
            + df["Org"],
            mode="markers",
            marker=dict(
                size=8,
                opacity=0.8,
                symbol="circle",
                line=dict(width=1, color="rgba(102,102,102)"),
                color="blue",
            ),
        )
    )
    fig.update_layout(
        title="IP Address Geographic Locations",
        geo=dict(
            scope="world",
            projection_type="natural earth",
            showland=True,
            landcolor="rgb(243,243,243)",
            countrycolor="rgb(204,204,204)",
        ),
    )
    return fig


def create_map(locations: List[Location], output_path: Path) -> None:
    if not locations:
        print("No valid location data to plot.")
        return
    fig = build_map_figure(locations)
    try:
        fig.write_html(output_path)
        print(f"Map saved to '{output_path.resolve()}'")
    except Exception as e:
        print(f"Error saving map HTML: {e}", file=sys.stderr)


# ----------------------------- Export Helpers ----------------------------- #


def export_json(
    locations: Sequence[Location], path: Optional[Path], meta: Dict[str, Any]
) -> str:
    data = [l.as_dict() for l in locations]
    payload = json.dumps({"locations": data, "meta": meta}, indent=2)
    if path:
        try:
            with open(path, "w", encoding="utf-8") as fh:
                fh.write(payload)
            print(f"JSON data written to {path}")
        except OSError as e:
            print(f"Failed to write JSON: {e}", file=sys.stderr)
    return payload


def export_csv(locations: Sequence[Location], path: Optional[Path]) -> str:
    output = io.StringIO()
    writer = csv.DictWriter(
        output, fieldnames=["IP", "Latitude", "Longitude", "Country", "City", "Org"]
    )
    writer.writeheader()
    for l in locations:
        writer.writerow(l.as_dict())
    contents = output.getvalue()
    if path:
        try:
            with open(path, "w", newline="", encoding="utf-8") as fh:
                fh.write(contents)
            print(f"CSV data written to {path}")
        except OSError as e:
            print(f"Failed to write CSV: {e}", file=sys.stderr)
    return contents


def summarize_results(
    requested_ips: Sequence[str], locations: Sequence[Location], elapsed: float
) -> Dict[str, Any]:
    failures = len(requested_ips) - len(locations)
    return {
        "requested": len(requested_ips),
        "succeeded": len(locations),
        "failed": failures,
        "countries": sorted({l.country for l in locations}),
        "elapsed_sec": round(elapsed, 3),
        "output_html": None,
    }


# ----------------------------- CLI ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Visualize geographic location of IP addresses on a world map."
    )
    p.add_argument("ips", nargs="*", help="IP addresses to track")
    p.add_argument("--file", type=Path, help="File containing IPs (one per line)")
    p.add_argument(
        "-o",
        "--output",
        dest="output",
        default="ip_map.html",
        help="Output HTML file (default ip_map.html)",
    )
    p.add_argument("--json", type=Path, help="Write JSON data to path")
    p.add_argument("--csv", type=Path, help="Write CSV data to path")
    p.add_argument(
        "--timeout", type=float, default=5.0, help="Request timeout seconds (default 5)"
    )
    p.add_argument(
        "--retry", type=int, default=2, help="Retries per IP on failure (default 2)"
    )
    p.add_argument(
        "--backoff", type=float, default=0.6, help="Base backoff seconds (default 0.6)"
    )
    p.add_argument(
        "--max-workers",
        type=int,
        default=8,
        help="Maximum concurrent requests (default 8)",
    )
    p.add_argument("--no-progress", action="store_true", help="Disable progress bar")
    p.add_argument("--summary", action="store_true", help="Print summary statistics")
    return p


def parse_args(argv: Optional[Sequence[str]]) -> Config:
    parser = build_parser()
    a = parser.parse_args(argv)
    ips: List[str] = list(dict.fromkeys(a.ips))  # remove duplicates preserve order
    if a.file:
        try:
            with open(a.file, "r", encoding="utf-8") as fh:
                for line in fh:
                    ip = line.strip()
                    if ip and ip not in ips:
                        ips.append(ip)
        except OSError as e:
            print(f"Error reading file: {e}", file=sys.stderr)
            raise SystemExit(2)
    cfg = Config(
        ips=ips,
        file=a.file,
        output_html=Path(a.output),
        json_path=a.json,
        csv_path=a.csv,
        timeout=a.timeout,
        retries=a.retry,
        backoff=a.backoff,
        max_workers=a.max_workers,
        show_progress=not a.no_progress,
        summary=a.summary,
    )
    try:
        cfg.validate()
    except ValueError as e:
        print(f"Argument error: {e}", file=sys.stderr)
        raise SystemExit(2)
    return cfg


# ----------------------------- Orchestration ----------------------------- #


def main(argv: Optional[Sequence[str]] = None) -> int:
    cfg = parse_args(argv)
    if not cfg.ips:
        print("No IPs provided after processing input.", file=sys.stderr)
        return 2
    start = time.time()
    locations = fetch_locations(cfg.ips, cfg)
    elapsed = time.time() - start

    if locations:
        create_map(locations, cfg.output_html)
    else:
        print("No successful IP lookups; map not created.")

    # Summary & exports
    meta = summarize_results(cfg.ips, locations, elapsed)
    if locations:
        meta["output_html"] = str(cfg.output_html)
    if cfg.summary:
        print("\nSummary:")
        for k, v in meta.items():
            print(f"  {k}: {v}")

    if cfg.json_path:
        export_json(locations, cfg.json_path, meta)
    if cfg.csv_path:
        export_csv(locations, cfg.csv_path)

    if not locations:
        return 1
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
