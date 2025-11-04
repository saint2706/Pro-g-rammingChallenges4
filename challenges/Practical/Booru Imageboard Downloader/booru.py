"""booru.py – Concurrent downloader for *booru style imageboards.

Enhancements:
  * Config dataclass for immutable runtime options
  * Extended CLI: timeout, retries, backoff, JSON summary, skip-existing toggle, progress toggle
  * Robust retry with exponential backoff (jitter)
  * Basic content-type and size validation
  * Safe filename sanitization & duplicate collision resolution
  * Thread-pooled downloads with graceful summary & exit codes
  * JSON summary (per-file status + aggregate stats) via --json
  * Structured logging with adjustable verbosity

Supported API shape: endpoint `${base}/index.php?page=dapi&s=post&q=index&json=1`.
Tested primarily with gelbooru-like responses.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import logging
import os
import random
import re
import time
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import List, Optional, Dict, Any, Tuple

import requests

try:  # progress bar is optional if --no-progress used
    from tqdm import tqdm  # type: ignore
except ImportError:  # pragma: no cover
    tqdm = None  # fallback; we will guard usage

# ----------------------------- Data Model ----------------------------- #


@dataclass(slots=True)
class Config:
    base_url: str
    tags: str
    limit: int
    out_dir: Path
    workers: int
    timeout: float
    retries: int
    backoff: float
    json_path: Optional[Path]
    skip_existing: bool
    progress: bool
    verbose: bool
    min_size: int  # bytes; ignore if <=0


@dataclass(slots=True)
class DownloadResult:
    url: str
    filename: str
    status: str  # 'downloaded' | 'skipped' | 'failed'
    bytes: int
    error: Optional[str] = None


# ----------------------------- Utilities ----------------------------- #

FILENAME_SAFE = re.compile(r"[^A-Za-z0-9._-]+")


def sanitize_filename(name: str) -> str:
    base, *rest = name.split("?")  # strip query strings if present
    base = os.path.basename(base)
    return FILENAME_SAFE.sub("_", base)[:255]


def backoff_sleep(attempt: int, base: float) -> None:
    # exponential backoff with jitter
    delay = base * (2 ** (attempt - 1))
    delay = delay + random.uniform(0, delay * 0.25)
    time.sleep(min(delay, 30))


# ----------------------------- Core Class ----------------------------- #


class BooruDownloader:
    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.session = requests.Session()
        self.session.headers.update(
            {"User-Agent": "BooruDownloader/2.0 (+https://example.local)"}
        )

    # --- API Fetch ---
    def get_posts(self) -> List[Dict[str, Any]]:
        api_url = (
            f"{self.cfg.base_url.rstrip('/')}/index.php?page=dapi&s=post&q=index&json=1"
        )
        params = {"tags": self.cfg.tags, "limit": self.cfg.limit}
        if self.cfg.verbose:
            logging.info(f"Fetching posts: {api_url} params={params}")
        try:
            resp = self.session.get(api_url, params=params, timeout=self.cfg.timeout)
            resp.raise_for_status()
            data = resp.json()
        except requests.RequestException as e:
            logging.error(f"API request failed: {e}")
            return []
        except json.JSONDecodeError:
            logging.error("API response not JSON; incompatible endpoint")
            return []

        posts = data if isinstance(data, list) else data.get("post", [])
        if not isinstance(posts, list):
            logging.error("Unexpected posts structure")
            return []
        return posts

    def extract_urls(self, posts: List[Dict[str, Any]]) -> List[str]:
        urls = []
        for p in posts:
            url = p.get("file_url") or p.get("sample_url") or p.get("source")
            if url:
                urls.append(url)
        if self.cfg.verbose:
            logging.info(f"Collected {len(urls)} candidate URLs")
        return urls[: self.cfg.limit]

    # --- Download ---
    def download_one(self, url: str) -> DownloadResult:
        safe_name = sanitize_filename(url.split("/")[-1] or "file")
        target_path = self.cfg.out_dir / safe_name

        if target_path.exists() and self.cfg.skip_existing:
            return DownloadResult(
                url=url, filename=safe_name, status="skipped", bytes=0
            )

        # Resolve collisions if not skipping
        if target_path.exists() and not self.cfg.skip_existing:
            stem = target_path.stem
            suffix = target_path.suffix
            for i in range(1, 10_000):
                alt = self.cfg.out_dir / f"{stem}_{i}{suffix}"
                if not alt.exists():
                    target_path = alt
                    safe_name = alt.name
                    break

        last_exc: Optional[str] = None
        for attempt in range(1, self.cfg.retries + 1):
            try:
                resp = self.session.get(url, stream=True, timeout=self.cfg.timeout)
                resp.raise_for_status()
                ctype = resp.headers.get("Content-Type", "")
                if ("html" in ctype.lower()) and self.cfg.verbose:
                    logging.warning(
                        f"Content type looks like HTML (maybe error page): {url}"
                    )
                total_bytes = 0
                with open(target_path, "wb") as fh:
                    for chunk in resp.iter_content(chunk_size=8192):
                        if not chunk:
                            continue
                        fh.write(chunk)
                        total_bytes += len(chunk)
                if self.cfg.min_size > 0 and total_bytes < self.cfg.min_size:
                    if self.cfg.verbose:
                        logging.warning(
                            f"File {safe_name} below min-size threshold; deleting"
                        )
                    try:
                        os.remove(target_path)
                    except OSError:
                        pass
                    return DownloadResult(
                        url=url,
                        filename=safe_name,
                        status="failed",
                        bytes=total_bytes,
                        error="too_small",
                    )
                return DownloadResult(
                    url=url, filename=safe_name, status="downloaded", bytes=total_bytes
                )
            except requests.RequestException as e:
                last_exc = str(e)
                if attempt < self.cfg.retries:
                    if self.cfg.verbose:
                        logging.info(
                            f"Retry {attempt}/{self.cfg.retries} for {safe_name}: {e}"
                        )
                    backoff_sleep(attempt, self.cfg.backoff)
                else:
                    return DownloadResult(
                        url=url,
                        filename=safe_name,
                        status="failed",
                        bytes=0,
                        error=last_exc,
                    )
        return DownloadResult(
            url=url,
            filename=safe_name,
            status="failed",
            bytes=0,
            error=last_exc or "unknown",
        )

    # --- Orchestrate ---
    def run(self) -> Tuple[List[DownloadResult], Dict[str, Any]]:
        self.cfg.out_dir.mkdir(parents=True, exist_ok=True)
        posts = self.get_posts()
        if not posts:
            logging.warning("No posts found.")
            return [], {"downloaded": 0, "failed": 0, "skipped": 0, "total": 0}
        urls = self.extract_urls(posts)
        if not urls:
            logging.warning("No URLs extracted from posts.")
            return [], {"downloaded": 0, "failed": 0, "skipped": 0, "total": 0}

        iterator = urls
        if self.cfg.progress and tqdm is not None:
            iterator = tqdm(urls, desc="Downloading", unit="img")

        results: List[DownloadResult] = []
        with concurrent.futures.ThreadPoolExecutor(
            max_workers=self.cfg.workers
        ) as pool:
            fut_to_url = {pool.submit(self.download_one, u): u for u in urls}
            for fut in concurrent.futures.as_completed(fut_to_url):
                res = fut.result()
                results.append(res)
                if isinstance(iterator, list) and self.cfg.verbose:
                    logging.info(f"{res.status}: {res.filename}")
                if tqdm is not None and not isinstance(iterator, list):
                    # If using tqdm we can update when each finishes (already tied to len)
                    pass

        # Aggregate
        counts = {"downloaded": 0, "failed": 0, "skipped": 0}
        total_bytes = 0
        for r in results:
            counts[r.status] += 1
            total_bytes += r.bytes
        summary = {**counts, "total": len(results), "bytes": total_bytes}
        return results, summary


# ----------------------------- CLI / Main ----------------------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Download images from a booru-style imageboard."
    )
    p.add_argument("tags", help="Tags to search for (quote them if including spaces).")
    p.add_argument(
        "-l",
        "--limit",
        type=int,
        default=100,
        help="Maximum number of images to consider",
    )
    p.add_argument("-d", "--out", default="downloads", help="Output directory")
    p.add_argument("-u", "--url", default="https://gelbooru.com", help="Base booru URL")
    p.add_argument(
        "-w", "--workers", type=int, default=10, help="Concurrent download threads"
    )
    p.add_argument(
        "--timeout", type=float, default=15, help="Per-request timeout seconds"
    )
    p.add_argument(
        "--retries", type=int, default=3, help="Retry attempts for each file"
    )
    p.add_argument(
        "--backoff",
        type=float,
        default=1.0,
        help="Base backoff seconds for retries (exponential)",
    )
    p.add_argument("--json", type=Path, help="Write JSON summary to path")
    p.add_argument(
        "--skip-existing",
        action="store_true",
        help="Skip downloads if file already exists",
    )
    p.add_argument(
        "--no-progress",
        action="store_true",
        help="Disable progress bar even if tqdm available",
    )
    p.add_argument(
        "--min-size",
        type=int,
        default=0,
        help="Minimum acceptable file size in bytes (0 to disable)",
    )
    p.add_argument("-v", "--verbose", action="store_true", help="Verbose logging")
    return p


def parse_args(argv: Optional[List[str]]) -> Config:
    parser = build_parser()
    args = parser.parse_args(argv)
    if args.limit <= 0:
        parser.error("--limit must be positive")
    if args.workers <= 0:
        parser.error("--workers must be positive")
    if args.retries < 1:
        parser.error("--retries must be >= 1")
    if args.timeout <= 0:
        parser.error("--timeout must be > 0")
    out_dir = Path(args.out)
    return Config(
        base_url=args.url,
        tags=args.tags,
        limit=args.limit,
        out_dir=out_dir,
        workers=args.workers,
        timeout=args.timeout,
        retries=args.retries,
        backoff=args.backoff,
        json_path=args.json,
        skip_existing=args.skip_existing,
        progress=not args.no_progress,
        verbose=args.verbose,
        min_size=args.min_size,
    )


def main(argv: Optional[List[str]] = None) -> int:
    cfg = parse_args(argv)
    logging.getLogger().setLevel(logging.DEBUG if cfg.verbose else logging.INFO)
    dl = BooruDownloader(cfg)
    results, summary = dl.run()

    # Print summary
    logging.info("--- Download Summary ---")
    logging.info(json.dumps(summary, indent=2))

    if cfg.json_path:
        payload = {
            "config": asdict(cfg),
            "summary": summary,
            "results": [asdict(r) for r in results],
        }
        try:
            cfg.json_path.write_text(json.dumps(payload, indent=2), encoding="utf-8")
            logging.info(f"Wrote JSON summary: {cfg.json_path}")
        except OSError as e:  # pragma: no cover
            logging.error(f"Failed to write JSON summary: {e}")

    if summary.get("failed", 0) and summary["failed"] == summary["total"]:
        return 2  # total failure
    if summary.get("failed", 0) > 0:
        return 1  # partial failure
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
