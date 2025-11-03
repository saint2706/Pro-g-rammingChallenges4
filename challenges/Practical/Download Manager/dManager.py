"""dManager.py - Robust multi-threaded (or single) file downloader.

Modernization & Features:
  * Argparse CLI (removed click dependency) for consistency with repo
  * Config & Result dataclasses (explicit configuration + structured outcomes)
  * HEAD probe with graceful fallback and range support detection
  * Multi-threaded ranged download using ThreadPoolExecutor
  * Optional resume for partially downloaded files (--resume)
  * Exponential backoff retries per chunk with jitter
  * Hash verification (SHA-256) if expected hash provided
  * JSON summary output for automation / logging
  * Timeout handling and user-friendly error messages
  * Clean separation: probing, planning, downloading, assembly, verification
  * Consistent exit codes: 0 success, 1 recoverable download/hash issue, 2 usage/config error

Usage Examples:
  python dManager.py https://example.com/file.iso -n 8 -o file.iso
  python dManager.py https://example.com/file.zip --resume --sha256 <expected_hash> --json summary.json
  python dManager.py https://example.com/large.bin -n 16 --retry 5 --timeout 20

Exit Codes:
  0 = success (checksum ok if provided)
  1 = download completed but checksum mismatch OR partial failure
  2 = invalid arguments / unrecoverable error (network, permission, etc.)
"""

from __future__ import annotations

import argparse
import concurrent.futures as futures
import contextlib
import hashlib
import json
import math
import os
import random
import sys
import threading
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Optional, List, Tuple, Dict, Any

import requests
from requests import Response

try:
    from tqdm import tqdm
except ImportError:  # pragma: no cover
    print(
        "Error: The 'tqdm' library is required. Install with: pip install tqdm",
        file=sys.stderr,
    )
    raise SystemExit(2)

# --------------------------- Data Models --------------------------- #


@dataclass(slots=True)
class Config:
    url: str
    threads: int
    output: Path
    retries: int
    timeout: float
    backoff: float
    resume: bool
    chunk_size: int
    expected_sha256: Optional[str]
    json_path: Optional[Path]
    verify_tls: bool
    user_agent: str


@dataclass(slots=True)
class Result:
    url: str
    output: str
    size: int
    downloaded: int
    threads: int
    elapsed: float
    resumed: bool
    checksum: Optional[str]
    checksum_match: Optional[bool]
    error: Optional[str]


RANGE_HEADER = "Accept-Ranges"
CONTENT_LENGTH = "Content-Length"
DEFAULT_UA = "ProChallengesDownloader/1.0"
LOCK = threading.Lock()  # for shared progress/state edits if needed


def _resume_metadata_path(target: Path) -> Path:
    return target.with_name(target.name + ".resume.json")


class ResumeState:
    """Track per-part completion and offsets for multi-threaded resume support."""

    def __init__(self, target: Path, size: int, parts: Dict[str, Dict[str, Any]]):
        self.target = target
        self.size = size
        self.meta_path = _resume_metadata_path(target)
        self._parts = parts
        self._lock = threading.Lock()

    @staticmethod
    def _key(part: Tuple[int, int]) -> str:
        return f"{part[0]}-{part[1]}"

    @staticmethod
    def _parse_key(key: str) -> Tuple[int, int]:
        start, end = key.split("-", 1)
        return int(start), int(end)

    @classmethod
    def initialize(
        cls, target: Path, size: int, parts: List[Tuple[int, int]]
    ) -> "ResumeState":
        existing: Dict[str, Dict[str, Any]] = {}
        meta_path = _resume_metadata_path(target)
        if meta_path.exists():
            try:
                with open(meta_path, "r", encoding="utf-8") as fh:
                    data = json.load(fh)
                if data.get("size") == size:
                    for key, value in data.get("parts", {}).items():
                        start, end = cls._parse_key(key)
                        length = max(0, end - start + 1)
                        if isinstance(value, dict):
                            part_entry = {
                                "done": bool(value.get("done", False)),
                                "offset": int(value.get("offset", 0) or 0),
                            }
                        else:
                            part_entry = {"done": bool(value), "offset": 0}
                        if part_entry["offset"] < 0:
                            part_entry["offset"] = 0
                        elif part_entry["offset"] > length:
                            part_entry["offset"] = length
                        existing[key] = part_entry
            except (OSError, ValueError, TypeError):
                existing = {}

        for part in parts:
            existing.setdefault(cls._key(part), {"done": False, "offset": 0})

        state = cls(target, size, existing)
        state._flush()
        return state

    def is_complete(self, part: Tuple[int, int]) -> bool:
        entry = self._parts.get(self._key(part))
        return bool(entry and entry.get("done"))

    def get_offset(self, part: Tuple[int, int]) -> int:
        entry = self._parts.get(self._key(part))
        if not entry:
            return 0
        offset = int(entry.get("offset", 0) or 0)
        start, end = part
        length = max(0, end - start + 1)
        if offset < 0:
            return 0
        return min(offset, length)

    def update_offset(self, part: Tuple[int, int], offset: int) -> None:
        key = self._key(part)
        start, end = part
        length = max(0, end - start + 1)
        clamped = max(0, min(offset, length))
        with self._lock:
            entry = self._parts.setdefault(key, {"done": False, "offset": 0})
            entry["offset"] = clamped
            entry["done"] = False
            self._flush()

    def mark_complete(self, part: Tuple[int, int]) -> None:
        key = self._key(part)
        with self._lock:
            entry = self._parts.setdefault(key, {"done": False, "offset": 0})
            entry["done"] = True
            entry["offset"] = 0
            self._flush()

    def completed_bytes(self) -> int:
        total = 0
        for key, entry in self._parts.items():
            start, end = self._parse_key(key)
            length = end - start + 1
            if entry.get("done"):
                total += length
            else:
                offset = int(entry.get("offset", 0) or 0)
                if offset > 0:
                    total += min(offset, length)
        return total

    def sync_with_file(self) -> None:
        existing_size = self.target.stat().st_size if self.target.exists() else 0
        changed = False
        for key, entry in list(self._parts.items()):
            start, end = self._parse_key(key)
            length = end - start + 1
            if entry.get("done"):
                if end >= existing_size:
                    entry["done"] = False
                    entry["offset"] = max(0, min(existing_size - start, length))
                    changed = True
            else:
                desired = max(0, min(existing_size - start, length))
                current_offset = int(entry.get("offset", 0) or 0)
                if current_offset != desired:
                    entry["offset"] = desired
                    changed = True
        if changed:
            self._flush()

    def cleanup(self) -> None:
        with contextlib.suppress(OSError):
            _resume_metadata_path(self.target).unlink()

    def _flush(self) -> None:
        payload = {"size": self.size, "parts": self._parts}
        try:
            self.meta_path.parent.mkdir(parents=True, exist_ok=True)
            with open(self.meta_path, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, sort_keys=True)
        except OSError:
            pass

# --------------------------- Utility Functions --------------------------- #


def human_size(num: int) -> str:
    if num < 0:
        return "Unknown"
    units = ["B", "KB", "MB", "GB", "TB"]
    i = 0
    n = float(num)
    while n >= 1024 and i < len(units) - 1:
        n /= 1024
        i += 1
    return f"{n:.2f} {units[i]}"


def backoff_sleep(base: float, attempt: int) -> None:
    # Exponential backoff with jitter
    delay = base * (2**attempt) + random.uniform(0, base)
    time.sleep(min(delay, 30))  # cap


# --------------------------- Probing --------------------------- #


def probe(
    url: str, timeout: float, verify: bool, ua: str
) -> Tuple[Optional[int], bool, Dict[str, str]]:
    """Perform a HEAD request to discover size and range support. Fallback to GET if HEAD not allowed."""
    session = requests.Session()
    headers = {"User-Agent": ua}
    try:
        resp = session.head(
            url, allow_redirects=True, timeout=timeout, verify=verify, headers=headers
        )
        if (
            resp.status_code >= 400 or resp.status_code == 405
        ):  # 405 = method not allowed
            # fallback to GET (no body?) with stream to avoid full download
            resp = session.get(
                url,
                stream=True,
                allow_redirects=True,
                timeout=timeout,
                verify=verify,
                headers=headers,
            )
        size = (
            int(resp.headers.get(CONTENT_LENGTH, -1))
            if resp.headers.get(CONTENT_LENGTH)
            else None
        )
        range_ok = "bytes" in resp.headers.get(RANGE_HEADER, "").lower()
        return size, range_ok, dict(resp.headers)
    except requests.RequestException as e:
        print(f"Error probing URL: {e}", file=sys.stderr)
        return None, False, {}


# --------------------------- Download Planning --------------------------- #


def plan_parts(size: int, threads: int) -> List[Tuple[int, int]]:
    if size <= 0:
        return []
    part_size = math.ceil(size / threads)
    parts: List[Tuple[int, int]] = []
    start = 0
    while start < size:
        end = min(start + part_size - 1, size - 1)
        parts.append((start, end))
        start = end + 1
    return parts


# --------------------------- Downloader Core --------------------------- #


def download_part(
    session: requests.Session,
    cfg: Config,
    part: Tuple[int, int],
    pbar: tqdm,
    retries: int,
    timeout: float,
    file_path: Path,
    resume_state: Optional[ResumeState] = None,
) -> int:
    start, end = part
    part_length = end - start + 1
    resume_offset = 0
    if resume_state:
        if resume_state.is_complete(part):
            return 0
        resume_offset = resume_state.get_offset(part)
    attempt = 0
    downloaded = 0
    while attempt <= retries:
        current_start = start + resume_offset
        if current_start > end:
            if resume_state:
                resume_state.mark_complete(part)
            return downloaded
        headers = {
            "Range": f"bytes={current_start}-{end}",
            "User-Agent": cfg.user_agent,
        }
        try:
            with session.get(
                cfg.url,
                headers=headers,
                stream=True,
                timeout=timeout,
                verify=cfg.verify_tls,
            ) as r:
                r.raise_for_status()
                # Write segment at position
                with open(file_path, "r+b") as f:
                    f.seek(current_start)
                    for chunk in r.iter_content(chunk_size=cfg.chunk_size):
                        if not chunk:
                            continue
                        f.write(chunk)
                        downloaded += len(chunk)
                        pbar.update(len(chunk))
                        if resume_state:
                            resume_offset = min(
                                resume_offset + len(chunk), part_length
                            )
                            resume_state.update_offset(part, resume_offset)
                if resume_state:
                    resume_state.mark_complete(part)
                return downloaded
        except requests.RequestException as e:
            attempt += 1
            if attempt > retries:
                print(
                    f"Part {start}-{end} failed after {retries} retries: {e}",
                    file=sys.stderr,
                )
                return downloaded
            if resume_state:
                resume_offset = resume_state.get_offset(part)
            backoff_sleep(cfg.backoff, attempt)
    return downloaded


def single_thread_download(
    session: requests.Session, cfg: Config, size: Optional[int], file_path: Path
) -> int:
    headers = {"User-Agent": cfg.user_agent}
    downloaded = 0
    mode = "ab" if cfg.resume and file_path.exists() else "wb"
    existing = file_path.stat().st_size if file_path.exists() else 0
    if cfg.resume and existing:
        headers["Range"] = f"bytes={existing}-"
        downloaded = existing
    try:
        with session.get(
            cfg.url,
            headers=headers,
            stream=True,
            timeout=cfg.timeout,
            verify=cfg.verify_tls,
        ) as r:
            r.raise_for_status()
            content_length = r.headers.get(CONTENT_LENGTH)
            total: Optional[int] = None
            if size is not None and size >= 0:
                total = size
            elif content_length:
                try:
                    length = int(content_length)
                except (TypeError, ValueError):
                    total = None
                else:
                    total = existing + length if existing else length
            with (
                open(file_path, mode) as f,
                tqdm(
                    total=total,
                    initial=existing,
                    unit="B",
                    unit_scale=True,
                    desc=file_path.name,
                ) as pbar,
            ):
                for chunk in r.iter_content(chunk_size=cfg.chunk_size):
                    if not chunk:
                        continue
                    f.write(chunk)
                    downloaded += len(chunk)
                    pbar.update(len(chunk))
        return downloaded
    except requests.RequestException as e:
        print(f"Single-thread download failed: {e}", file=sys.stderr)
        return downloaded


# --------------------------- Workflow --------------------------- #


def download(cfg: Config) -> Result:
    t0 = time.time()
    probed_size, range_ok, headers = probe(
        cfg.url, cfg.timeout, cfg.verify_tls, cfg.user_agent
    )
    if probed_size is None and not headers:
        return Result(
            cfg.url,
            str(cfg.output),
            -1,
            0,
            cfg.threads,
            0.0,
            False,
            None,
            None,
            "Probe failed",
        )
    size = probed_size
    reported_size = size if size is not None else -1

    resumed = False
    session = requests.Session()
    threads_used = 1

    if cfg.resume and cfg.output.exists():
        existing = cfg.output.stat().st_size
        if size and existing < size:
            resumed = True
        elif size and existing == size:
            print("File already fully downloaded. Skipping.")
            elapsed = time.time() - t0
            if cfg.resume:
                with contextlib.suppress(OSError):
                    _resume_metadata_path(cfg.output).unlink()
            checksum = compute_sha256(cfg.output) if cfg.expected_sha256 else None
            checksum_match = (
                (checksum == cfg.expected_sha256) if cfg.expected_sha256 else None
            )
            return Result(
                cfg.url,
                str(cfg.output),
                size,
                size,
                0,
                elapsed,
                True,
                checksum,
                checksum_match,
                None,
            )
        elif existing:
            resumed = True

    downloaded = 0
    resume_state: Optional[ResumeState] = None

    if not range_ok or cfg.threads == 1 or size is None:
        if cfg.resume:
            with contextlib.suppress(OSError):
                _resume_metadata_path(cfg.output).unlink()
        downloaded = single_thread_download(session, cfg, size, cfg.output)
    else:
        parts = plan_parts(size, cfg.threads)
        resume_state = (
            ResumeState.initialize(cfg.output, size, parts) if cfg.resume else None
        )
        if resume_state:
            resume_state.sync_with_file()
        initial_bytes = resume_state.completed_bytes() if resume_state else 0
        if size:
            initial_bytes = min(initial_bytes, size)
        if initial_bytes:
            resumed = True
        downloaded = initial_bytes
        parts_to_download = [
            part for part in parts if not resume_state or not resume_state.is_complete(part)
        ]
        # Prepare file
        mode = "r+b" if cfg.output.exists() else "wb"
        with open(cfg.output, mode) as f:
            f.truncate(size)
        with tqdm(
            total=size,
            unit="B",
            unit_scale=True,
            desc=cfg.output.name,
            initial=initial_bytes,
        ) as pbar:
            if parts_to_download:
                with futures.ThreadPoolExecutor(max_workers=cfg.threads) as pool:
                    future_list = [
                        pool.submit(
                            download_part,
                            session,
                            cfg,
                            part,
                            pbar,
                            cfg.retries,
                            cfg.timeout,
                            cfg.output,
                            resume_state,
                        )
                        for part in parts_to_download
                    ]
                    for fut in futures.as_completed(future_list):
                        downloaded += fut.result()
            else:
                pbar.update(0)
        threads_used = cfg.threads

    elapsed = time.time() - t0
    final_size = cfg.output.stat().st_size if cfg.output.exists() else 0

    checksum = (
        compute_sha256(cfg.output) if cfg.expected_sha256 and final_size else None
    )
    checksum_match = (
        (checksum == cfg.expected_sha256) if checksum and cfg.expected_sha256 else None
    )

    error = None
    if size and final_size != size:
        error = f"Size mismatch: expected {size}, got {final_size}"
    if checksum_match is False:
        error = (error + "; " if error else "") + "Checksum mismatch"

    if not error and size and final_size == size and resume_state:
        resume_state.cleanup()

    return Result(
        url=cfg.url,
        output=str(cfg.output),
        size=reported_size,
        downloaded=final_size,
        threads=threads_used,
        elapsed=elapsed,
        resumed=resumed,
        checksum=checksum,
        checksum_match=checksum_match,
        error=error,
    )


# --------------------------- Verification --------------------------- #


def compute_sha256(path: Path, chunk: int = 1024 * 1024) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for block in iter(lambda: f.read(chunk), b""):
            h.update(block)
    return h.hexdigest()


# --------------------------- CLI --------------------------- #


def build_arg_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Robust multi-threaded downloader with resume & checksum."
    )
    p.add_argument("url", help="URL of the file to download")
    p.add_argument(
        "-n",
        "--threads",
        type=int,
        default=8,
        help="Number of download threads (default: 8)",
    )
    p.add_argument(
        "-o", "--output", type=Path, help="Output file name (default: derived from URL)"
    )
    p.add_argument(
        "--retry",
        dest="retries",
        type=int,
        default=3,
        help="Retries per chunk on failure (default: 3)",
    )
    p.add_argument(
        "--timeout",
        type=float,
        default=15.0,
        help="Request timeout seconds (default: 15)",
    )
    p.add_argument(
        "--backoff",
        type=float,
        default=0.75,
        help="Base backoff seconds (default: 0.75)",
    )
    p.add_argument(
        "--resume", action="store_true", help="Attempt to resume if partial file exists"
    )
    p.add_argument(
        "--chunk-size",
        type=int,
        default=8192,
        help="Streaming chunk size bytes (default: 8192)",
    )
    p.add_argument(
        "--sha256", dest="sha256", help="Expected SHA-256 checksum to verify integrity"
    )
    p.add_argument(
        "--json", dest="json_path", type=Path, help="Write JSON summary to path"
    )
    p.add_argument(
        "--insecure", action="store_true", help="Disable TLS certificate verification"
    )
    p.add_argument("--user-agent", default=DEFAULT_UA, help="Custom User-Agent header")
    return p


def parse_args(argv: Optional[List[str]]) -> Config:
    parser = build_arg_parser()
    args = parser.parse_args(argv)
    if args.threads < 1:
        print("Error: --threads must be >= 1", file=sys.stderr)
        raise SystemExit(2)
    if args.retries < 0:
        print("Error: --retry must be >= 0", file=sys.stderr)
        raise SystemExit(2)
    output = args.output or Path(args.url.rstrip("/").split("/")[-1] or "download.bin")
    return Config(
        url=args.url,
        threads=args.threads,
        output=output,
        retries=args.retries,
        timeout=args.timeout,
        backoff=args.backoff,
        resume=args.resume,
        chunk_size=args.chunk_size,
        expected_sha256=args.sha256.lower() if args.sha256 else None,
        json_path=args.json_path,
        verify_tls=not args.insecure,
        user_agent=args.user_agent,
    )


# --------------------------- Orchestration --------------------------- #


def main(argv: Optional[List[str]] = None) -> int:
    cfg = parse_args(argv)
    result = download(cfg)

    # Human summary
    print(f"\nURL:        {result.url}")
    print(f"Output:     {result.output}")
    print(f"Size:       {human_size(result.size)}")
    print(f"Downloaded: {human_size(result.downloaded)}")
    print(f"Threads:    {result.threads}")
    print(f"Elapsed:    {result.elapsed:.2f}s")
    print(f"Resumed:    {result.resumed}")
    if result.checksum:
        print(f"SHA256:     {result.checksum}")
        if result.checksum_match is not None:
            print(f"Checksum OK: {result.checksum_match}")
    if result.error:
        print(f"Error:      {result.error}", file=sys.stderr)

    if cfg.json_path:
        payload = {
            "url": result.url,
            "output": result.output,
            "size": result.size,
            "downloaded": result.downloaded,
            "threads": result.threads,
            "elapsed": result.elapsed,
            "resumed": result.resumed,
            "checksum": result.checksum,
            "checksum_match": result.checksum_match,
            "error": result.error,
        }
        try:
            with open(cfg.json_path, "w", encoding="utf-8") as fh:
                json.dump(payload, fh, indent=2)
        except OSError as e:
            print(f"Failed to write JSON summary: {e}", file=sys.stderr)

    if result.error:
        return 1
    if result.checksum_match is False:
        return 1
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
