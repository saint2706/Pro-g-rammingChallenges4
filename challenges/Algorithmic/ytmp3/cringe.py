"""YouTube Audio Downloader (MP3/other formats) - Modernized.

Features:
  * Prefers `yt-dlp` (actively maintained) with fallback to `youtube_dl`.
  * Batch downloads: pass multiple URLs or a file of URLs.
  * Choose output audio format & quality (requires ffmpeg)
  * JSON summary output (--json) for scripting pipelines
  * Safe filename templating; configurable output directory
  * Graceful error handling with per-item status collection

Examples:
  python cringe.py https://youtu.be/VIDEOID
  python cringe.py -f opus -q 128 https://youtu.be/ID1 https://youtu.be/ID2
  python cringe.py -F mp3 -q 192 -o downloads/ -i urls.txt --json

Notes:
  * Ensure `ffmpeg` is installed and in PATH for post-processing.
  * Some formats/qualities depend on source availability.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Tuple

# ------------------------------ Backend Selection ------------------------------ #

DOWNLOAD_BACKEND = None  # Will hold module (yt_dlp or youtube_dl)
BACKEND_NAME = None

try:  # Prefer yt-dlp
    import yt_dlp as ydl_mod  # type: ignore

    DOWNLOAD_BACKEND = ydl_mod
    BACKEND_NAME = "yt-dlp"
except Exception:  # pragma: no cover - optional dependency
    try:
        import youtube_dl as ydl_mod  # type: ignore

        DOWNLOAD_BACKEND = ydl_mod
        BACKEND_NAME = "youtube_dl"
    except Exception:
        print(
            "Error: Neither 'yt-dlp' nor 'youtube_dl' is installed. Install one with 'pip install yt-dlp'.",
            file=sys.stderr,
        )
        sys.exit(1)

DownloadError = getattr(DOWNLOAD_BACKEND, "DownloadError", Exception)


# ------------------------------ Configuration ------------------------------ #


@dataclass(slots=True)
class DownloadConfig:
    urls: List[str]
    format: str = "mp3"  # output audio codec / container
    quality: str = "192"  # kbps target for mp3; some backends ignore for others
    out_dir: Path = Path(".")
    url_file: Optional[Path] = None
    json_out: bool = False
    overwrite: bool = False
    quiet: bool = False

    def validate(self) -> None:
        if not self.urls and not self.url_file:
            raise ValueError("Provide at least one URL or --input file")
        if self.url_file and not self.url_file.exists():
            raise ValueError(f"URL file not found: {self.url_file}")
        if self.format.lower() not in {"mp3", "m4a", "opus", "vorbis", "wav", "flac"}:
            raise ValueError("Unsupported format (choose mp3,m4a,opus,vorbis,wav,flac)")
        self.out_dir.mkdir(parents=True, exist_ok=True)

    def all_urls(self) -> List[str]:
        urls = list(self.urls)
        if self.url_file:
            with self.url_file.open("r", encoding="utf-8") as f:
                for line in f:
                    line = line.strip()
                    if line and not line.startswith("#"):
                        urls.append(line)
        return urls


# ------------------------------ Download Logic ------------------------------ #


def build_ydl_opts(cfg: DownloadConfig) -> dict:
    postprocessors = [
        {
            "key": "FFmpegExtractAudio",
            "preferredcodec": cfg.format,
            "preferredquality": cfg.quality,
        }
    ]
    # For lossless/wav we may not want re-encode quality setting, but backend will handle gracefully.
    return {
        "format": "bestaudio/best",
        "postprocessors": postprocessors,
        "outtmpl": str(cfg.out_dir / "%(title)s.%(ext)s"),
        "quiet": cfg.quiet,
        "ignoreerrors": True,  # We collect errors manually
        "overwrites": cfg.overwrite,
        "noplaylist": True,  # treat single video; user can pass multiple explicit URLs/files
        "nocheckcertificate": True,
    }


def download_one(url: str, cfg: DownloadConfig) -> Tuple[str, bool, Optional[str]]:
    ydl_opts = build_ydl_opts(cfg)
    try:
        with DOWNLOAD_BACKEND.YoutubeDL(ydl_opts) as ydl:  # type: ignore[attr-defined]
            ydl.download([url])
        return url, True, None
    except DownloadError as e:  # type: ignore
        return url, False, str(e)
    except Exception as e:  # pragma: no cover - unexpected
        return url, False, f"Unexpected:{e}"  # captured for JSON summary


def batch_download(cfg: DownloadConfig) -> dict:
    results = []
    success = 0
    total = 0
    for url in cfg.all_urls():
        total += 1
        if not cfg.quiet:
            print(f"Downloading: {url}")
        u, ok, err = download_one(url, cfg)
        if ok:
            success += 1
        else:
            if not cfg.quiet:
                print(f"  Failed: {err}", file=sys.stderr)
        results.append({"url": u, "success": ok, "error": err})
    return {
        "backend": BACKEND_NAME,
        "format": cfg.format,
        "quality": cfg.quality,
        "out_dir": str(cfg.out_dir.resolve()),
        "total": total,
        "success": success,
        "failed": total - success,
        "items": results,
    }


# ------------------------------ CLI ------------------------------ #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="YouTube audio downloader (yt-dlp preferred)"
    )
    p.add_argument("urls", nargs="*", help="Video URLs to download")
    p.add_argument(
        "-i",
        "--input",
        dest="url_file",
        help="Path to file containing URLs (one per line)",
    )
    p.add_argument(
        "-f",
        "--format",
        default="mp3",
        help="Output audio format (mp3,m4a,opus,vorbis,wav,flac)",
    )
    p.add_argument(
        "-q",
        "--quality",
        default="192",
        help="Audio quality/bitrate target (where applicable)",
    )
    p.add_argument("-o", "--out-dir", default=".", help="Output directory")
    p.add_argument("--overwrite", action="store_true", help="Overwrite existing files")
    p.add_argument("--json", action="store_true", help="Emit JSON summary to stdout")
    p.add_argument(
        "--quiet",
        action="store_true",
        help="Suppress progress lines (errors still printed)",
    )
    return p


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    cfg = DownloadConfig(
        urls=args.urls,
        format=args.format.lower(),
        quality=args.quality,
        out_dir=Path(args.out_dir),
        url_file=Path(args.url_file) if args.url_file else None,
        json_out=args.json,
        overwrite=args.overwrite,
        quiet=args.quiet,
    )
    try:
        cfg.validate()
    except ValueError as e:
        parser.error(str(e))

    summary = batch_download(cfg)
    if cfg.json_out:
        print(json.dumps(summary, indent=2))
    else:
        print(
            f"Completed: {summary['success']}/{summary['total']} succeeded (backend={summary['backend']})"
        )
    return 0 if summary["failed"] == 0 else 1


if __name__ == "__main__":  # pragma: no cover - CLI entry
    raise SystemExit(main())
