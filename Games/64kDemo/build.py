#!/usr/bin/env python3
"""Bundle and compress the 64k demo using esbuild for robustness."""

from __future__ import annotations

import gzip
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).parent
SRC = ROOT / "src"
DIST = ROOT / "dist"


class BuildError(RuntimeError):
    """Raised when the bundler encounters a fatal error."""


def bundle_js(entry: Path) -> str:
    """Run esbuild (via local install or npx) and return the bundled source."""

    esbuild = shutil.which("esbuild")
    npx = shutil.which("npx")
    commands = []
    if esbuild:
        commands.append([esbuild])
    if npx:
        commands.append([npx, "esbuild"])
    if not commands:
        raise BuildError(
            "esbuild is required for bundling. Install it globally or ensure 'npx' is available."
        )

    with tempfile.NamedTemporaryFile(suffix=".js", delete=False) as tmp:
        outfile = Path(tmp.name)

    for prefix in commands:
        cmd = [
            *prefix,
            str(entry),
            "--bundle",
            "--minify",
            "--format=iife",
            "--target=es2017",
            f"--outfile={outfile}",
            "--log-level=error",
        ]
        try:
            subprocess.run(cmd, check=True, capture_output=True, text=True)
            result = outfile.read_text(encoding="utf-8")
            outfile.unlink(missing_ok=True)
            return result
        except FileNotFoundError:
            continue
        except subprocess.CalledProcessError as exc:  # pragma: no cover - surfaced to CLI
            outfile.unlink(missing_ok=True)
            raise BuildError(exc.stderr or "esbuild failed to bundle the entry point")

    outfile.unlink(missing_ok=True)
    raise BuildError("Unable to execute esbuild via installed binary or npx.")


def minify_html(text: str) -> str:
    """Whitespace-only HTML minifier that preserves inline scripts/styles."""

    lines = [line.strip() for line in text.splitlines() if line.strip()]
    return "".join(lines)


def build() -> None:
    DIST.mkdir(exist_ok=True)
    html = (SRC / "index.html").read_text(encoding="utf-8")
    try:
        js_bundle = bundle_js(SRC / "main.js")
    except BuildError as exc:
        print(f"error: {exc}", file=sys.stderr)
        raise SystemExit(1) from exc

    packed_html = html.replace(
        '<script src="main.js"></script>', f"<script>{js_bundle}</script>"
    )
    packed_html = minify_html(packed_html)
    output = DIST / "index.html"
    output.write_text(packed_html, encoding="utf-8")
    with gzip.open(DIST / "index.html.gz", "wb", compresslevel=9) as fh:
        fh.write(packed_html.encode("utf-8"))
    print("minified bytes:", len(packed_html.encode("utf-8")))
    print("gzipped bytes:", (DIST / "index.html.gz").stat().st_size)


if __name__ == "__main__":
    build()
