# Image Converter

A Pillow-powered toolkit for bulk image conversion with optional resizing and
metadata preservation.  The project exposes a reusable Python API, a CLI, and a
small Tkinter GUI wrapper so it fits into command line workflows or quick
one-off desktop usage.

## Features

| Capability | Notes |
|------------|-------|
| Multi-format export | JPEG, PNG, WebP, TIFF, BMP (extensible via `SUPPORTED_EXPORT_FORMATS`). |
| Batch mode | Accept individual files or whole directories (optionally recursive). |
| Resizing | Use `WIDTHxHEIGHT`, `WIDTHx`, or `xHEIGHT` to scale while retaining aspect ratios when one dimension is omitted. |
| Quality control | Pass JPEG/WebP quality as `--quality 1..100`. |
| Metadata preservation | EXIF + ICC profiles copied when compatible; PNG text chunks mirrored when possible. |
| GUI convenience | `--gui` flag launches a minimal Tk interface for casual use. |

## Installation

```bash
pip install -r "Practical/requirements.txt"
```

The consolidated requirements already ship with Pillow.  Tkinter is bundled
with most desktop Python distributions; on Linux you may need your distro's
`python3-tk` package for the GUI.

## Command Line Usage

```bash
python "Practical/Image Converter/convert.py" input.jpg another/dir --target-format png \
    --output-dir converted --resize 1200x --quality 92
```

Key options:

| Flag | Description |
|------|-------------|
| `-t / --target-format` | Desired output format (case-insensitive). |
| `-o / --output-dir` | Target directory (default: alongside source). |
| `--resize WxH` | Resize using `WIDTHxHEIGHT` syntax – allow missing width/height to preserve aspect ratio. |
| `-q / --quality` | Quality for lossy formats (JPEG/WebP). |
| `--recursive` | Recurse into directories when expanding inputs. |
| `--strip-metadata` | Skip EXIF/ICC/PNG text preservation. |
| `--gui` | Launch the Tkinter wrapper instead of the CLI workflow. |

Run `python "Practical/Image Converter/convert.py" --help` to see the full
usage text.

## Python API

```python
from pathlib import Path
from Practical.Image Converter.convert import batch_convert, ResizeSpec

outputs = batch_convert(
    [Path("images")],
    target_format="webp",
    output_dir=Path("out"),
    resize=ResizeSpec(width=1600),
    quality=85,
    recursive=True,
)
print(f"Converted {len(outputs)} files")
```

The module exposes:

- `convert_image(Path, target_format, ...)` – single file conversion.
- `batch_convert([...], target_format, ...)` – orchestrates directory expansion
  and conversion.
- `ResizeSpec` and `parse_resize()` – helpers for consistent resize parsing.

Errors are logged but non-fatal items are skipped so long batch jobs can
continue processing remaining files.

## Examples

| Goal | Command |
|------|---------|
| Convert a camera roll to WebP, strip metadata, keep originals untouched | `python "Practical/Image Converter/convert.py" dcim --target-format webp --output-dir webp --recursive --strip-metadata` |
| Generate blog-ready JPEGs from PNG illustrations at 1600px wide | `python "Practical/Image Converter/convert.py" art/*.png --target-format jpeg --resize 1600x --quality 88` |
| Downscale screenshots to 1080p height while retaining EXIF | `python "Practical/Image Converter/convert.py" shots --target-format png --resize x1080` |
| Launch GUI mode for manual selections | `python "Practical/Image Converter/convert.py" dummy --target-format png --gui` *(inputs selected interactively)* |

> **Tip:** WebP conversions require a Pillow build compiled with WebP support.
> Most wheels include it; if you see `OSError: cannot write mode`, reinstall
> Pillow with `pip install Pillow[webp]`.

## Testing

Automated coverage lives in `tests/test_convert.py` and can be executed from
repo root:

```bash
pytest "Practical/Image Converter/tests"
```

The tests dynamically generate sample images (with EXIF) to keep the repository
lightweight.
