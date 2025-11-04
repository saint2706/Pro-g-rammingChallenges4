"""Utility functions and CLI for batch image conversion.

The module provides a high level :func:`batch_convert` helper that
wraps Pillow's image codecs with a few niceties:

* Automatic inference of output names and extensions.
* Optional batch processing of directories (non-recursive by default).
* High quality resizing with aspect ratio preservation when a single
  dimension is provided.
* Best-effort metadata preservation (EXIF, ICC profiles, PNG text
  chunks) without getting in the way when Pillow lacks support.
* A small CLI and opt-in Tk GUI wrapper so the script remains friendly
  to both terminals and casual desktop use.

The functions are written so tests can import them directly without
spawning subprocesses.  ``main()`` simply wires argparse to
``batch_convert`` and optionally launches the GUI.
"""

from __future__ import annotations

from dataclasses import dataclass
import argparse
import logging
import os
import sys
from pathlib import Path
from typing import IO, Iterable, Iterator, List, Optional, Sequence, Tuple, Union

try:
    from PIL import Image, ImageOps, PngImagePlugin, UnidentifiedImageError
except ImportError as exc:  # pragma: no cover - dependency missing
    raise SystemExit(
        "Pillow is required for the image converter. Install it with ``pip install Pillow``."
    ) from exc

LOGGER = logging.getLogger(__name__)


class ImageOpenError(RuntimeError):
    """Raised when an input image cannot be opened."""


SUPPORTED_EXPORT_FORMATS = {
    "JPEG": (".jpg", ".jpeg"),
    "PNG": (".png",),
    "WEBP": (".webp",),
    "TIFF": (".tif", ".tiff"),
    "BMP": (".bmp",),
}
SUPPORTED_EXPORT_SET = {fmt for fmt in SUPPORTED_EXPORT_FORMATS}


@dataclass(frozen=True)
class ResizeSpec:
    """Desired output dimensions.

    ``width`` or ``height`` can be ``None`` to indicate "scale based on the
    other dimension".  When both are provided, the image is resized to the
    exact dimensions, respecting aspect ratio only if the caller computed it.
    """

    width: Optional[int] = None
    height: Optional[int] = None

    def apply(self, image: Image.Image) -> Image.Image:
        """Return a resized copy of ``image`` using high quality resampling."""

        width, height = self.width, self.height
        if width is None and height is None:
            return image

        if width is None:
            scale = height / image.height
            width = max(1, int(image.width * scale))
        elif height is None:
            scale = width / image.width
            height = max(1, int(image.height * scale))

        assert width is not None and height is not None
        if width == image.width and height == image.height:
            return image

        return image.resize((width, height), Image.Resampling.LANCZOS)


def parse_resize(value: str) -> ResizeSpec:
    """Parse ``WIDTHxHEIGHT`` style strings into :class:`ResizeSpec`.

    ``800x600`` resizes to that exact size.
    ``800x`` keeps aspect ratio while fitting width to 800 pixels.
    ``x600`` keeps aspect ratio while fitting height to 600 pixels.
    """

    if "x" not in value:
        raise argparse.ArgumentTypeError(
            "Resize must be WIDTHxHEIGHT (allowing missing width/height)."
        )
    width_s, height_s = value.split("x", 1)
    width = int(width_s) if width_s.strip() else None
    height = int(height_s) if height_s.strip() else None
    if width is None and height is None:
        raise argparse.ArgumentTypeError(
            "Resize requires at least a width or height value."
        )
    if width is not None and width <= 0:
        raise argparse.ArgumentTypeError("Width must be > 0.")
    if height is not None and height <= 0:
        raise argparse.ArgumentTypeError("Height must be > 0.")
    return ResizeSpec(width=width, height=height)


def _gather_inputs(paths: Sequence[Path], recursive: bool = False) -> Iterator[Path]:
    """Yield image file paths from ``paths``.

    Directories are expanded.  Non-existent files are skipped with a warning.
    """

    for raw_path in paths:
        path = Path(raw_path)
        if not path.exists():
            LOGGER.warning("Skipping missing path: %s", path)
            continue
        if path.is_dir():
            iterator = path.rglob("*") if recursive else path.glob("*")
            for candidate in iterator:
                if candidate.is_file():
                    yield candidate
        elif path.is_file():
            yield path


def _ensure_mode(image: Image.Image, target_format: str) -> Image.Image:
    """Convert the image mode if required by the destination format."""

    target_format = target_format.upper()
    if target_format in {"JPEG", "JPG"} and image.mode not in {"RGB", "L"}:
        # JPEG does not support alpha/ palette. Convert to RGB.
        return image.convert("RGB")
    return image


def _collect_metadata(
    image: Image.Image,
) -> Tuple[dict, Optional[bytes], Optional[bytes]]:
    """Return (info, exif_bytes, icc_profile)."""

    info = {}
    if image.info:
        for key, value in image.info.items():
            # Pillow objects (e.g., chunks) often do not round-trip; keep only safe values.
            if isinstance(value, (str, bytes)):
                info[key] = value
    exif_bytes = None
    try:
        exif_bytes = image.info.get("exif") or image.getexif().tobytes()
    except Exception:  # pragma: no cover - Pillow can throw on unsupported EXIF
        exif_bytes = None
    icc_profile = image.info.get("icc_profile")
    return info, exif_bytes, icc_profile


def _build_save_kwargs(
    target_format: str,
    info: dict,
    exif_bytes: Optional[bytes],
    icc_profile: Optional[bytes],
) -> dict:
    """Create keyword arguments for :meth:`Image.Image.save` preserving metadata."""

    save_kwargs: dict = {}
    fmt_upper = target_format.upper()
    if exif_bytes and fmt_upper in {"JPEG", "JPG", "TIFF"}:
        save_kwargs["exif"] = exif_bytes
    if icc_profile:
        save_kwargs["icc_profile"] = icc_profile

    if fmt_upper == "PNG" and info:
        pnginfo = PngImagePlugin.PngInfo()
        added_any = False
        for key, value in info.items():
            if key in {"transparency", "exif", "icc_profile"}:
                continue
            if isinstance(value, bytes):
                try:
                    value = value.decode("utf-8")
                except UnicodeDecodeError:
                    continue
            pnginfo.add_text(str(key), str(value))
            added_any = True
        if added_any:
            save_kwargs["pnginfo"] = pnginfo
    return save_kwargs


def _infer_output_path(
    input_path: Path, output_dir: Optional[Path], target_format: str
) -> Path:
    fmt_upper = target_format.upper()
    try:
        extension = SUPPORTED_EXPORT_FORMATS[fmt_upper][0]
    except KeyError as exc:
        raise ValueError(f"Unsupported export format: {target_format}") from exc
    destination_dir = output_dir if output_dir else input_path.parent
    return destination_dir / f"{input_path.stem}{extension}"


PathOrStr = Union[str, os.PathLike, Path]
FileLike = IO[bytes]


def _is_path_like(value: object) -> bool:
    return isinstance(value, (str, Path, os.PathLike))


def convert_image(
    input_path: Union[PathOrStr, FileLike],
    *,
    target_format: str,
    output_path: Optional[Union[PathOrStr, FileLike]] = None,
    quality: Optional[int] = None,
    resize: Optional[ResizeSpec] = None,
    keep_metadata: bool = True,
) -> Optional[Path]:
    """Convert an image to ``target_format``.

    Parameters accept filesystem paths or file-like objects. When ``input_path``
    is a file-like, an explicit ``output_path`` must be provided. When
    ``output_path`` is a file-like, ``None`` is returned instead of a
    :class:`~pathlib.Path` instance.
    """

    target_format = target_format.upper()
    if target_format not in SUPPORTED_EXPORT_SET:
        raise ValueError(
            f"Unsupported target format '{target_format}'. Supported: {sorted(SUPPORTED_EXPORT_SET)}"
        )

    resolved_input_path: Optional[Path] = None
    if _is_path_like(input_path):
        resolved_input_path = Path(input_path)
        if not resolved_input_path.exists():
            raise FileNotFoundError(resolved_input_path)
        image_source: Union[Path, FileLike] = resolved_input_path
    else:
        if not hasattr(input_path, "read"):
            raise TypeError(
                "input_path must be a filesystem path or binary file-like object"
            )
        if hasattr(input_path, "seek"):
            try:
                input_path.seek(0)
            except Exception:  # pragma: no cover - best effort
                pass
        image_source = input_path  # type: ignore[assignment]

    destination: Union[Path, FileLike]
    written_path: Optional[Path] = None
    if output_path is None:
        if resolved_input_path is None:
            raise ValueError(
                "output_path must be provided when reading from a file-like object"
            )
        destination = _infer_output_path(resolved_input_path, None, target_format)
        written_path = destination
    elif _is_path_like(output_path):
        resolved_output = Path(output_path)
        if resolved_output.is_dir():
            if resolved_input_path is not None:
                resolved_output = _infer_output_path(
                    resolved_input_path, resolved_output, target_format
                )
            else:
                name_hint = getattr(input_path, "name", None)
                if not name_hint:
                    raise ValueError(
                        "Directory output_path requires the file-like input to provide a 'name' attribute."
                    )
                resolved_output = (
                    resolved_output
                    / f"{Path(str(name_hint)).stem}{SUPPORTED_EXPORT_FORMATS[target_format][0]}"
                )
        else:
            resolved_output.parent.mkdir(parents=True, exist_ok=True)
        destination = resolved_output
        written_path = resolved_output
    else:
        destination = output_path  # type: ignore[assignment]

    try:
        opener = Image.open(image_source)
    except (OSError, UnidentifiedImageError) as exc:
        label: Union[str, Path]
        if resolved_input_path is not None:
            label = resolved_input_path
        else:
            label = getattr(image_source, "name", None) or "provided input"
        raise ImageOpenError(f"Failed to open image '{label}': {exc}") from exc

    with opener as img:
        img = ImageOps.exif_transpose(img)
        info: dict = {}
        exif_bytes: Optional[bytes] = None
        icc_profile: Optional[bytes] = None
        if keep_metadata:
            info, exif_bytes, icc_profile = _collect_metadata(img)
        if resize is not None:
            img = resize.apply(img)
        img = _ensure_mode(img, target_format)

        save_kwargs = (
            _build_save_kwargs(target_format, info, exif_bytes, icc_profile)
            if keep_metadata
            else {}
        )
        if quality is not None:
            if not 1 <= quality <= 100:
                raise ValueError("Quality must be within 1..100")
            save_kwargs["quality"] = quality
        img.save(destination, format=target_format, **save_kwargs)

    if hasattr(destination, "seek"):
        try:
            destination.seek(0)  # type: ignore[union-attr]
        except Exception:  # pragma: no cover - non seekable output
            pass

    return written_path


BatchInput = Union[PathOrStr, FileLike, Tuple[str, FileLike]]


def _normalize_file_like(
    item: Union[FileLike, Tuple[str, FileLike]],
) -> Tuple[str, FileLike]:
    if isinstance(item, tuple):
        if len(item) != 2:
            raise ValueError("Tuple file-like inputs must be (name, file_obj)")
        name, file_obj = item
        if not hasattr(file_obj, "read"):
            raise TypeError("Provided file-like object does not support read()")
        return str(name), file_obj

    if not hasattr(item, "read"):
        raise TypeError("File-like inputs must implement read()")
    name = getattr(item, "name", None)
    if not name:
        raise ValueError(
            "File-like inputs must provide a name via attribute or tuple metadata"
        )
    return str(name), item


def batch_convert(
    inputs: Sequence[BatchInput],
    *,
    target_format: str,
    output_dir: Optional[Path] = None,
    quality: Optional[int] = None,
    resize: Optional[ResizeSpec] = None,
    recursive: bool = False,
    keep_metadata: bool = True,
) -> List[Path]:
    """Convert multiple files. Returns successfully written paths."""

    fmt_upper = target_format.upper()
    if fmt_upper not in SUPPORTED_EXPORT_SET:
        raise ValueError(
            f"Unsupported target format '{target_format}'. Supported: {sorted(SUPPORTED_EXPORT_SET)}"
        )

    written: List[Path] = []
    output_dir = Path(output_dir) if output_dir else None
    for raw_input in inputs:
        if _is_path_like(raw_input):
            for input_path in _gather_inputs([Path(raw_input)], recursive=recursive):
                try:
                    destination = _infer_output_path(
                        input_path, output_dir, target_format
                    )
                    destination.parent.mkdir(parents=True, exist_ok=True)
                    path = convert_image(
                        input_path,
                        target_format=target_format,
                        output_path=destination,
                        quality=quality,
                        resize=resize,
                        keep_metadata=keep_metadata,
                    )
                    if path is not None:
                        written.append(path)
                except Exception as exc:
                    LOGGER.error("Failed to convert %s: %s", input_path, exc)
        else:
            try:
                if output_dir is None:
                    raise ValueError(
                        "output_dir is required when converting file-like inputs"
                    )
                name, file_obj = _normalize_file_like(raw_input)
                extension = SUPPORTED_EXPORT_FORMATS[fmt_upper][0]
                destination = output_dir / f"{Path(name).stem}{extension}"
                destination.parent.mkdir(parents=True, exist_ok=True)
                path = convert_image(
                    file_obj,
                    target_format=target_format,
                    output_path=destination,
                    quality=quality,
                    resize=resize,
                    keep_metadata=keep_metadata,
                )
                if path is not None:
                    written.append(path)
            except Exception as exc:
                LOGGER.error(
                    "Failed to convert in-memory file %s: %s",
                    getattr(raw_input, "name", raw_input),
                    exc,
                )
    return written


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Batch image conversion utility")
    parser.add_argument(
        "inputs", nargs="+", help="Input files or directories to convert"
    )
    parser.add_argument(
        "--target-format",
        "-t",
        required=True,
        help="Output format (e.g. png, jpeg, webp)",
    )
    parser.add_argument("--output-dir", "-o", help="Directory to place converted files")
    parser.add_argument(
        "--quality", "-q", type=int, help="Quality for lossy formats (1-100)"
    )
    parser.add_argument(
        "--resize",
        type=parse_resize,
        metavar="WxH",
        help="Resize images. Examples: 800x600 exact, 1200x keep aspect by width, x720 keep aspect by height.",
    )
    parser.add_argument(
        "--recursive",
        action="store_true",
        help="Recurse into directories when gathering inputs",
    )
    parser.add_argument(
        "--strip-metadata", action="store_true", help="Do not attempt to copy metadata"
    )
    parser.add_argument(
        "--gui", action="store_true", help="Launch minimal Tkinter GUI wrapper"
    )
    parser.add_argument(
        "--verbose", "-v", action="count", default=0, help="Increase logging verbosity"
    )
    return parser


def launch_gui() -> None:
    """Launch a lightweight Tkinter interface wrapping :func:`batch_convert`."""

    import tkinter as tk
    from tkinter import filedialog, messagebox

    class ConverterGUI(tk.Tk):
        def __init__(self) -> None:
            super().__init__()
            self.title("Image Converter")
            self.inputs: List[str] = []

            self.columnconfigure(1, weight=1)

            tk.Label(self, text="Inputs:").grid(
                row=0, column=0, sticky="w", padx=4, pady=4
            )
            self.inputs_var = tk.StringVar(value="No files selected")
            tk.Label(
                self, textvariable=self.inputs_var, anchor="w", justify="left"
            ).grid(row=0, column=1, sticky="ew", padx=4, pady=4)
            tk.Button(self, text="Choose files", command=self.choose_files).grid(
                row=0, column=2, padx=4, pady=4
            )

            tk.Label(self, text="Output directory:").grid(
                row=1, column=0, sticky="w", padx=4, pady=4
            )
            self.output_dir_var = tk.StringVar()
            tk.Entry(self, textvariable=self.output_dir_var).grid(
                row=1, column=1, sticky="ew", padx=4, pady=4
            )
            tk.Button(self, text="Browse", command=self.choose_output_dir).grid(
                row=1, column=2, padx=4, pady=4
            )

            tk.Label(self, text="Target format:").grid(
                row=2, column=0, sticky="w", padx=4, pady=4
            )
            self.format_var = tk.StringVar(value="png")
            tk.Entry(self, textvariable=self.format_var).grid(
                row=2, column=1, sticky="ew", padx=4, pady=4
            )

            tk.Label(self, text="Quality (1-100):").grid(
                row=3, column=0, sticky="w", padx=4, pady=4
            )
            self.quality_var = tk.StringVar()
            tk.Entry(self, textvariable=self.quality_var).grid(
                row=3, column=1, sticky="ew", padx=4, pady=4
            )

            tk.Label(self, text="Resize (WxH):").grid(
                row=4, column=0, sticky="w", padx=4, pady=4
            )
            self.resize_var = tk.StringVar()
            tk.Entry(self, textvariable=self.resize_var).grid(
                row=4, column=1, sticky="ew", padx=4, pady=4
            )

            self.keep_metadata_var = tk.BooleanVar(value=True)
            tk.Checkbutton(
                self, text="Preserve metadata", variable=self.keep_metadata_var
            ).grid(row=5, column=0, columnspan=2, sticky="w", padx=4, pady=4)

            tk.Button(self, text="Convert", command=self.convert).grid(
                row=6, column=0, columnspan=3, pady=12
            )

        def choose_files(self) -> None:
            files = filedialog.askopenfilenames(title="Select images")
            if files:
                self.inputs = list(files)
                self.inputs_var.set("\n".join(Path(f).name for f in files))

        def choose_output_dir(self) -> None:
            directory = filedialog.askdirectory(title="Select output directory")
            if directory:
                self.output_dir_var.set(directory)

        def convert(self) -> None:
            if not self.inputs:
                messagebox.showwarning(
                    "Image Converter", "Please select at least one input file."
                )
                return
            fmt = self.format_var.get().strip()
            if not fmt:
                messagebox.showwarning("Image Converter", "Target format is required.")
                return
            resize_value = self.resize_var.get().strip()
            resize_spec = None
            if resize_value:
                try:
                    resize_spec = parse_resize(resize_value)
                except Exception as exc:  # pragma: no cover - GUI path hard to test
                    messagebox.showerror(
                        "Image Converter", f"Invalid resize specification: {exc}"
                    )
                    return
            quality_value = self.quality_var.get().strip()
            quality = int(quality_value) if quality_value else None

            results = batch_convert(
                [Path(p) for p in self.inputs],
                target_format=fmt,
                output_dir=(
                    Path(self.output_dir_var.get())
                    if self.output_dir_var.get()
                    else None
                ),
                quality=quality,
                resize=resize_spec,
                keep_metadata=self.keep_metadata_var.get(),
            )
            messagebox.showinfo("Image Converter", f"Converted {len(results)} file(s).")

    ConverterGUI().mainloop()


def configure_logging(level: int) -> None:
    if level >= 2:
        log_level = logging.DEBUG
    elif level == 1:
        log_level = logging.INFO
    else:
        log_level = logging.WARNING
    logging.basicConfig(level=log_level, format="%(levelname)s: %(message)s")


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    if args.gui:
        launch_gui()
        return 0

    configure_logging(args.verbose)

    resize_spec = args.resize
    keep_metadata = not args.strip_metadata
    inputs = [Path(p) for p in args.inputs]
    output_dir = Path(args.output_dir) if args.output_dir else None

    results = batch_convert(
        inputs,
        target_format=args.target_format,
        output_dir=output_dir,
        quality=args.quality,
        resize=resize_spec,
        recursive=args.recursive,
        keep_metadata=keep_metadata,
    )
    LOGGER.info("Converted %d file(s).", len(results))
    return 0 if results else 1


if __name__ == "__main__":  # pragma: no cover - manual invocation
    sys.exit(main())
