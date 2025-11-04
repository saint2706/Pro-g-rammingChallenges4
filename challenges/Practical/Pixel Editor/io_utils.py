"""Import/export helpers for the Pixel Editor."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, List, Optional, Sequence

from PIL import Image

from .core import (
    Palette,
    PaletteIndex,
    PixelDocument,
    PixelFrame,
    PixelLayer,
    TRANSPARENT,
    composite_to_rgba,
)


@dataclass
class SpriteSheetSpec:
    frame_width: int
    frame_height: int
    columns: Optional[int] = None
    rows: Optional[int] = None

    def __post_init__(self) -> None:
        if self.frame_width <= 0 or self.frame_height <= 0:
            raise ValueError("Frame dimensions must be positive")


def _to_image_data(
    composite: Sequence[Sequence[PaletteIndex]], palette: Palette
) -> Image.Image:
    rgba = composite_to_rgba(composite, palette)
    height = len(rgba)
    width = len(rgba[0]) if height else 0
    image = Image.new("RGBA", (width, height))
    flat = [channel for row in rgba for pixel in row for channel in pixel]
    image.putdata([tuple(flat[i : i + 4]) for i in range(0, len(flat), 4)])
    return image


def export_png_sprite_sheet(
    document: PixelDocument,
    destination: Path,
    frames: Optional[Iterable[int]] = None,
    frames_per_row: Optional[int] = None,
) -> Path:
    destination = Path(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    indices = list(frames) if frames is not None else list(range(len(document.frames)))
    if not indices:
        raise ValueError("No frames selected for export")

    frames_per_row = frames_per_row or len(indices)
    rows = (len(indices) + frames_per_row - 1) // frames_per_row
    sheet = Image.new(
        "RGBA", (document.width * frames_per_row, document.height * rows), (0, 0, 0, 0)
    )

    for i, frame_idx in enumerate(indices):
        composite = document.composite_frame(frame_idx)
        frame_img = _to_image_data(composite, document.palette)
        x = (i % frames_per_row) * document.width
        y = (i // frames_per_row) * document.height
        sheet.paste(frame_img, (x, y))

    sheet.save(destination, format="PNG")
    return destination


def export_gif(
    document: PixelDocument, destination: Path, duration: int = 120, loop: int = 0
) -> Path:
    destination = Path(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    frames: List[Image.Image] = []
    for frame_idx in range(len(document.frames)):
        composite = document.composite_frame(frame_idx)
        frame_img = _to_image_data(composite, document.palette).convert(
            "P", palette=Image.ADAPTIVE, colors=256
        )
        frames.append(frame_img)

    if not frames:
        raise ValueError("Document has no frames to export")

    first, *rest = frames
    first.save(
        destination,
        format="GIF",
        save_all=True,
        append_images=rest,
        duration=duration,
        loop=loop,
        transparency=0,
    )
    return destination


def import_sprite_sheet(
    path: Path, spec: SpriteSheetSpec, palette: Optional[Palette] = None
) -> PixelDocument:
    path = Path(path)
    if not path.exists():
        raise FileNotFoundError(path)

    image = Image.open(path).convert("RGBA")
    palette = palette or Palette()

    columns = spec.columns or max(1, image.width // spec.frame_width)
    rows = spec.rows or max(1, image.height // spec.frame_height)

    document = PixelDocument(
        spec.frame_width, spec.frame_height, palette=palette, frames=[]
    )

    for row in range(rows):
        for col in range(columns):
            left = col * spec.frame_width
            top = row * spec.frame_height
            if left >= image.width or top >= image.height:
                continue
            box = (left, top, left + spec.frame_width, top + spec.frame_height)
            frame_region = image.crop(box)
            pixels = frame_region.load()
            frame = PixelFrame(spec.frame_width, spec.frame_height, layers=[])
            base_layer = PixelLayer(spec.frame_width, spec.frame_height, name="Layer 1")
            for y in range(spec.frame_height):
                for x in range(spec.frame_width):
                    if x >= frame_region.width or y >= frame_region.height:
                        continue
                    r, g, b, a = pixels[x, y]
                    if a == 0:
                        base_layer.pixels[y][x] = TRANSPARENT
                    else:
                        hex_color = f"#{r:02x}{g:02x}{b:02x}"
                        index = palette.add(hex_color)
                        base_layer.pixels[y][x] = index
            frame.layers.append(base_layer)
            document.frames.append(frame)

    if not document.frames:
        document.frames.append(PixelFrame(spec.frame_width, spec.frame_height))

    return document
