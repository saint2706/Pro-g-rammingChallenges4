"""Core data structures for the Pixel Editor project.

The module keeps the pixel art model completely toolkit-agnostic so that
it can be used from GUIs, command line tools, or automated pipelines. The
classes below are purposely lightweight and only depend on the Python
standard library to simplify testing.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Iterable, List, Optional, Sequence, Tuple

Color = str  # Hex string "#RRGGBB" or "#RRGGBBAA"
PaletteIndex = int

TRANSPARENT: PaletteIndex = -1


def _ensure_bounds(value: int, minimum: int, maximum: int) -> int:
    if not minimum <= value < maximum:
        raise IndexError(f"Value {value} out of bounds [{minimum}, {maximum})")
    return value


@dataclass
class Palette:
    """Palette that stores colors as hex strings."""

    colors: List[Color] = field(default_factory=lambda: [
        "#000000",
        "#ffffff",
        "#d32f2f",
        "#fbc02d",
        "#388e3c",
        "#1976d2",
        "#7b1fa2",
        "#ff9800",
    ])

    def add(self, color: Color) -> PaletteIndex:
        """Append ``color`` to the palette and return its index."""

        if color not in self.colors:
            self.colors.append(color)
        return self.colors.index(color)

    def remove(self, index: PaletteIndex) -> Color:
        _ensure_bounds(index, 0, len(self.colors))
        return self.colors.pop(index)

    def swap(self, first: PaletteIndex, second: PaletteIndex) -> None:
        _ensure_bounds(first, 0, len(self.colors))
        _ensure_bounds(second, 0, len(self.colors))
        self.colors[first], self.colors[second] = self.colors[second], self.colors[first]

    def get(self, index: PaletteIndex) -> Color:
        _ensure_bounds(index, 0, len(self.colors))
        return self.colors[index]


@dataclass
class PixelLayer:
    """A single 2D grid of palette indices."""

    width: int
    height: int
    name: str = "Layer"
    visible: bool = True
    pixels: List[List[PaletteIndex]] = field(init=False)

    def __post_init__(self) -> None:
        self.pixels = [[TRANSPARENT for _ in range(self.width)] for _ in range(self.height)]

    def clone(self, name: Optional[str] = None) -> "PixelLayer":
        clone = PixelLayer(self.width, self.height, name=name or self.name, visible=self.visible)
        clone.pixels = [row[:] for row in self.pixels]
        return clone

    def clear(self) -> None:
        for row in self.pixels:
            for x in range(self.width):
                row[x] = TRANSPARENT

    def set_pixel(self, x: int, y: int, value: PaletteIndex) -> PaletteIndex:
        _ensure_bounds(x, 0, self.width)
        _ensure_bounds(y, 0, self.height)
        previous = self.pixels[y][x]
        self.pixels[y][x] = value
        return previous

    def get_pixel(self, x: int, y: int) -> PaletteIndex:
        _ensure_bounds(x, 0, self.width)
        _ensure_bounds(y, 0, self.height)
        return self.pixels[y][x]


@dataclass
class PixelFrame:
    """Collection of layers representing a single animation frame."""

    width: int
    height: int
    layers: List[PixelLayer] = field(default_factory=list)

    def __post_init__(self) -> None:
        if not self.layers:
            self.layers.append(PixelLayer(self.width, self.height, name="Layer 1"))

    def add_layer(self, name: Optional[str] = None, index: Optional[int] = None) -> PixelLayer:
        layer = PixelLayer(self.width, self.height, name=name or f"Layer {len(self.layers)+1}")
        if index is None:
            self.layers.append(layer)
        else:
            _ensure_bounds(index, 0, len(self.layers) + 1)
            self.layers.insert(index, layer)
        return layer

    def remove_layer(self, index: int) -> PixelLayer:
        _ensure_bounds(index, 0, len(self.layers))
        if len(self.layers) == 1:
            raise ValueError("Cannot delete the final layer")
        return self.layers.pop(index)

    def composite(self) -> List[List[PaletteIndex]]:
        """Flatten visible layers into a single 2D list of palette indices."""

        flat = [[TRANSPARENT for _ in range(self.width)] for _ in range(self.height)]
        for layer in self.layers:
            if not layer.visible:
                continue
            for y in range(self.height):
                for x in range(self.width):
                    if layer.pixels[y][x] != TRANSPARENT:
                        flat[y][x] = layer.pixels[y][x]
        return flat

    def clone(self) -> "PixelFrame":
        return PixelFrame(self.width, self.height, layers=[layer.clone() for layer in self.layers])


@dataclass
class PixelDocument:
    """Represents a sprite project with a palette and animation frames."""

    width: int
    height: int
    palette: Palette = field(default_factory=Palette)
    frames: List[PixelFrame] = field(default_factory=list)

    def __post_init__(self) -> None:
        if not self.frames:
            self.frames.append(PixelFrame(self.width, self.height))

    def add_frame(self, clone_index: Optional[int] = None) -> PixelFrame:
        if clone_index is not None:
            _ensure_bounds(clone_index, 0, len(self.frames))
            frame = self.frames[clone_index].clone()
        else:
            frame = PixelFrame(self.width, self.height)
        self.frames.append(frame)
        return frame

    def remove_frame(self, index: int) -> PixelFrame:
        _ensure_bounds(index, 0, len(self.frames))
        if len(self.frames) == 1:
            raise ValueError("Cannot delete the final frame")
        return self.frames.pop(index)

    def composite_frame(self, index: int) -> List[List[PaletteIndex]]:
        _ensure_bounds(index, 0, len(self.frames))
        return self.frames[index].composite()

    def resize(self, width: int, height: int) -> None:
        if width <= 0 or height <= 0:
            raise ValueError("Dimensions must be positive")
        self.width = width
        self.height = height
        for frame in self.frames:
            for layer in frame.layers:
                new_pixels = [[TRANSPARENT for _ in range(width)] for _ in range(height)]
                for y in range(min(height, len(layer.pixels))):
                    for x in range(min(width, len(layer.pixels[0]))):
                        new_pixels[y][x] = layer.pixels[y][x]
                layer.width = width
                layer.height = height
                layer.pixels = new_pixels

    def iter_flattened(self) -> Iterable[List[List[PaletteIndex]]]:
        for frame in self.frames:
            yield frame.composite()

    def add_palette_color(self, color: Color) -> PaletteIndex:
        return self.palette.add(color)

    def remove_palette_color(self, index: int) -> None:
        self.palette.remove(index)
        for frame in self.frames:
            for layer in frame.layers:
                for y in range(layer.height):
                    for x in range(layer.width):
                        value = layer.pixels[y][x]
                        if value == TRANSPARENT:
                            continue
                        if value == index:
                            layer.pixels[y][x] = TRANSPARENT
                        elif value > index:
                            layer.pixels[y][x] = value - 1

    def swap_palette_colors(self, first: int, second: int) -> None:
        if first == second:
            return
        self.palette.swap(first, second)
        for frame in self.frames:
            for layer in frame.layers:
                for y in range(layer.height):
                    for x in range(layer.width):
                        value = layer.pixels[y][x]
                        if value == first:
                            layer.pixels[y][x] = second
                        elif value == second:
                            layer.pixels[y][x] = first


def composite_to_rgba(
    composite: Sequence[Sequence[PaletteIndex]], palette: Palette, background: Tuple[int, int, int, int] = (0, 0, 0, 0)
) -> List[List[Tuple[int, int, int, int]]]:
    """Convert a flattened frame into RGBA tuples."""

    rgba: List[List[Tuple[int, int, int, int]]] = []
    for row in composite:
        rgba_row: List[Tuple[int, int, int, int]] = []
        for index in row:
            if index == TRANSPARENT:
                rgba_row.append(background)
            else:
                color = palette.get(index)
                if len(color) == 9:  # #RRGGBBAA
                    r = int(color[1:3], 16)
                    g = int(color[3:5], 16)
                    b = int(color[5:7], 16)
                    a = int(color[7:9], 16)
                else:
                    r = int(color[1:3], 16)
                    g = int(color[3:5], 16)
                    b = int(color[5:7], 16)
                    a = 255
                rgba_row.append((r, g, b, a))
        rgba.append(rgba_row)
    return rgba
