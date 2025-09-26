import importlib.util
import sys
from pathlib import Path

import pytest

MODULE_DIR = Path(__file__).resolve().parents[1]
CORE_PATH = MODULE_DIR / "core.py"
spec = importlib.util.spec_from_file_location("pixel_editor_core", CORE_PATH)
core = importlib.util.module_from_spec(spec)
assert spec.loader is not None
sys.modules[spec.name] = core
spec.loader.exec_module(core)

Palette = core.Palette
PixelDocument = core.PixelDocument
PixelFrame = core.PixelFrame
PixelLayer = core.PixelLayer
TRANSPARENT = core.TRANSPARENT
composite_to_rgba = core.composite_to_rgba


def test_palette_add_remove_swap():
    palette = Palette(["#000000", "#ffffff"])
    idx_red = palette.add("#ff0000")
    assert idx_red == 2
    assert palette.get(idx_red) == "#ff0000"

    palette.swap(0, 2)
    assert palette.get(0) == "#ff0000"

    removed = palette.remove(1)
    assert removed == "#ffffff"
    assert len(palette.colors) == 2


def test_pixel_layer_set_and_get():
    layer = PixelLayer(4, 4, name="Test")
    previous = layer.set_pixel(1, 1, 3)
    assert previous == TRANSPARENT
    assert layer.get_pixel(1, 1) == 3

    with pytest.raises(IndexError):
        layer.set_pixel(5, 0, 1)


def test_frame_composite_respects_visibility():
    frame = PixelFrame(2, 1, layers=[])
    base = frame.add_layer(name="Base")
    overlay = frame.add_layer(name="Overlay")
    base.set_pixel(0, 0, 1)
    overlay.set_pixel(1, 0, 2)
    overlay.visible = False

    composite = frame.composite()
    assert composite == [[1, TRANSPARENT]]

    overlay.visible = True
    composite = frame.composite()
    assert composite == [[1, 2]]


def test_document_frame_management_and_palette():
    document = PixelDocument(2, 2)
    document.frames[0].layers[0].set_pixel(0, 0, 0)

    clone = document.add_frame(clone_index=0)
    assert clone.layers[0].get_pixel(0, 0) == 0

    document.remove_palette_color(0)
    assert clone.layers[0].get_pixel(0, 0) == TRANSPARENT

    document.remove_frame(1)
    with pytest.raises(ValueError):
        document.remove_frame(0)


def test_composite_to_rgba_handles_alpha():
    palette = Palette(["#ff0000", "#00ff007f"])
    composite = [[0, 1, TRANSPARENT]]
    rgba = composite_to_rgba(composite, palette)
    assert rgba[0][0] == (255, 0, 0, 255)
    assert rgba[0][1] == (0, 255, 0, 127)
    assert rgba[0][2] == (0, 0, 0, 0)
