"""Tests for the ImgToASCII converter CLI utilities."""

import importlib.util
import io
import sys
from pathlib import Path

import pytest

MODULE_PATH = Path(__file__).resolve().parents[1] / "challenges/Practical/ImgToASCII/convert.py"
MODULE_NAME = "img_to_ascii_convert"
SPEC = importlib.util.spec_from_file_location(MODULE_NAME, MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[MODULE_NAME] = MODULE
assert SPEC and SPEC.loader
SPEC.loader.exec_module(MODULE)  # type: ignore[attr-defined]


def test_convert_invalid_image_file_like(tmp_path: Path) -> None:
    cfg = MODULE.Config(input_path=tmp_path / "placeholder.png", output_path=None)
    bad_buffer = io.BytesIO(b"this is not an image")
    bad_buffer.name = "bad.png"

    with pytest.raises(MODULE.ImageOpenError, match="Failed to open image 'bad.png'"):
        MODULE.convert(cfg, image_source=bad_buffer)
