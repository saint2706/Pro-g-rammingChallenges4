import importlib.util
import sys
from pathlib import Path

import pytest
from PIL import Image

MODULE_PATH = Path(__file__).resolve().parents[1] / "convert.py"
MODULE_NAME = "image_converter"
SPEC = importlib.util.spec_from_file_location(MODULE_NAME, MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
sys.modules[MODULE_NAME] = MODULE
assert SPEC and SPEC.loader  # for type checkers
SPEC.loader.exec_module(MODULE)  # type: ignore[attr-defined]


@pytest.fixture
def sample_jpeg(tmp_path: Path) -> Path:
    image = Image.new("RGB", (800, 600), color="navy")
    exif = Image.Exif()
    exif[0x010E] = "Unit Test Description"
    path = tmp_path / "sample.jpg"
    image.save(path, "JPEG", exif=exif)
    return path


def test_convert_image_resizes_and_preserves_exif(sample_jpeg: Path, tmp_path: Path) -> None:
    output_dir = tmp_path / "converted"
    output_dir.mkdir()
    resize_spec = MODULE.ResizeSpec(width=400)
    output_path = MODULE.convert_image(
        sample_jpeg,
        target_format="jpeg",
        output_path=output_dir / "resized.jpg",
        resize=resize_spec,
        keep_metadata=True,
    )

    assert output_path.exists()
    with Image.open(output_path) as result:
        assert result.size == (400, 300)
        exif = result.getexif()
        assert exif[0x010E] == "Unit Test Description"


def test_batch_convert_directory(tmp_path: Path) -> None:
    inputs_dir = tmp_path / "inputs"
    inputs_dir.mkdir()
    nested = inputs_dir / "nested"
    nested.mkdir()

    for idx, target_dir in enumerate([inputs_dir, nested], start=1):
        img = Image.new("RGBA", (200 + idx * 10, 200), color=(0, idx * 50, 200, 255))
        img.save(target_dir / f"img{idx}.png")

    output_dir = tmp_path / "outputs"
    resize_spec = MODULE.ResizeSpec(height=128)
    results = MODULE.batch_convert(
        [inputs_dir],
        target_format="png",
        output_dir=output_dir,
        resize=resize_spec,
        recursive=True,
    )

    assert len(results) == 2
    for path in results:
        assert path.exists()
        with Image.open(path) as image:
            assert image.height == 128
