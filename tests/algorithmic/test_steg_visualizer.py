import json
import sys
from pathlib import Path

import pytest

try:  # Pillow is optional; CLI tests skip if it is not available.
    from PIL import Image
except ImportError:  # pragma: no cover - exercised when Pillow missing.
    Image = None


PROJECT_DIR = Path(__file__).resolve().parents[2] / "challenges" / "Algorithmic" / "Steganography"
if str(PROJECT_DIR) not in sys.path:
    sys.path.insert(0, str(PROJECT_DIR))


from steg_visualizer import analyse_image_pair, compute_diff_metrics, main  # noqa: E402


def _save_pixels(path: Path, pixels: list[list[list[int]]]) -> None:
    if Image is None:  # pragma: no cover - helper guarded by skip in tests.
        raise RuntimeError("Pillow is required to save images in this test")
    height = len(pixels)
    width = len(pixels[0]) if height else 0
    img = Image.new("RGB", (width, height))
    flat = [tuple(pixels[y][x]) for y in range(height) for x in range(width)]
    img.putdata(flat)
    img.save(path)


def test_compute_diff_metrics_counts_expected_bits():
    cover = [
        [[0, 0, 0], [10, 10, 10]],
        [[20, 20, 20], [30, 30, 30]],
    ]
    stego = [
        [[0, 0, 0], [10, 10, 10]],
        [[20, 20, 20], [30, 30, 30]],
    ]
    stego[0][0][0] ^= 1  # Flip red LSB in pixel (0,0)
    stego[1][1][2] ^= 2  # Flip blue bit-1 in pixel (1,1)

    metrics, mask = compute_diff_metrics(cover, stego)

    assert metrics["width"] == 2
    assert metrics["height"] == 2
    assert metrics["total_pixels"] == 4
    assert metrics["modified_pixels"] == 2
    assert metrics["bit_planes"]["aggregate"][0] == 1
    assert metrics["bit_planes"]["aggregate"][1] == 1
    assert metrics["per_channel"]["R"]["bit_counts"][0] == 1
    assert metrics["per_channel"]["B"]["bit_counts"][1] == 1
    # Two distinct pixels should be flagged as changed in the mask
    assert sum(sum(1 for value in row if value) for row in mask) == 2


def test_cli_json_diff_metrics(tmp_path, capsys):
    if Image is None:
        pytest.skip("Pillow not installed")
    cover_path = tmp_path / "cover.png"
    stego_path = tmp_path / "stego.png"

    cover = [
        [[0, 0, 0], [0, 0, 0]],
        [[0, 0, 0], [0, 0, 0]],
    ]
    stego = [
        [[0, 1, 0], [0, 0, 0]],
        [[0, 0, 0], [0, 0, 2]],
    ]

    _save_pixels(cover_path, cover)
    _save_pixels(stego_path, stego)

    rc = main([str(cover_path), str(stego_path), "--json", "-"])
    assert rc == 0
    out = capsys.readouterr().out
    payload = json.loads(out)

    assert payload["modified_pixels"] == 2
    assert payload["bit_planes"]["aggregate"][0] == 1
    assert payload["bit_planes"]["aggregate"][1] == 1

    _, _, analysed_metrics, mask = analyse_image_pair(cover_path, stego_path)
    assert analysed_metrics["modified_pixels"] == 2
    assert analysed_metrics["per_channel"]["G"]["bit_counts"][0] == 1
    assert analysed_metrics["per_channel"]["B"]["bit_counts"][1] == 1
    assert sum(sum(1 for value in row if value) for row in mask) == 2
