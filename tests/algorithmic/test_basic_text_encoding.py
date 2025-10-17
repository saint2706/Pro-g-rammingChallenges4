import importlib.util
from pathlib import Path

import pytest

MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "basic text encoding"
    / "txtToHexAndBin.py"
)

spec = importlib.util.spec_from_file_location("txtToHexAndBin", MODULE_PATH)
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


@pytest.mark.parametrize(
    "text, expected",
    [
        ("Hi", "48 69"),
        ("⚡", "e2 9a a1"),
    ],
)
def test_text_to_hex_roundtrip(text, expected):
    hex_value = module.text_to_hex(text, separator=" ")
    assert hex_value == expected
    assert module.hex_to_text(expected, separator=" ") == text


def test_hex_without_separator_roundtrip():
    text = "data"
    hex_value = module.text_to_hex(text, separator="")
    assert hex_value == "64617461"
    assert module.hex_to_text(hex_value, separator="") == text


def test_text_to_bin_roundtrip():
    text = "A"
    binary = module.text_to_bin(text, separator="")
    assert binary == "01000001"
    assert module.bin_to_text(binary, separator="") == text


def test_invalid_encoding():
    with pytest.raises(ValueError):
        module.text_to_hex("test", encoding="invalid-encoding")


@pytest.mark.parametrize(
    "value, separator, decoder",
    [
        ("01002", "", module.bin_to_text),
        ("zz", " ", module.hex_to_text),
    ],
)
def test_invalid_binary_or_hex(value, separator, decoder):
    with pytest.raises(ValueError):
        decoder(value, separator=separator)


def test_invalid_hex_chunk_alignment():
    with pytest.raises(ValueError):
        module.hex_to_text("abc", separator="")


def test_text_to_base_requires_string():
    with pytest.raises(TypeError):
        module.text_to_hex(123)  # type: ignore[arg-type]
