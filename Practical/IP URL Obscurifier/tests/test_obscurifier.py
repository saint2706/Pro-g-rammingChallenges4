import importlib.util
import sys
from pathlib import Path

MODULE_PATH = Path(__file__).resolve().parents[1] / "obscurifier.py"
spec = importlib.util.spec_from_file_location("obscurifier", MODULE_PATH)
obscurifier = importlib.util.module_from_spec(spec)
sys.modules.setdefault(spec.name, obscurifier)
spec.loader.exec_module(obscurifier)  # type: ignore[union-attr]


def test_generate_ipv4_variants_basic():
    bundle = obscurifier.generate_ipv4_variants(
        "192.168.0.1", include_default_mixes=False
    )
    assert bundle.canonical == "192.168.0.1"
    assert bundle.integers["decimal"] == "3232235521"
    assert bundle.integers["hex"] == "0xC0A80001"
    assert bundle.dotted["hex"] == "0xC0.0xA8.0x00.0x01"
    assert bundle.dotted["octal"] == "0300.0250.0000.0001"


def test_decode_ipv4_handles_hex_integer():
    canonical, breakdown = obscurifier.decode_ipv4("0xC0A80001")
    assert canonical == "192.168.0.1"
    assert breakdown[0][1] == "hex"


def test_decode_ipv4_handles_mixed_dotted():
    canonical, breakdown = obscurifier.decode_ipv4("0300.0xA8.0b00000000.0001")
    assert canonical == "192.168.0.1"
    bases = [entry[1] for entry in breakdown]
    assert bases == ["octal", "hex", "binary", "zero-padded"]


def test_encode_url_with_credentials():
    bundle, variants = obscurifier.encode_url(
        "http://192.168.0.1/admin",
        credentials="support:ticket",
        include_default_mixes=False,
    )
    variant_map = dict(variants)
    assert bundle.canonical == "192.168.0.1"
    assert variant_map["integer-decimal"].startswith("http://support:ticket@3232235521")
    assert variant_map["dotted-hex"].startswith("http://support:ticket@0xc0.0xa8")


def test_decode_url_reveals_credentials():
    data = obscurifier.decode_url("http://user:pass@0xC0A80001/login")
    assert data["canonical_host"] == "192.168.0.1"
    assert data["has_credentials"] is True
    assert data["credentials"]["username"] == "user"
    assert data["credentials"]["password"] == "pass"
    assert data["canonical_url"].startswith("http://192.168.0.1")
