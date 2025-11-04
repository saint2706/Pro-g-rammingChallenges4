"""Encoding visualization utilities and CLI."""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

# Ensure we can import the conversion helpers even though the directory
# name contains spaces.
MODULE_DIR = Path(__file__).resolve().parent
if str(MODULE_DIR) not in sys.path:
    sys.path.insert(0, str(MODULE_DIR))

from txtToHexAndBin import bin_to_text, hex_to_text, text_to_bin, text_to_hex  # noqa: E402


class VisualizationData:
    """Structured representation of encoding information."""

    def __init__(
        self,
        *,
        text: str,
        encoding: str,
        byte_values: List[int],
        hex_values: List[str],
        binary_values: List[str],
        bit_matrix: List[List[int]],
        roundtrip: Dict[str, Any],
        comparisons: Dict[str, Any],
        metadata: Dict[str, Any],
    ) -> None:
        self.text = text
        self.encoding = encoding
        self.byte_values = byte_values
        self.hex_values = hex_values
        self.binary_values = binary_values
        self.bit_matrix = bit_matrix
        self.roundtrip = roundtrip
        self.comparisons = comparisons
        self.metadata = metadata

    def to_json_dict(self) -> Dict[str, Any]:
        """Return a plain dictionary suitable for JSON serialization."""
        return {
            "text": self.text,
            "encoding": self.encoding,
            "byte_values": self.byte_values,
            "hex_values": self.hex_values,
            "binary_values": self.binary_values,
            "bit_matrix": self.bit_matrix,
            "roundtrip": self.roundtrip,
            "comparisons": self.comparisons,
            "metadata": self.metadata,
        }


def _split_values(value: str) -> List[str]:
    return value.split() if value else []


def _to_bit_matrix(binary_values: Iterable[str]) -> List[List[int]]:
    return [[int(bit) for bit in bits] for bits in binary_values]


def _build_ascii_comparison(text: str, encoding: str) -> Dict[str, Any]:
    try:
        ascii_hex = text_to_hex(text, encoding="ascii", separator=" ")
        ascii_bin = text_to_bin(text, encoding="ascii", separator=" ")
    except UnicodeEncodeError:
        return {
            "encoding": "ascii",
            "supported": False,
            "reason": "Input cannot be encoded using ASCII.",
        }

    ascii_hex_values = _split_values(ascii_hex)
    ascii_binary_values = _split_values(ascii_bin)
    ascii_byte_values = [int(value, 16) for value in ascii_hex_values]

    return {
        "encoding": "ascii",
        "supported": True,
        "byte_values": ascii_byte_values,
        "hex_values": ascii_hex_values,
        "binary_values": ascii_binary_values,
        "differences": [],
        "note": "Input already encoded as ASCII."
        if encoding.lower() == "ascii"
        else None,
    }


def _merge_differences(
    encoded_bytes: List[int],
    ascii_comparison: Dict[str, Any],
) -> None:
    if not ascii_comparison.get("supported"):
        return

    ascii_bytes = ascii_comparison.get("byte_values", [])
    length = max(len(encoded_bytes), len(ascii_bytes))
    differences = []
    for index in range(length):
        encoded_byte: Optional[int] = (
            encoded_bytes[index] if index < len(encoded_bytes) else None
        )
        ascii_byte: Optional[int] = (
            ascii_bytes[index] if index < len(ascii_bytes) else None
        )
        delta: Optional[int]
        if encoded_byte is None or ascii_byte is None:
            delta = None
        else:
            delta = encoded_byte - ascii_byte
        differences.append(
            {
                "index": index,
                "byte": encoded_byte,
                "ascii_byte": ascii_byte,
                "delta": delta,
            }
        )
    ascii_comparison["differences"] = differences


def generate_visualization_data(
    text: str, encoding: str = "utf-8"
) -> VisualizationData:
    """Create structured data for visualising text encodings."""
    hex_string = text_to_hex(text, encoding=encoding, separator=" ")
    bin_string = text_to_bin(text, encoding=encoding, separator=" ")

    hex_values = _split_values(hex_string)
    binary_values = _split_values(bin_string)
    byte_values = [int(value, 16) for value in hex_values]
    bit_matrix = _to_bit_matrix(binary_values)

    roundtrip_hex = (
        hex_to_text(hex_string, encoding=encoding, separator=" ") if hex_values else ""
    )
    roundtrip_bin = (
        bin_to_text(bin_string, encoding=encoding, separator=" ")
        if binary_values
        else ""
    )

    ascii_comparison = _build_ascii_comparison(text, encoding)
    _merge_differences(byte_values, ascii_comparison)

    metadata = {
        "byte_count": len(byte_values),
        "bit_count": len(bit_matrix) * 8,
        "is_empty": not text,
    }

    return VisualizationData(
        text=text,
        encoding=encoding,
        byte_values=byte_values,
        hex_values=hex_values,
        binary_values=binary_values,
        bit_matrix=bit_matrix,
        roundtrip={
            "from_hex": roundtrip_hex,
            "from_bin": roundtrip_bin,
            "hex_matches": roundtrip_hex == text,
            "bin_matches": roundtrip_bin == text,
        },
        comparisons=ascii_comparison,
        metadata=metadata,
    )


def create_visualizations(
    data: VisualizationData,
    *,
    show: bool = False,
    save_path: Optional[Path] = None,
) -> None:
    """Render plots that visualise the encoding details."""
    try:
        import matplotlib

        matplotlib.use("Agg", force=True)
        import matplotlib.pyplot as plt  # type: ignore
    except ModuleNotFoundError as exc:  # pragma: no cover - dependency injection
        raise RuntimeError("matplotlib is required to generate plots") from exc

    if not data.byte_values:
        raise ValueError("Cannot visualise empty input text")

    fig, axes = plt.subplots(3, 1, figsize=(10, 12))

    indices = list(range(len(data.byte_values)))
    axes[0].bar(indices, data.byte_values, color="tab:blue")
    axes[0].set_title(f"Byte values ({data.encoding})")
    axes[0].set_xlabel("Byte index")
    axes[0].set_ylabel("Value")

    axes[1].imshow(data.bit_matrix, cmap="Blues", aspect="auto", vmin=0, vmax=1)
    axes[1].set_title("Bit matrix")
    axes[1].set_xlabel("Bit position")
    axes[1].set_ylabel("Byte index")
    axes[1].set_xticks(range(8))

    ascii_comparison = data.comparisons
    if ascii_comparison.get("supported") and ascii_comparison.get("byte_values"):
        comparison_bytes = ascii_comparison["byte_values"]
        length = max(len(data.byte_values), len(comparison_bytes))
        encoded_series = data.byte_values + [0] * (length - len(data.byte_values))
        ascii_series = comparison_bytes + [0] * (length - len(comparison_bytes))
        offset_indices = [index + 0.4 for index in range(length)]

        axes[2].bar(
            range(length),
            encoded_series,
            width=0.4,
            label=data.encoding,
            color="tab:purple",
        )
        axes[2].bar(
            offset_indices, ascii_series, width=0.4, label="ascii", color="tab:orange"
        )
        axes[2].set_title("Byte comparison with ASCII")
        axes[2].set_xlabel("Byte index")
        axes[2].set_ylabel("Value")
        axes[2].legend()
    else:
        axes[2].axis("off")
        message = ascii_comparison.get("reason", "ASCII comparison unavailable.")
        axes[2].text(0.5, 0.5, message, ha="center", va="center", fontsize=12)

    plt.tight_layout()

    if save_path is not None:
        save_path = Path(save_path)
        save_path.parent.mkdir(parents=True, exist_ok=True)
        fig.savefig(save_path, dpi=300)
    if show:
        plt.show()
    else:
        plt.close(fig)


def _parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Visualise text encodings")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--text", help="Text to encode")
    group.add_argument("--file", type=Path, help="Path to a text file to encode")
    parser.add_argument(
        "--encoding",
        default="utf-8",
        help="Character encoding to use for conversions (default: utf-8)",
    )
    parser.add_argument(
        "--json",
        type=str,
        help="Path to write JSON summary (use '-' for stdout)",
    )
    parser.add_argument(
        "--pretty",
        action="store_true",
        help="Pretty-print JSON output with indentation.",
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Display plots interactively.",
    )
    parser.add_argument(
        "--save-plot",
        type=Path,
        help="Save plots to the specified image file.",
    )
    return parser.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> VisualizationData:
    args = _parse_args(argv)

    if args.text is not None:
        text = args.text
    else:
        try:
            text = args.file.read_text(encoding=args.encoding)
        except FileNotFoundError as exc:  # pragma: no cover - handled by argparse
            raise SystemExit(str(exc)) from exc

    data = generate_visualization_data(text, encoding=args.encoding)

    if args.json:
        payload = json.dumps(data.to_json_dict(), indent=2 if args.pretty else None)
        if args.json == "-":
            print(payload)
        else:
            Path(args.json).write_text(payload, encoding="utf-8")

    if args.show or args.save_plot:
        create_visualizations(data, show=args.show, save_path=args.save_plot)

    return data


if __name__ == "__main__":
    main()
