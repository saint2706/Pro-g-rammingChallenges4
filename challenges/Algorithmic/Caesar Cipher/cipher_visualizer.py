#!/usr/bin/env python3
"""Cipher visualization utilities for classical substitution ciphers.

This module bridges several challenge implementations so that a single entry
point can generate substitution mappings, frequency analysis data, and optional
Plotly visualisations.  It is designed to run both as a CLI/GUI helper and as a
library that returns pure data structures suitable for unit testing.

Supported ciphers
-----------------
* Caesar (letters/alphanumeric/printable alphabets)
* ROT13
* Vigenère
* Affine (with optional brute-force search)
* Atbash
"""

from __future__ import annotations

import argparse
import json
import string
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional

try:
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
except ModuleNotFoundError:  # pragma: no cover - optional dependency
    go = None  # type: ignore[assignment]
    make_subplots = None  # type: ignore[assignment]

# -----------------------------------------------------------------------------
# Import cipher implementations from sibling challenge folders
# -----------------------------------------------------------------------------
CURRENT_DIR = Path(__file__).resolve().parent
ALGORITHM_DIR = CURRENT_DIR.parent

ROT13_DIR = ALGORITHM_DIR / "ROT 13"
RUMKIN_DIR = ALGORITHM_DIR / "Rumkin Ciphers"
VIG_DIR = ALGORITHM_DIR / "Vigniere Cipher"

for extra in (ROT13_DIR, RUMKIN_DIR, VIG_DIR):
    if str(extra) not in sys.path:
        sys.path.insert(0, str(extra))

from caesar import AlphabetType, caesar_cipher  # type: ignore  # noqa: E402
from rot13 import rot13  # type: ignore  # noqa: E402
from vig import sanitize_key, vigenere_cipher  # type: ignore  # noqa: E402
from affine import (  # type: ignore  # noqa: E402
    VALID_A_VALUES,
    affine_cipher,
    brute_force_affine,
    modular_inverse,
)
from atbash import atbash_cipher  # type: ignore  # noqa: E402


ALPHABET: List[str] = list(string.ascii_uppercase)
DEFAULT_TEXT = "The quick brown fox jumps over the lazy dog."


@dataclass(slots=True)
class VisualizationConfig:
    """Configuration container for generating visualisation data."""

    cipher: str
    mode: str = "encrypt"
    text: str = DEFAULT_TEXT
    shift: int = 3
    alphabet_type: AlphabetType = AlphabetType.LETTERS_ONLY
    key: Optional[str] = None
    a: Optional[int] = None
    b: Optional[int] = None
    include_bruteforce: bool = False
    max_bruteforce_results: int = 10


def _build_substitution_matrix(mappings: Iterable[Dict[str, str]]) -> List[List[int]]:
    matrix = [[0 for _ in ALPHABET] for _ in ALPHABET]
    for mapping in mappings:
        for src, dst in mapping.items():
            if src not in ALPHABET or dst not in ALPHABET:
                continue
            i = ord(src) - ord("A")
            j = ord(dst) - ord("A")
            matrix[i][j] += 1
    return matrix


def _normalise_matrix(matrix: List[List[int]]) -> List[List[float]]:
    max_val = max((value for row in matrix for value in row), default=0)
    if max_val == 0:
        return [[0.0 for _ in row] for row in matrix]
    return [[value / max_val for value in row] for row in matrix]


def _frequency_profile(text: str) -> Dict[str, List[float]]:
    counts = {letter: 0 for letter in ALPHABET}
    for ch in text.upper():
        if ch in counts:
            counts[ch] += 1
    total = sum(counts.values())
    normalised = [counts[letter] / total if total else 0.0 for letter in ALPHABET]
    return {
        "alphabet": ALPHABET,
        "counts": [counts[letter] for letter in ALPHABET],
        "normalized": normalised,
        "total_letters": total,
    }


def _mapping_for_caesar(cfg: VisualizationConfig) -> Dict[str, str]:
    shift = cfg.shift if cfg.mode == "encrypt" else -cfg.shift
    return {
        letter: caesar_cipher(letter, shift, cfg.alphabet_type) for letter in ALPHABET
    }


def _mapping_for_rot13() -> Dict[str, str]:
    return {letter: rot13(letter) for letter in ALPHABET}


def _mapping_for_vigenere(cfg: VisualizationConfig) -> List[Dict[str, str]]:
    assert cfg.key is not None
    sanitized = sanitize_key(cfg.key)
    if not sanitized:
        raise ValueError("Vigenère key must contain alphabetic characters")
    mappings: List[Dict[str, str]] = []
    for key_char in sanitized:
        shift = ord(key_char) - ord("a")
        if cfg.mode == "decrypt":
            shift = (-shift) % 26
        mapping = {
            letter: chr((ord(letter) - ord("A") + shift) % 26 + ord("A"))
            for letter in ALPHABET
        }
        mappings.append(mapping)
    return mappings


def _mapping_for_affine(cfg: VisualizationConfig) -> Dict[str, str]:
    if cfg.a is None or cfg.b is None:
        raise ValueError("Affine cipher requires keys 'a' and 'b'")
    if cfg.a not in VALID_A_VALUES:
        raise ValueError(f"Key 'a' must be coprime with 26; got {cfg.a}")
    inv_a = modular_inverse(cfg.a, 26)
    if inv_a is None:
        raise ValueError(f"Key 'a'={cfg.a} has no modular inverse modulo 26")
    mapping: Dict[str, str] = {}
    for letter in ALPHABET:
        idx = ord(letter) - ord("A")
        if cfg.mode == "encrypt":
            mapped_idx = (cfg.a * idx + cfg.b) % 26
        else:
            mapped_idx = (inv_a * (idx - cfg.b)) % 26
        mapping[letter] = chr(mapped_idx + ord("A"))
    return mapping


def _mapping_for_atbash() -> Dict[str, str]:
    return {letter: atbash_cipher(letter) for letter in ALPHABET}


def generate_visualization_data(cfg: VisualizationConfig) -> Dict[str, object]:
    """Produce mapping tables and frequency statistics for the requested cipher."""

    cipher = cfg.cipher.lower()
    mode = cfg.mode.lower()
    if mode not in {"encrypt", "decrypt"}:
        raise ValueError("mode must be 'encrypt' or 'decrypt'")

    sanitized_key: Optional[str] = None

    if cipher == "caesar":
        mapping = _mapping_for_caesar(cfg)
        mapping_list = [mapping]
        output_text = caesar_cipher(
            cfg.text,
            cfg.shift if mode == "encrypt" else -cfg.shift,
            cfg.alphabet_type,
        )
        parameters = {
            "shift": cfg.shift,
            "alphabet_type": cfg.alphabet_type.value,
        }
    elif cipher == "rot13":
        mapping = _mapping_for_rot13()
        mapping_list = [mapping]
        output_text = rot13(cfg.text)
        parameters = {}
    elif cipher == "vigenere":
        if not cfg.key:
            raise ValueError("Vigenère cipher requires --key")
        mapping_list = _mapping_for_vigenere(cfg)
        mapping = mapping_list[0]
        output_text = vigenere_cipher(cfg.text, cfg.key, mode)
        sanitized_key = sanitize_key(cfg.key)
        parameters = {"key": cfg.key, "sanitized_key": sanitized_key}
    elif cipher == "affine":
        mapping = _mapping_for_affine(cfg)
        mapping_list = [mapping]
        if cfg.a is None or cfg.b is None:
            raise ValueError("Affine cipher requires both --a and --b keys")
        output_text = affine_cipher(cfg.text, cfg.a, cfg.b, mode)
        parameters = {"a": cfg.a, "b": cfg.b}
    elif cipher == "atbash":
        mapping = _mapping_for_atbash()
        mapping_list = [mapping]
        output_text = atbash_cipher(cfg.text)
        parameters = {}
    else:
        raise ValueError(f"Unsupported cipher '{cfg.cipher}'")

    matrix = _build_substitution_matrix(mapping_list)
    normalised_matrix = _normalise_matrix(matrix)

    input_freq = _frequency_profile(cfg.text)
    output_freq = _frequency_profile(output_text)

    data: Dict[str, object] = {
        "cipher": cipher,
        "mode": mode,
        "parameters": parameters,
        "input_text": cfg.text,
        "output_text": output_text,
        "mapping": {
            "alphabet": ALPHABET,
            "permutation": mapping if len(mapping_list) == 1 else None,
            "key_mappings": (
                [
                    {
                        "key_letter": sanitized_key[idx].upper(),
                        "mapping": mapping_dict,
                    }
                    for idx, mapping_dict in enumerate(mapping_list)
                ]
                if cipher == "vigenere" and sanitized_key is not None
                else None
            ),
            "matrix": matrix,
            "matrix_normalized": normalised_matrix,
        },
        "frequencies": {
            "input": input_freq,
            "output": output_freq,
        },
    }

    if cipher == "affine" and cfg.include_bruteforce and cfg.mode == "decrypt":
        brute_results = brute_force_affine(cfg.text)
        trimmed = [
            {"a": a, "b": b, "plaintext": plain}
            for a, b, plain in brute_results[: cfg.max_bruteforce_results]
        ]
        data["affine_bruteforce"] = {
            "reported": len(trimmed),
            "total_considered": len(brute_results),
            "results": trimmed,
        }

    return data


def build_plotly_figure(data: Dict[str, object]) -> go.Figure:
    """Construct a Plotly figure showing substitution matrix and frequencies."""

    if go is None or make_subplots is None:  # pragma: no cover - requires plotly
        raise RuntimeError("Plotly is not installed; install plotly to build figures")

    mapping = data["mapping"]  # type: ignore[assignment]
    frequencies = data["frequencies"]  # type: ignore[assignment]

    heatmap = go.Heatmap(
        z=mapping["matrix_normalized"],
        x=mapping["alphabet"],
        y=mapping["alphabet"],
        colorscale="Viridis",
        colorbar=dict(title="Normalized weight"),
    )

    freq_bar = go.Bar(
        x=mapping["alphabet"],
        y=frequencies["output"]["counts"],
        name="Output letter counts",
    )

    fig = make_subplots(
        rows=1,
        cols=2,
        subplot_titles=("Substitution map", "Output frequencies"),
    )
    fig.add_trace(heatmap, row=1, col=1)
    fig.add_trace(freq_bar, row=1, col=2)
    fig.update_layout(
        title=f"{data['cipher'].title()} cipher ({data['mode']})",
        xaxis_title="Cipher letter",
        yaxis_title="Plain letter",
        showlegend=False,
        template="plotly_white",
    )
    fig.update_xaxes(title_text="Letters", row=1, col=2)
    fig.update_yaxes(title_text="Count", row=1, col=2)
    return fig


def _read_text(args: argparse.Namespace) -> str:
    if args.text:
        return args.text
    if args.file:
        return Path(args.file).read_text(encoding="utf-8")
    return DEFAULT_TEXT


def _parse_args(argv: Optional[List[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate visualisations and data summaries for classic ciphers",
    )
    parser.add_argument(
        "--cipher",
        required=True,
        choices=["caesar", "rot13", "vigenere", "affine", "atbash"],
        help="Cipher to visualise",
    )
    parser.add_argument(
        "--mode",
        choices=["encrypt", "decrypt"],
        default="encrypt",
        help="Transformation direction",
    )
    parser.add_argument("-t", "--text", help="Inline text to transform")
    parser.add_argument("--file", help="Read input text from a UTF-8 file")
    parser.add_argument(
        "--alphabet",
        choices=[at.value for at in AlphabetType],
        default=AlphabetType.LETTERS_ONLY.value,
        help="Alphabet preset for the Caesar cipher",
    )
    parser.add_argument("--shift", type=int, default=3, help="Shift value for Caesar")
    parser.add_argument("--key", help="Key for Vigenère cipher")
    parser.add_argument("--a", type=int, help="Affine 'a' coefficient")
    parser.add_argument("--b", type=int, help="Affine 'b' shift")
    parser.add_argument(
        "--bruteforce",
        action="store_true",
        help="Include affine brute-force summaries when decrypting",
    )
    parser.add_argument(
        "--max-bruteforce",
        type=int,
        default=10,
        help="Limit number of brute-force rows included in JSON",
    )
    parser.add_argument("--output-json", help="Path to write JSON data payload")
    parser.add_argument("--output-html", help="Write Plotly figure to HTML file")
    parser.add_argument(
        "--output-svg", help="Write Plotly figure to SVG (requires kaleido)"
    )
    parser.add_argument(
        "--show",
        action="store_true",
        help="Display the Plotly figure in an interactive window",
    )
    parser.add_argument(
        "--no-plot",
        action="store_true",
        help="Skip Plotly figure generation (data only)",
    )
    parser.add_argument(
        "--pretty",
        action="store_true",
        help="Pretty-print JSON to stdout when no output path is supplied",
    )
    return parser.parse_args(argv)


def main(argv: Optional[List[str]] = None) -> int:
    args = _parse_args(argv)

    cfg = VisualizationConfig(
        cipher=args.cipher,
        mode=args.mode,
        text=_read_text(args),
        shift=args.shift,
        alphabet_type=AlphabetType(args.alphabet),
        key=args.key,
        a=args.a,
        b=args.b,
        include_bruteforce=args.bruteforce,
        max_bruteforce_results=args.max_bruteforce,
    )

    try:
        data = generate_visualization_data(cfg)
    except Exception as exc:  # pragma: no cover - user error path
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    json_payload = json.dumps(data, indent=2 if args.pretty else None)

    if args.output_json:
        Path(args.output_json).write_text(json_payload + "\n", encoding="utf-8")
    else:
        print(json_payload)

    if not args.no_plot:
        if go is None or make_subplots is None:
            print(
                "Warning: Plotly not installed; skipping figure generation",
                file=sys.stderr,
            )
        else:
            fig = build_plotly_figure(data)
            if args.output_html:
                fig.write_html(args.output_html)
            if args.output_svg:
                try:
                    fig.write_image(args.output_svg)
                except ValueError as exc:  # pragma: no cover - depends on kaleido
                    print(f"Warning: SVG export failed ({exc})", file=sys.stderr)
            if args.show:
                fig.show()

    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(main())
