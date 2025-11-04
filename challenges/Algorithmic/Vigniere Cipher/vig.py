"""Vigenère Cipher Tool (modernized).

Features:
  * Clean pure function `vigenere_cipher` for encryption/decryption
  * Key sanitization (letters only, case-insensitive internally)
  * Streaming-friendly implementation (can adapt to chunked processing)
  * CLI with flexible input/output sources
  * JSON output mode for automation pipelines
  * Optional upper-case normalization for output for historical style

Examples:
  python vig.py encrypt "LEMON" -t "Attack at dawn" --json
  python vig.py decrypt secret --in ciphertext.txt --out plain.txt
  echo "Hello World" | python vig.py encrypt key
  python vig.py encrypt key -t "Mixed CASE & punctuation!" --upper

Algorithm Notes:
  The Vigenère cipher applies a Caesar shift determined by successive key
  letters. Non-alphabetic characters are passed through unchanged and do not
  consume key characters (traditional variant). Case of letters is preserved
  unless --upper is supplied.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from typing import Optional

# ------------------------------ Core Logic ------------------------------ #


def sanitize_key(key: str) -> str:
    """Return lowercase alphabetic-only key.

    Empty result after sanitization is invalid and handled by caller.
    """
    return "".join(ch for ch in key if ch.isalpha()).lower()


def vigenere_cipher(
    text: str, key: str, mode: str = "encrypt", *, upper: bool = False
) -> str:
    """Encrypt or decrypt ``text`` with Vigenère cipher.

    Parameters
    ----------
    text : str
        Input text (any unicode; only A-Z/a-z transformed)
    key : str
        Cipher key (letters; other chars ignored). Case-insensitive.
    mode : {'encrypt','decrypt'}
        Operation mode.
    upper : bool
        If True, force alphabetic output to uppercase (common style in crypto puzzles).
    """
    if mode not in {"encrypt", "decrypt"}:
        raise ValueError("mode must be 'encrypt' or 'decrypt'")
    skey = sanitize_key(key)
    if not skey:
        raise ValueError("key must contain at least one alphabetic character")

    out_chars = []
    klen = len(skey)
    kpos = 0
    decrypt = mode == "decrypt"

    for ch in text:
        if ch.isalpha():
            base = "A" if ch.isupper() else "a"
            base_ord = ord(base)
            shift = ord(skey[kpos % klen]) - ord("a")
            if decrypt:
                shift = -shift
            transformed = chr((ord(ch) - base_ord + shift) % 26 + base_ord)
            if upper:
                transformed = transformed.upper()
            out_chars.append(transformed)
            kpos += 1
        else:
            out_chars.append(ch)
    return "".join(out_chars)


# ------------------------------ Configuration ------------------------------ #


@dataclass(slots=True)
class CLIConfig:
    mode: str
    key: str
    text: Optional[str] = None
    infile: Optional[str] = None
    outfile: Optional[str] = None
    json_out: bool = False
    upper: bool = False

    def validate(self) -> None:
        if self.mode not in {"encrypt", "decrypt"}:
            raise ValueError("mode must be 'encrypt' or 'decrypt'")
        if not sanitize_key(self.key):
            raise ValueError(
                "key must contain at least one alphabetic character after sanitization"
            )
        if self.text is not None and self.infile is not None:
            raise ValueError("Provide either --text or --in, not both")
        if self.text is None and self.infile is None:
            # Allow stdin fallback; handled later
            pass


# ------------------------------ I/O Helpers ------------------------------ #


def read_input(cfg: CLIConfig) -> str:
    if cfg.text is not None:
        return cfg.text
    if cfg.infile is not None:
        with open(cfg.infile, "r", encoding="utf-8") as f:
            return f.read()
    # Stdin fallback
    return sys.stdin.read()


def write_output(cfg: CLIConfig, data: str) -> None:
    if cfg.outfile:
        with open(cfg.outfile, "w", encoding="utf-8") as f:
            f.write(data)
    else:
        # Raw output (not wrapped) when not JSON to simplify piping
        print(data)


# ------------------------------ CLI Assembly ------------------------------ #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Vigenère cipher encrypt/decrypt utility (modernized)"
    )
    p.add_argument("mode", choices=["encrypt", "decrypt"], help="Operation mode")
    p.add_argument("key", help="Cipher key (letters; other chars ignored)")
    src = p.add_mutually_exclusive_group()
    src.add_argument("-t", "--text", help="Inline plaintext/ciphertext input")
    src.add_argument(
        "--in", dest="infile", metavar="PATH", help="Input file path (UTF-8)"
    )
    p.add_argument(
        "--out", dest="outfile", metavar="PATH", help="Write result to file (UTF-8)"
    )
    p.add_argument(
        "--json",
        action="store_true",
        help="Emit JSON metadata instead of plain text output",
    )
    p.add_argument(
        "--upper", action="store_true", help="Force output letters to uppercase"
    )
    return p


def main(argv: Optional[list[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    cfg = CLIConfig(
        mode=args.mode,
        key=args.key,
        text=args.text,
        infile=args.infile,
        outfile=args.outfile,
        json_out=args.json,
        upper=args.upper,
    )
    try:
        cfg.validate()
    except ValueError as e:
        parser.error(str(e))

    data_in = read_input(cfg)
    try:
        result = vigenere_cipher(data_in, cfg.key, cfg.mode, upper=cfg.upper)
    except ValueError as e:
        parser.error(str(e))

    if cfg.json_out:
        payload = {
            "mode": cfg.mode,
            "key_length": len(sanitize_key(cfg.key)),
            "input_length": len(data_in),
            "output_length": len(result),
            "result": result,
            "upper": cfg.upper,
            "source": (
                "text" if cfg.text is not None else ("file" if cfg.infile else "stdin")
            ),
            "outfile": bool(cfg.outfile),
        }
        json_str = json.dumps(payload, indent=2)
        if cfg.outfile:
            with open(cfg.outfile, "w", encoding="utf-8") as f:
                f.write(json_str)
        else:
            print(json_str)
    else:
        write_output(cfg, result)
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry
    raise SystemExit(main())
