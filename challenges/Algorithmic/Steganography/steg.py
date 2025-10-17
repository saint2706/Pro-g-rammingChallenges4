"""steg.py - Minimal LSB (least significant bit) steganography helper.

This module hides ASCII/UTF-8 text inside the least significant bit of each
color channel in an RGB image. It is intentionally simple and NOT meant for
serious secrecy (no encryption, no obfuscation, no compression). It is a
teaching example for:
  * Bit-level operations (masking & setting LSBs)
  * Capacity planning (bits vs characters)
  * Simple data framing using an end-of-message marker

Enhancements over original script:
  * Dataclass configs for Hide / Extract operations
  * Centralized capacity calculation & validation
  * JSON output option for automation / scripting
  * Additional command: capacity (report how many characters fit)
  * Improved error handling (exceptions propagate with clear messages)
  * Separation of pure logic vs CLI side effects
  * Option to supply message via --message, file via --message-file, or stdin
  * Safe early stop while decoding (stop once marker found)

Limitations:
  * Marker-based framing is fragile if marker appears in plaintext; we escape it.
  * No encryption or compression; large messages inflate output detection risk.
  * Only 1 bit per channel used (3 bpp) for simplicity.
"""

from __future__ import annotations

import argparse
import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Optional, Sequence

from PIL import Image

EOM_MARKER = "||EOM||"
ESCAPE_SEQ = "\\E"  # escape introducer for marker occurrences


# ----------------- Utility Functions -----------------
def escape_message(msg: str) -> str:
    return msg.replace(ESCAPE_SEQ, ESCAPE_SEQ + ESCAPE_SEQ).replace(
        EOM_MARKER, ESCAPE_SEQ + EOM_MARKER
    )


def unescape_message(msg: str) -> str:
    # Reverse of escape_message: process escape sequences
    out = []
    i = 0
    while i < len(msg):
        if msg.startswith(ESCAPE_SEQ, i):
            i += len(ESCAPE_SEQ)
            if i < len(msg):
                out.append(msg[i])
                i += 1
        else:
            out.append(msg[i])
            i += 1
    return "".join(out)


def text_to_bits(text: str) -> str:
    return "".join(f"{byte:08b}" for byte in text.encode("utf-8"))


def bits_to_text(bits: str) -> str:
    bytes_out = bytearray()
    for i in range(0, len(bits), 8):
        chunk = bits[i : i + 8]
        if len(chunk) == 8:
            bytes_out.append(int(chunk, 2))
    try:
        return bytes_out.decode("utf-8", errors="ignore")
    except UnicodeDecodeError:
        return bytes_out.decode("utf-8", errors="replace")


def image_capacity_chars(
    width: int, height: int, bits_per_channel: int = 1, channels: int = 3
) -> int:
    total_bits = width * height * channels * bits_per_channel
    return (
        total_bits // 8
    )  # bytes -> characters (1 byte per char assumption after UTF-8 encoding; conservative)


def open_image(path: str) -> Image.Image:
    try:
        return Image.open(path).convert("RGB")
    except FileNotFoundError:
        raise FileNotFoundError(f"Image not found: {path}")


# ----------------- Core Logic -----------------
def hide_message_in_image(img: Image.Image, message: str) -> Image.Image:
    escaped = escape_message(message)
    payload = escaped + EOM_MARKER
    bit_stream = text_to_bits(payload)
    if len(bit_stream) > img.width * img.height * 3:
        raise ValueError("Message too large for image capacity")

    pixels = img.load()  # type: ignore[assignment]
    bit_index = 0
    for y in range(img.height):
        for x in range(img.width):
            if bit_index >= len(bit_stream):
                break
            r, g, b = pixels[x, y]  # type: ignore[index]
            if bit_index < len(bit_stream):
                r = (r & 0xFE) | int(bit_stream[bit_index])
                bit_index += 1
            if bit_index < len(bit_stream):
                g = (g & 0xFE) | int(bit_stream[bit_index])
                bit_index += 1
            if bit_index < len(bit_stream):
                b = (b & 0xFE) | int(bit_stream[bit_index])
                bit_index += 1
            pixels[x, y] = (r, g, b)  # type: ignore[index]
        if bit_index >= len(bit_stream):
            break
    return img


def extract_message_from_image(img: Image.Image) -> str:
    bits = []
    append = bits.append
    px = img.load()
    for y in range(img.height):
        for x in range(img.width):
            r, g, b = px[x, y]  # type: ignore[index]
            append(str(r & 1))
            append(str(g & 1))
            append(str(b & 1))
            # Convert periodically to check for marker to allow early exit
            if len(bits) % 240 == 0:  # every 80 chars
                partial_text = bits_to_text("".join(bits))
                idx = partial_text.find(EOM_MARKER)
                if idx != -1:
                    return unescape_message(partial_text[:idx])
    # final decode
    full_text = bits_to_text("".join(bits))
    idx = full_text.find(EOM_MARKER)
    if idx == -1:
        raise ValueError("No hidden message marker found")
    return unescape_message(full_text[:idx])


def create_dummy_image(path: str, size: tuple[int, int] = (100, 100)) -> None:
    img = Image.new("RGB", size, color="white")
    img.save(path)


# ----------------- Dataclasses -----------------
@dataclass(slots=True)
class HideConfig:
    image: str
    output: str
    message: Optional[str] = None
    message_file: Optional[str] = None
    stdin: bool = False
    json_output: bool = False

    def resolve_message(self) -> str:
        sources = [self.message is not None, self.message_file is not None, self.stdin]
        if sum(sources) != 1:
            raise ValueError(
                "Provide exactly one message source (--message / --message-file / --stdin)"
            )
        if self.message is not None:
            return self.message
        if self.message_file is not None:
            try:
                return Path(self.message_file).read_text(encoding="utf-8")
            except OSError as e:
                raise ValueError(f"Failed reading message file: {e}")
        return sys.stdin.read()


@dataclass(slots=True)
class ExtractConfig:
    image: str
    json_output: bool = False


@dataclass(slots=True)
class CapacityConfig:
    image: str
    json_output: bool = False


# ----------------- CLI -----------------
def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="LSB steganography tool")
    sub = p.add_subparsers(dest="cmd", required=True)

    h = sub.add_parser("hide", help="Hide a message in an image")
    h.add_argument("image")
    h.add_argument("output")
    h.add_argument("--message")
    h.add_argument("--message-file")
    h.add_argument("--stdin", action="store_true", help="Read message from STDIN")
    h.add_argument("--json", action="store_true")

    e = sub.add_parser("extract", help="Extract a hidden message from an image")
    e.add_argument("image")
    e.add_argument("--json", action="store_true")

    c = sub.add_parser("capacity", help="Report capacity of an image in characters")
    c.add_argument("image")
    c.add_argument("--json", action="store_true")

    d = sub.add_parser("create-dummy", help="Create a dummy white image")
    d.add_argument("output")
    d.add_argument("--size", type=int, nargs=2, metavar=("W", "H"), default=[100, 100])
    return p


def cmd_hide(cfg: HideConfig) -> int:
    img = open_image(cfg.image)
    msg = cfg.resolve_message()
    capacity = image_capacity_chars(img.width, img.height)
    # We compute actual bytes after encoding for accurate size check
    encoded = escape_message(msg) + EOM_MARKER
    required_bits = len(text_to_bits(encoded))
    if required_bits > img.width * img.height * 3:
        print(
            f"Error: message requires {required_bits} bits but capacity is {img.width * img.height * 3}",
            file=sys.stderr,
        )
        return 1
    new_img = hide_message_in_image(img, msg)
    try:
        new_img.save(cfg.output)
    except OSError as e:
        print(f"Error saving image: {e}", file=sys.stderr)
        return 1
    if cfg.json_output:
        payload = {
            "status": "ok",
            "output": cfg.output,
            "width": img.width,
            "height": img.height,
            "capacity_chars": capacity,
            "message_length": len(msg),
        }
        print(json.dumps(payload, indent=2))
    else:
        print(f"Message hidden in {cfg.output} (capacity={capacity} chars)")
    return 0


def cmd_extract(cfg: ExtractConfig) -> int:
    img = open_image(cfg.image)
    try:
        message = extract_message_from_image(img)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    if cfg.json_output:
        print(
            json.dumps(
                {
                    "status": "ok",
                    "length": len(message),
                    "message_sample": message[:120],
                },
                indent=2,
            )
        )
    else:
        print(message)
    return 0


def cmd_capacity(cfg: CapacityConfig) -> int:
    img = open_image(cfg.image)
    capacity = image_capacity_chars(img.width, img.height)
    if cfg.json_output:
        print(
            json.dumps(
                {"width": img.width, "height": img.height, "capacity_chars": capacity},
                indent=2,
            )
        )
    else:
        print(f"Capacity: {capacity} characters")
    return 0


def cmd_create_dummy(output: str, size: list[int]) -> int:
    try:
        create_dummy_image(output, (size[0], size[1]))
    except OSError as e:
        print(f"Error creating image: {e}", file=sys.stderr)
        return 1
    print(f"Created dummy image {output} ({size[0]}x{size[1]})")
    return 0


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    if args.cmd == "hide":
        cfg = HideConfig(
            image=args.image,
            output=args.output,
            message=args.message,
            message_file=args.message_file,
            stdin=args.stdin,
            json_output=args.json,
        )
        try:
            return cmd_hide(cfg)
        except ValueError as e:
            print(f"Error: {e}", file=sys.stderr)
            return 1
    if args.cmd == "extract":
        cfg = ExtractConfig(image=args.image, json_output=args.json)
        return cmd_extract(cfg)
    if args.cmd == "capacity":
        cfg = CapacityConfig(image=args.image, json_output=args.json)
        return cmd_capacity(cfg)
    if args.cmd == "create-dummy":
        return cmd_create_dummy(args.output, args.size)
    parser.error("Unknown command")
    return 2


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
