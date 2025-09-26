"""IPv4 and URL obfuscation/inspection helpers.

This module exposes helper functions and a CLI that can encode an IPv4
address into a range of alternate textual forms (decimal integer, hex,
binary, etc.), rewrite URLs to embed those forms, and decode them back to a
canonical dotted-decimal string.

The tool is intentionally educational—showing how easy it is to disguise a
network destination—and should be used ethically.

The public helpers are designed to be imported by other tools or notebooks.
They expose structured return types and raise :class:`ObscurifierError`
instead of terminating the process so callers can provide their own error
handling and presentation layers.
"""

from __future__ import annotations

import argparse
import ipaddress
import json
import re
import sys
from dataclasses import dataclass
from typing import Dict, Iterable, List, Sequence, Tuple
from urllib.parse import ParseResult, urlparse, urlunparse

DEFAULT_MIX_PATTERNS: List["MixPattern"] = [
    ("hex-lead", ("hex", "hex", "decimal", "decimal")),
    ("binary-tail", ("decimal", "decimal", "binary", "binary")),
    ("octal-sandwich", ("octal", "decimal", "octal", "hex")),
]


@dataclass
class IPv4VariantBundle:
    """Container for IPv4 obfuscation variants.

    Attributes
    ----------
    canonical:
        Canonical dotted-decimal representation of the IPv4 address.
    integers:
        Mapping of style name to single-integer disguises.
    dotted:
        Mapping of style name to dotted representations using a single base
        per octet.
    mixed:
        Sequence of ``(label, dotted_value)`` pairs describing mixed-base
        dotted representations.
    """

    canonical: str
    integers: Dict[str, str]
    dotted: Dict[str, str]
    mixed: List[Tuple[str, str]]


__all__ = [
    "DEFAULT_MIX_PATTERNS",
    "IPv4VariantBundle",
    "ObscurifierError",
    "generate_ipv4_variants",
    "decode_ipv4",
    "encode_url",
    "decode_url",
    "build_parser",
    "main",
]


_BASE_FORMATTERS = {
    "decimal": lambda x: f"{x}",
    "zero-padded": lambda x: f"{x:03d}",
    "hex": lambda x: f"0x{x:02X}",
    "hex-noprefix": lambda x: f"{x:02X}",
    "octal": lambda x: f"0{x:03o}",
    "explicit-octal": lambda x: f"0o{x:o}",
    "binary": lambda x: f"0b{x:08b}",
}


class ObscurifierError(ValueError):
    """Custom error type for clearer CLI messaging."""


def _validate_mix_sequence(sequence: Sequence[str]) -> Tuple[str, str, str, str]:
    if len(sequence) != 4:
        raise ObscurifierError("Mixed-base patterns must include exactly four components.")
    normalised = []
    for component in sequence:
        key = component.strip().lower()
        if key not in _BASE_FORMATTERS:
            raise ObscurifierError(
                f"Unsupported base '{component}'. Allowed: {', '.join(sorted(_BASE_FORMATTERS))}."
            )
        normalised.append(key)
    return tuple(normalised)  # type: ignore[return-value]


MixPattern = Tuple[str, Tuple[str, str, str, str]]
MixPatternInput = Sequence[Tuple[str, Sequence[str]]]


def generate_ipv4_variants(
    ip: str,
    *,
    custom_mix: Sequence[str] | None = None,
    include_default_mixes: bool = True,
    mix_limit: int | None = None,
    default_mix_patterns: MixPatternInput | None = None,
) -> IPv4VariantBundle:
    """Produce dotted and integer IPv4 disguises.

    Parameters
    ----------
    ip:
        IPv4 address in dotted-decimal notation.
    custom_mix:
        Optional sequence of base labels (``decimal``, ``hex``, ``octal``,
        ``binary``, ``zero-padded``) that will be used to generate a single
        mixed dotted representation.
    include_default_mixes:
        When ``True`` the default mix patterns (or the ones supplied via
        ``default_mix_patterns``) are included in the bundle.
    mix_limit:
        Optional cap on the number of default mix patterns returned.
    default_mix_patterns:
        Override for the default mix pattern catalogue. Each entry should be
        a ``(label, pattern)`` tuple where ``pattern`` contains four base
        names.

    Returns
    -------
    IPv4VariantBundle
        Structured collection containing canonical, integer and dotted
        variants.

    Raises
    ------
    ObscurifierError
        If the IPv4 address is invalid or a mix pattern is malformed.
    """

    try:
        addr = ipaddress.IPv4Address(ip)
    except ipaddress.AddressValueError as exc:
        raise ObscurifierError(str(exc)) from exc

    octets = [int(part) for part in addr.exploded.split(".")]
    canonical = str(addr)
    integer_value = int(addr)

    integers = {
        "decimal": str(integer_value),
        "hex": f"0x{integer_value:08X}",
        "octal": f"0o{integer_value:011o}",
        "binary": f"0b{integer_value:032b}",
    }

    dotted = {
        "decimal": canonical,
        "zero-padded": ".".join(f"{octet:03d}" for octet in octets),
        "hex": ".".join(f"0x{octet:02X}" for octet in octets),
        "octal": ".".join(f"0{octet:03o}" for octet in octets),
        "binary": ".".join(f"0b{octet:08b}" for octet in octets),
    }

    mixed: List[Tuple[str, str]] = []

    if include_default_mixes:
        patterns_source: Sequence[Tuple[str, Sequence[str]]]
        patterns_source = default_mix_patterns if default_mix_patterns is not None else DEFAULT_MIX_PATTERNS
        patterns_list = list(patterns_source)
        if mix_limit is not None:
            patterns_list = patterns_list[:mix_limit]
        for label, pattern in patterns_list:
            normalised = _validate_mix_sequence(pattern)
            mixed.append((label, _format_dotted(octets, normalised)))

    if custom_mix:
        pattern = _validate_mix_sequence(custom_mix)
        mixed.append((f"custom({'/'.join(pattern)})", _format_dotted(octets, pattern)))

    return IPv4VariantBundle(canonical=canonical, integers=integers, dotted=dotted, mixed=mixed)


def _format_dotted(octets: Sequence[int], pattern: Sequence[str]) -> str:
    formatted = []
    for octet, style in zip(octets, pattern):
        formatter = _BASE_FORMATTERS[style]
        formatted.append(formatter(octet))
    return ".".join(formatted)


def _detect_base(token: str) -> Tuple[int, str]:
    stripped = token.strip()
    lowered = stripped.lower()

    if not stripped:
        raise ObscurifierError("Empty IPv4 component")

    prefix_map = {
        "0x": (16, "hex"),
        "0o": (8, "explicit-octal"),
        "0b": (2, "binary"),
    }
    for prefix, (base, label) in prefix_map.items():
        if lowered.startswith(prefix):
            return int(stripped, base), label

    if re.fullmatch(r"[0-9a-f]+", lowered) and any(c.isalpha() for c in lowered):
        return int(stripped, 16), "hex"

    if stripped.startswith("0") and len(stripped) > 1 and stripped.isdigit():
        decimal_value = int(stripped, 10)
        octal_value = int(stripped, 8)
        if decimal_value == octal_value:
            return decimal_value, "zero-padded"
        return octal_value, "octal"

    return int(stripped, 10), "decimal"


def decode_ipv4(value: str) -> Tuple[str, List[Tuple[str, str, int]]]:
    """Normalise an IPv4 string back to dotted decimal form.

    Parameters
    ----------
    value:
        String containing either a dotted representation or an integer form
        of the IPv4 address.

    Returns
    -------
    tuple[str, list[tuple[str, str, int]]]
        A tuple of the canonical dotted-decimal IPv4 string and a breakdown
        of the detected bases for each component.

    Raises
    ------
    ObscurifierError
        If the input cannot be interpreted as a valid IPv4 address.
    """

    stripped = value.strip()
    if not stripped:
        raise ObscurifierError("Empty value provided")

    if "." not in stripped:
        integer_value, base_label = _detect_integer(stripped)
        try:
            addr = ipaddress.IPv4Address(integer_value)
        except ipaddress.AddressValueError as exc:
            raise ObscurifierError(str(exc)) from exc
        return str(addr), [(stripped, base_label, integer_value)]

    components = stripped.split(".")
    if len(components) != 4:
        raise ObscurifierError("IPv4 dotted form must have exactly four components.")

    octets: List[int] = []
    breakdown: List[Tuple[str, str, int]] = []
    for component in components:
        value_int, base_label = _detect_base(component)
        if not 0 <= value_int <= 255:
            raise ObscurifierError(f"Component '{component}' resolves to {value_int}, outside 0-255 range.")
        octets.append(value_int)
        breakdown.append((component, base_label, value_int))

    canonical = ".".join(str(part) for part in octets)
    return canonical, breakdown


def _detect_integer(token: str) -> Tuple[int, str]:
    lowered = token.lower()
    prefix_map = {
        "0x": (16, "hex"),
        "0o": (8, "explicit-octal"),
        "0b": (2, "binary"),
    }
    for prefix, (base, label) in prefix_map.items():
        if lowered.startswith(prefix):
            return int(token, base), label

    if lowered.startswith("0") and len(lowered) > 1 and lowered.isdigit():
        decimal_value = int(token, 10)
        octal_value = int(token, 8)
        if decimal_value == octal_value:
            return decimal_value, "zero-padded"
        return octal_value, "octal"

    return int(token, 10), "decimal"


def _parse_credentials(value: str | None) -> Tuple[str | None, str | None]:
    if value is None:
        return None, None
    if ":" not in value:
        raise ObscurifierError("Credentials must be in the form user:password")
    user, password = value.split(":", 1)
    if not user:
        raise ObscurifierError("Credential username may not be empty")
    return user, password


def encode_url(
    url: str,
    *,
    credentials: str | None = None,
    custom_mix: Sequence[str] | None = None,
    include_default_mixes: bool = True,
    mix_limit: int | None = None,
    default_mix_patterns: MixPatternInput | None = None,
) -> Tuple[IPv4VariantBundle, List[Tuple[str, str]]]:
    """Rewrite a URL with hosts disguised via IPv4 variants.

    Parameters
    ----------
    url:
        Input URL whose host component should be obfuscated.
    credentials:
        Optional ``user:password`` string to inject if the URL does not
        already provide credentials.
    custom_mix, include_default_mixes, mix_limit, default_mix_patterns:
        Passed through to :func:`generate_ipv4_variants` to control the host
        disguise catalogue.

    Returns
    -------
    tuple[IPv4VariantBundle, list[tuple[str, str]]]
        ``IPv4VariantBundle`` describing the host and a list of
        ``(label, url)`` pairs for each rewritten URL.

    Raises
    ------
    ObscurifierError
        If the URL or credentials are malformed.
    """
    parsed = _parse_url(url)
    host = parsed.hostname
    if host is None:
        raise ObscurifierError("URL is missing a hostname")

    canonical_host, _ = decode_ipv4(host)
    variant_bundle = generate_ipv4_variants(
        canonical_host,
        custom_mix=custom_mix,
        include_default_mixes=include_default_mixes,
        mix_limit=mix_limit,
        default_mix_patterns=default_mix_patterns,
    )

    user, password = _parse_credentials(credentials)
    if user is None:
        user = parsed.username
        password = parsed.password

    url_variants = []
    base_netloc_suffix = f":{parsed.port}" if parsed.port else ""

    for label, host_variant in _iter_url_hosts(variant_bundle):
        netloc = host_variant.lower() if any(ch.isalpha() for ch in host_variant) else host_variant
        if user is not None:
            cred = f"{user}:{password or ''}@"
        else:
            cred = ""
        full_netloc = f"{cred}{netloc}{base_netloc_suffix}"
        rebuilt = parsed._replace(netloc=full_netloc)
        url_variants.append((label, urlunparse(rebuilt)))

    return variant_bundle, url_variants


def _parse_url(url: str) -> ParseResult:
    parsed = urlparse(url)
    if not parsed.scheme:
        raise ObscurifierError("URL must include a scheme (e.g., http://)")
    if not parsed.netloc:
        raise ObscurifierError("URL must include a network location")
    return parsed


def _iter_url_hosts(bundle: IPv4VariantBundle) -> Iterable[Tuple[str, str]]:
    yield ("canonical", bundle.canonical)
    for label, value in bundle.integers.items():
        yield (f"integer-{label}", value)
    for label, value in bundle.dotted.items():
        yield (f"dotted-{label}", value)
    for label, value in bundle.mixed:
        yield (f"mixed-{label}", value)


def decode_url(url: str) -> Dict[str, object]:
    """Inspect a URL and surface canonical addressing information.

    Parameters
    ----------
    url:
        URL whose host may be disguised.

    Returns
    -------
    dict[str, object]
        JSON-serialisable structure mirroring the CLI output, including
        credentials, host breakdown, and canonical URL.

    Raises
    ------
    ObscurifierError
        If the URL is malformed or the host cannot be decoded.
    """
    parsed = _parse_url(url)
    host = parsed.hostname
    if host is None:
        raise ObscurifierError("URL is missing a hostname")

    canonical_host, breakdown = decode_ipv4(host)

    sanitized_netloc = canonical_host
    if parsed.port:
        sanitized_netloc += f":{parsed.port}"

    canonical_url = urlunparse(parsed._replace(netloc=sanitized_netloc))

    return {
        "original": url,
        "canonical_host": canonical_host,
        "has_credentials": parsed.username is not None,
        "credentials": {
            "username": parsed.username,
            "password": parsed.password,
        },
        "host_breakdown": breakdown,
        "canonical_url": canonical_url,
        "path": parsed.path or "/",
        "query": parsed.query,
        "fragment": parsed.fragment,
    }


def _format_table(rows: Sequence[Tuple[str, str]]) -> str:
    width = max(len(name) for name, _ in rows)
    lines = [f"{name.ljust(width)}  {value}" for name, value in rows]
    return "\n".join(lines)


def _print_json(data: object) -> None:
    json.dump(data, sys.stdout, indent=2)
    sys.stdout.write("\n")


def _handle_encode_ip(args: argparse.Namespace) -> None:
    custom_mix = args.mix.split(",") if args.mix else None
    bundle = generate_ipv4_variants(
        args.value,
        custom_mix=custom_mix,
        include_default_mixes=not args.no_default_mixes,
        mix_limit=args.mix_limit,
    )

    if args.format == "json":
        output = {
            "canonical": bundle.canonical,
            "integers": bundle.integers,
            "dotted": bundle.dotted,
            "mixed": bundle.mixed,
        }
        _print_json(output)
        return

    rows: List[Tuple[str, str]] = [("canonical", bundle.canonical)]
    rows.extend((f"integer-{label}", value) for label, value in bundle.integers.items())
    rows.extend((f"dotted-{label}", value) for label, value in bundle.dotted.items())
    for label, value in bundle.mixed:
        rows.append((f"mixed-{label}", value))

    print(_format_table(rows))


def _handle_encode_url(args: argparse.Namespace) -> None:
    custom_mix = args.mix.split(",") if args.mix else None
    bundle, url_variants = encode_url(
        args.value,
        credentials=args.credentials,
        custom_mix=custom_mix,
        include_default_mixes=not args.no_default_mixes,
        mix_limit=args.mix_limit,
    )

    if args.format == "json":
        output = {
            "canonical_host": bundle.canonical,
            "variants": url_variants,
        }
        _print_json(output)
        return

    rows = [("canonical-host", bundle.canonical)]
    rows.extend(url_variants)
    print(_format_table(rows))


def _handle_decode_ip(args: argparse.Namespace) -> None:
    canonical, breakdown = decode_ipv4(args.value)

    if args.format == "json":
        output = {
            "canonical": canonical,
            "breakdown": [
                {"component": component, "base": base, "value": value}
                for component, base, value in breakdown
            ],
        }
        _print_json(output)
        return

    print(f"canonical  {canonical}")
    if len(breakdown) == 1 and breakdown[0][0] == args.value.strip():
        component, base, value_int = breakdown[0]
        print(f"detected   {component} ({base}) -> {value_int}")
        return

    print("components:")
    width = max(len(component) for component, _, _ in breakdown)
    for component, base, value_int in breakdown:
        print(f"  {component.ljust(width)}  {base:<13} -> {value_int}")


def _handle_decode_url(args: argparse.Namespace) -> None:
    data = decode_url(args.value)

    if args.format == "json":
        _print_json(data)
        return

    rows = [
        ("original", data["original"]),
        ("canonical-host", data["canonical_host"]),
        ("canonical-url", data["canonical_url"]),
        (
            "credentials",
            "yes" if data["has_credentials"] else "no",
        ),
    ]
    print(_format_table(rows))
    breakdown = data["host_breakdown"]
    if breakdown:
        print("\ncomponents:")
        width = max(len(component) for component, _, _ in breakdown)
        for component, base, value_int in breakdown:
            print(f"  {component.ljust(width)}  {base:<13} -> {value_int}")


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Encode and decode IPv4/URL obfuscations.",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    encode_ip = subparsers.add_parser("encode-ip", help="Generate IPv4 disguises")
    encode_ip.add_argument("value", help="IPv4 address in dotted decimal form")
    encode_ip.add_argument("--format", choices=["table", "json"], default="table")
    encode_ip.add_argument("--mix", help="Comma separated base names for a custom mixed dotted variant")
    encode_ip.add_argument(
        "--mix-limit",
        type=int,
        default=None,
        help="Limit the number of default mixed patterns included",
    )
    encode_ip.add_argument(
        "--no-default-mixes",
        action="store_true",
        help="Only output the custom mix variant",
    )
    encode_ip.set_defaults(func=_handle_encode_ip)

    encode_url_parser = subparsers.add_parser("encode-url", help="Rewrite URL with disguised host")
    encode_url_parser.add_argument("value", help="URL whose host will be obscured")
    encode_url_parser.add_argument("--credentials", help="Inject credentials (user:password)")
    encode_url_parser.add_argument("--format", choices=["table", "json"], default="table")
    encode_url_parser.add_argument("--mix", help="Comma separated base names for a custom mixed dotted variant")
    encode_url_parser.add_argument(
        "--mix-limit",
        type=int,
        default=None,
        help="Limit the number of default mixed patterns included",
    )
    encode_url_parser.add_argument(
        "--no-default-mixes",
        action="store_true",
        help="Only output the custom mix variant",
    )
    encode_url_parser.set_defaults(func=_handle_encode_url)

    decode_ip = subparsers.add_parser("decode-ip", help="Inspect an obfuscated IPv4")
    decode_ip.add_argument("value", help="IPv4 string (integer or dotted)")
    decode_ip.add_argument("--format", choices=["table", "json"], default="table")
    decode_ip.set_defaults(func=_handle_decode_ip)

    decode_url_parser = subparsers.add_parser("decode-url", help="Inspect an obfuscated URL")
    decode_url_parser.add_argument("value", help="URL to decode")
    decode_url_parser.add_argument("--format", choices=["table", "json"], default="table")
    decode_url_parser.set_defaults(func=_handle_decode_url)

    return parser


def main(argv: Sequence[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        args.func(args)
    except ObscurifierError as exc:  # type: ignore[attr-defined]
        parser.error(str(exc))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
