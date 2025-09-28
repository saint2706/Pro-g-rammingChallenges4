"""Streamlit interface for the IP URL Obscurifier helpers."""

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Iterable, Sequence, Tuple

import pandas as pd
import streamlit as st

PACKAGE_ROOT = Path(__file__).resolve().parent
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))

import obscurifier  # type: ignore  # pylint: disable=import-error

from obscurifier import (  # noqa: E402  # pylint: disable=wrong-import-position
    DEFAULT_MIX_PATTERNS,
    ObscurifierError,
    decode_ipv4,
    decode_url,
    encode_url,
    generate_ipv4_variants,
)


def render() -> None:
    """Render the IP/URL obscurifier interface."""

    st.set_page_config(page_title="IP URL Obscurifier", layout="wide")
    st.title("IP URL Obscurifier")

    module_doc = obscurifier.__doc__ or ""
    if module_doc:
        st.warning(module_doc.strip())

    tab_encode_ip, tab_decode_ip, tab_encode_url, tab_decode_url = st.tabs(
        [
            "Generate IPv4 disguises",
            "Decode IPv4 value",
            "Encode URL",
            "Decode URL",
        ]
    )

    with tab_encode_ip:
        st.subheader("Generate IPv4 disguises")
        ip_value = st.text_input("IPv4 address", value="192.168.0.1")
        custom_mix_input = st.text_input(
            "Custom mixed bases (comma separated)",
            help="Example: hex,decimal,octal,binary",
        )
        selected_patterns = _collect_mix_patterns("encode-ip")

        if st.button("Generate variants", key="generate-ip") and ip_value:
            try:
                custom_mix = [
                    part.strip() for part in custom_mix_input.split(",") if part.strip()
                ] or None
                bundle = generate_ipv4_variants(
                    ip_value,
                    custom_mix=custom_mix,
                    default_mix_patterns=selected_patterns,
                )
            except ObscurifierError as exc:  # pragma: no cover - UI feedback
                st.error(str(exc))
            else:
                st.success(f"Canonical IPv4: {bundle.canonical}")
                st.dataframe(_bundle_to_table(bundle), use_container_width=True)
                payload = {
                    "canonical": bundle.canonical,
                    "integers": bundle.integers,
                    "dotted": bundle.dotted,
                    "mixed": bundle.mixed,
                }
                st.download_button(
                    "Download IPv4 JSON",
                    data=json.dumps(payload, indent=2),
                    file_name="ipv4_variants.json",
                    mime="application/json",
                )

    with tab_decode_ip:
        st.subheader("Decode IPv4 value")
        decode_value = st.text_input("IPv4 string or integer", value="0xC0A80001")
        if st.button("Decode IPv4", key="decode-ip") and decode_value:
            try:
                canonical, breakdown = decode_ipv4(decode_value)
            except ObscurifierError as exc:  # pragma: no cover - UI feedback
                st.error(str(exc))
            else:
                st.success(f"Canonical IPv4: {canonical}")
                st.dataframe(_breakdown_table(breakdown), use_container_width=True)
                payload = {
                    "canonical": canonical,
                    "breakdown": [
                        {"component": component, "base": base, "value": value}
                        for component, base, value in breakdown
                    ],
                }
                st.download_button(
                    "Download decode JSON",
                    data=json.dumps(payload, indent=2),
                    file_name="ipv4_decode.json",
                    mime="application/json",
                )

    with tab_encode_url:
        st.subheader("Encode URL")
        url_value = st.text_input("URL", value="http://192.168.0.1/admin")
        credential_value = st.text_input(
            "Credentials (user:password)",
            help="Optional credentials to inject into the rewritten URLs.",
        )
        url_custom_mix = st.text_input(
            "Custom mixed bases for URL hosts",
            key="url-custom-mix",
            help="Comma separated list.",
        )
        url_patterns = _collect_mix_patterns("encode-url")

        if st.button("Rewrite URL", key="encode-url") and url_value:
            try:
                custom_mix = [
                    part.strip() for part in url_custom_mix.split(",") if part.strip()
                ] or None
                bundle, variants = encode_url(
                    url_value,
                    credentials=credential_value or None,
                    custom_mix=custom_mix,
                    default_mix_patterns=url_patterns,
                )
            except ObscurifierError as exc:  # pragma: no cover - UI feedback
                st.error(str(exc))
            else:
                st.success(f"Canonical host: {bundle.canonical}")
                variant_table = pd.DataFrame(
                    [{"label": label, "url": value} for label, value in variants]
                )
                st.dataframe(variant_table, use_container_width=True)
                payload = {
                    "canonical_host": bundle.canonical,
                    "variants": variants,
                }
                st.download_button(
                    "Download URL variants JSON",
                    data=json.dumps(payload, indent=2),
                    file_name="url_variants.json",
                    mime="application/json",
                )

    with tab_decode_url:
        st.subheader("Decode URL")
        decode_url_value = st.text_input(
            "URL to decode", value="http://0xC0A80001/login"
        )
        if st.button("Inspect URL", key="decode-url") and decode_url_value:
            try:
                data = decode_url(decode_url_value)
            except ObscurifierError as exc:  # pragma: no cover - UI feedback
                st.error(str(exc))
            else:
                st.success(f"Canonical URL: {data['canonical_url']}")
                summary_table = pd.DataFrame(
                    [
                        {"property": "original", "value": data["original"]},
                        {"property": "canonical_host", "value": data["canonical_host"]},
                        {"property": "canonical_url", "value": data["canonical_url"]},
                        {
                            "property": "credentials",
                            "value": "yes" if data["has_credentials"] else "no",
                        },
                    ]
                )
                st.dataframe(summary_table, use_container_width=True)
                breakdown = data.get("host_breakdown", [])
                if breakdown:
                    st.markdown("### Host breakdown")
                    st.dataframe(_breakdown_table(breakdown), use_container_width=True)
                st.download_button(
                    "Download URL decode JSON",
                    data=json.dumps(data, indent=2),
                    file_name="url_decode.json",
                    mime="application/json",
                )


def _collect_mix_patterns(label_prefix: str) -> Sequence[Tuple[str, Sequence[str]]]:
    """Return the mix patterns selected by the user via toggles."""

    selected: list[Tuple[str, Sequence[str]]] = []
    for label, pattern in DEFAULT_MIX_PATTERNS:
        include = st.toggle(
            f"Include '{label}' pattern",
            value=True,
            key=f"{label_prefix}-mix-{label}",
            help="Toggle whether this default mixed-base pattern is generated.",
        )
        if include:
            selected.append((label, pattern))
    return selected


def _bundle_to_table(bundle: obscurifier.IPv4VariantBundle) -> pd.DataFrame:
    rows = [{"category": "canonical", "label": "canonical", "value": bundle.canonical}]
    rows.extend(
        {"category": "integer", "label": name, "value": value}
        for name, value in bundle.integers.items()
    )
    rows.extend(
        {"category": "dotted", "label": name, "value": value}
        for name, value in bundle.dotted.items()
    )
    rows.extend(
        {"category": "mixed", "label": label, "value": value}
        for label, value in bundle.mixed
    )
    return pd.DataFrame(rows)


def _breakdown_table(breakdown: Iterable[Tuple[str, str, int]]) -> pd.DataFrame:
    return pd.DataFrame(
        [
            {"component": component, "base": base, "value": value}
            for component, base, value in breakdown
        ]
    )


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover - manual execution helper
    render()
