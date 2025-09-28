"""Streamlit interface for the IP tracking visualization tool."""

from __future__ import annotations

import time
from pathlib import Path
from typing import Iterable, List, Sequence

import pandas as pd
import streamlit as st

from trackip import (
    Config,
    build_map_figure,
    export_csv,
    export_json,
    fetch_locations,
    summarize_results,
)

DEFAULT_OUTPUT = Path("streamlit_map.html")


def _parse_ip_lines(lines: Iterable[str]) -> List[str]:
    """Parse an iterable of strings into a list of unique, trimmed IPs."""

    cleaned: List[str] = []
    for raw in lines:
        # Split on whitespace and commas to accommodate a variety of inputs.
        for token in raw.replace(",", " ").split():
            ip = token.strip()
            if ip and ip not in cleaned:
                cleaned.append(ip)
    return cleaned


def _read_uploaded_ips(upload) -> List[str]:
    if upload is None:
        return []
    try:
        text = upload.getvalue().decode("utf-8")
    except UnicodeDecodeError:
        st.error("Uploaded file must be UTF-8 encoded.")
        return []
    return _parse_ip_lines(text.splitlines())


def _build_config(
    ips: Sequence[str], timeout: float, retries: int, backoff: float, workers: int
) -> Config:
    cfg = Config(
        ips=list(ips),
        file=None,
        output_html=DEFAULT_OUTPUT,
        json_path=None,
        csv_path=None,
        timeout=timeout,
        retries=retries,
        backoff=backoff,
        max_workers=workers,
        show_progress=False,
        summary=False,
    )
    cfg.validate()
    return cfg


def _render_summary(meta: dict[str, object]) -> None:
    st.subheader("Summary")
    cols = st.columns(3)
    cols[0].metric("Requested", meta.get("requested", 0))
    cols[1].metric("Succeeded", meta.get("succeeded", 0))
    cols[2].metric("Failed", meta.get("failed", 0))

    countries = meta.get("countries") or []
    if countries:
        st.write(
            "**Countries:**",
            ", ".join(country for country in countries if isinstance(country, str)),
        )
    st.write(f"**Elapsed:** {meta.get('elapsed_sec', 0)} seconds")


def render() -> None:
    st.set_page_config(page_title="IP Tracking Visualization", layout="wide")
    st.title("IP Tracking Visualization")
    st.write(
        "Enter IP addresses manually or upload a text file."
        " Configure retry behaviour and click **Fetch** to retrieve"
        " geolocation details."
    )

    text_ips = st.text_area(
        "IP Addresses",
        help="Provide one IP per line or separate them with spaces/commas.",
        height=160,
    )
    uploaded_file = st.file_uploader(
        "Upload IP List", type=["txt", "csv"], help="Plain text files only."
    )

    col1, col2, col3 = st.columns(3)
    timeout = col1.number_input(
        "Timeout (seconds)", min_value=0.1, max_value=60.0, value=5.0, step=0.5
    )
    retries = col2.number_input(
        "Retries", min_value=0, max_value=10, value=2, step=1, help="Retries per IP"
    )
    backoff = col3.number_input(
        "Backoff base (seconds)", min_value=0.1, max_value=5.0, value=0.6, step=0.1
    )
    workers = st.slider(
        "Max concurrent lookups", min_value=1, max_value=16, value=8, step=1
    )

    if st.button("Fetch", type="primary"):
        manual_ips = _parse_ip_lines(text_ips.splitlines())
        file_ips = _read_uploaded_ips(uploaded_file)
        all_ips = list(dict.fromkeys([*manual_ips, *file_ips]))

        if not all_ips:
            st.error("Provide at least one IP address via the text area or upload.")
            return

        try:
            cfg = _build_config(all_ips, timeout, int(retries), backoff, int(workers))
        except ValueError as exc:
            st.error(f"Configuration error: {exc}")
            return

        with st.spinner("Fetching IP geolocation data..."):
            start = time.time()
            try:
                locations = fetch_locations(cfg.ips, cfg)
            except Exception as exc:  # noqa: BLE001 - surface failures to UI
                st.error(f"Failed to fetch IP data: {exc}")
                return
            elapsed = time.time() - start

        if not locations:
            st.error("No location data retrieved. Check the IPs or try again later.")
            return

        figure = build_map_figure(locations)
        st.plotly_chart(figure, use_container_width=True)

        meta = summarize_results(cfg.ips, locations, elapsed)
        _render_summary(meta)

        df = pd.DataFrame([loc.as_dict() for loc in locations])
        st.subheader("Results")
        st.dataframe(df, use_container_width=True)

        json_payload = export_json(locations, None, meta)
        csv_payload = export_csv(locations, None)
        st.download_button(
            "Download JSON",
            data=json_payload,
            file_name="ip_locations.json",
            mime="application/json",
        )
        st.download_button(
            "Download CSV",
            data=csv_payload,
            file_name="ip_locations.csv",
            mime="text/csv",
        )


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover
    render()
