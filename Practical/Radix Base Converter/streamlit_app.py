"""Streamlit interface for the Radix/Base converter tools."""

from __future__ import annotations

import csv
import io

try:  # Optional dependency – keeps the app usable without pandas installed.
    import pandas as pd  # type: ignore
except Exception:  # pragma: no cover - streamlit runtime will expose import errors
    pd = None  # type: ignore

import streamlit as st

from radix import batch_convert, convert_formatted, format_output, normalize_bases


def _parse_base_list(text: str) -> list[int]:
    """Parse a comma/space separated list of bases into validated integers."""

    if not text.strip():
        return []

    tokens = [token.strip() for token in text.replace(",", " ").split() if token.strip()]
    bases: list[int] = []
    for token in tokens:
        try:
            bases.append(int(token, 10))
        except ValueError as exc:  # Provide a clean error for the UI.
            raise ValueError(f"Base '{token}' is not an integer") from exc

    normalized = normalize_bases(bases)

    # Preserve order while removing duplicates to avoid redundant conversions.
    seen: set[int] = set()
    unique: list[int] = []
    for base in normalized:
        if base not in seen:
            seen.add(base)
            unique.append(base)
    return unique


def _build_batch_downloads(rows: list[dict[str, str | int]]) -> tuple[bytes, bytes]:
    """Return CSV and plain text exports for the given conversion rows."""

    csv_buffer = io.StringIO()
    writer = csv.DictWriter(csv_buffer, fieldnames=["input", "from_base", "to_base", "output"])
    writer.writeheader()
    writer.writerows(rows)
    csv_bytes = csv_buffer.getvalue().encode("utf-8")

    text_lines = [
        f"{row['input']} (base {row['from_base']}) -> base {row['to_base']} = {row['output']}"
        for row in rows
    ]
    text_bytes = "\n".join(text_lines).encode("utf-8")
    return csv_bytes, text_bytes


def render() -> None:
    st.set_page_config(page_title="Radix Base Converter", page_icon="🔢", layout="centered")
    st.title("🔢 Radix Base Converter")
    st.write(
        "Convert between bases 2 through 36 with optional batch tables and exportable results."
    )

    value = st.text_input("Value", value="FF")

    col_from, col_to, col_pad = st.columns(3)
    with col_from:
        from_base = int(
            st.number_input("From base", min_value=2, max_value=36, value=16, step=1)
        )
    with col_to:
        to_base = int(st.number_input("To base", min_value=2, max_value=36, value=10, step=1))
    with col_pad:
        pad_width = int(st.number_input("Pad width", min_value=0, max_value=64, value=0, step=1))

    case_option = st.selectbox("Output case", ["Uppercase", "Lowercase"], index=0)
    lower_case = case_option == "Lowercase"

    st.subheader("Single conversion")
    if value.strip():
        try:
            result = convert_formatted(
                value.strip(),
                from_base,
                to_base,
                lower=lower_case,
                pad=pad_width,
            )
            st.success(
                f"{value.strip()} (base {from_base}) = {result} (base {to_base})"
            )
        except ValueError as exc:
            st.error(str(exc))
    else:
        st.info("Enter a value to convert.")

    st.subheader("Batch conversions")
    st.write("Provide optional lists to generate a cross-table of conversions.")
    batch_col_from, batch_col_to = st.columns(2)
    with batch_col_from:
        batch_from_text = st.text_input(
            "From bases list",
            value="",
            placeholder="e.g. 2, 8, 10",
            help="Comma or space separated list. Leave blank to reuse the single from-base.",
        )
    with batch_col_to:
        batch_to_text = st.text_input(
            "To bases list",
            value="",
            placeholder="e.g. 10 16 36",
            help="Comma or space separated list. Leave blank to reuse the single to-base.",
        )

    if st.button("Run batch conversion"):
        if not value.strip():
            st.error("Enter a value to convert before running batch conversions.")
        else:
            try:
                batch_from = _parse_base_list(batch_from_text) or [from_base]
                batch_to = _parse_base_list(batch_to_text) or [to_base]
            except ValueError as exc:
                st.error(str(exc))
            else:
                results = batch_convert(value.strip(), batch_from, batch_to)
                if not results:
                    st.info("No valid conversions were produced for the provided bases.")
                else:
                    table_rows: list[dict[str, str | int]] = []
                    for item in results:
                        formatted = format_output(item.output, lower=lower_case, pad=pad_width)
                        table_rows.append(
                            {
                                "input": item.input,
                                "from_base": item.from_base,
                                "to_base": item.to_base,
                                "output": formatted,
                            }
                        )

                    if pd is not None:
                        st.dataframe(pd.DataFrame(table_rows), use_container_width=True)
                    else:
                        st.dataframe(table_rows, use_container_width=True)

                    csv_bytes, text_bytes = _build_batch_downloads(table_rows)
                    st.download_button(
                        "Download CSV",
                        data=csv_bytes,
                        file_name="radix_conversions.csv",
                        mime="text/csv",
                    )
                    st.download_button(
                        "Download text",
                        data=text_bytes,
                        file_name="radix_conversions.txt",
                        mime="text/plain",
                    )


__all__ = ["render"]


if __name__ == "__main__":
    render()
