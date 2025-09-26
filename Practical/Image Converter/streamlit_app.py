"""Streamlit interface for the image converter utility."""
from __future__ import annotations

import importlib.util
import io
import sys
from pathlib import Path
from typing import List, Optional

import streamlit as st

MODULE_PATH = Path(__file__).resolve().with_name("convert.py")
SPEC = importlib.util.spec_from_file_location("image_converter_streamlit", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC and SPEC.loader  # for type checkers
sys.modules["image_converter_streamlit"] = MODULE
SPEC.loader.exec_module(MODULE)  # type: ignore[attr-defined]

SUPPORTED_EXPORT_FORMATS = MODULE.SUPPORTED_EXPORT_FORMATS
SUPPORTED_EXPORT_SET = MODULE.SUPPORTED_EXPORT_SET
ResizeSpec = MODULE.ResizeSpec
convert_image = MODULE.convert_image

MIME_TYPES = {
    "JPEG": "image/jpeg",
    "PNG": "image/png",
    "WEBP": "image/webp",
    "TIFF": "image/tiff",
    "BMP": "image/bmp",
}

LOSSY_FORMATS = {"JPEG", "WEBP"}


def _build_resize_spec(width_text: str, height_text: str) -> Optional[ResizeSpec]:
    width = width_text.strip()
    height = height_text.strip()
    width_value: Optional[int] = None
    height_value: Optional[int] = None

    if width:
        width_value = int(width)
        if width_value <= 0:
            raise ValueError("Width must be a positive integer")
    if height:
        height_value = int(height)
        if height_value <= 0:
            raise ValueError("Height must be a positive integer")

    if width_value is None and height_value is None:
        raise ValueError("Provide at least a width or height when resizing")

    return ResizeSpec(width=width_value, height=height_value)


def render() -> None:
    st.set_page_config(page_title="Image Converter", page_icon="🖼️")
    st.title("Image Converter")
    st.write(
        "Upload one or more images, select a target format, and optionally resize "
        "or adjust quality. Converted files can be downloaded individually."
    )

    uploaded_files = st.file_uploader(
        "Upload images",
        type=None,
        accept_multiple_files=True,
        help="You can select multiple files at once.",
    )

    fmt_upper_choices = sorted(SUPPORTED_EXPORT_SET)
    fmt_upper = st.selectbox("Target format", fmt_upper_choices)

    keep_metadata = st.checkbox(
        "Preserve metadata when supported (EXIF/ICC/PNG text)",
        value=True,
    )

    resize_enabled = st.checkbox("Resize images", value=False)
    resize_width = ""
    resize_height = ""
    if resize_enabled:
        col_width, col_height = st.columns(2)
        resize_width = col_width.text_input(
            "Width (px)",
            value="",
            placeholder="Leave blank to keep aspect",
        )
        resize_height = col_height.text_input(
            "Height (px)",
            value="",
            placeholder="Leave blank to keep aspect",
        )

    quality: Optional[int] = None
    if fmt_upper in LOSSY_FORMATS:
        adjust_quality = st.checkbox("Adjust quality", value=False)
        if adjust_quality:
            default_quality = 85 if fmt_upper == "JPEG" else 80
            quality = st.slider("Quality", min_value=1, max_value=100, value=default_quality)

    convert_clicked = st.button("Convert")

    if not convert_clicked:
        return

    if not uploaded_files:
        st.warning("Upload at least one image to convert.")
        return

    resize_spec: Optional[ResizeSpec] = None
    try:
        if resize_enabled:
            resize_spec = _build_resize_spec(resize_width, resize_height)
    except ValueError as exc:
        st.error(str(exc))
        return

    results: List[dict] = []
    progress = st.progress(0.0)
    status = st.empty()

    total = len(uploaded_files)
    for index, uploaded in enumerate(uploaded_files, start=1):
        status.info(f"Processing {uploaded.name} ({index}/{total})")
        try:
            original_bytes = uploaded.getvalue()
            input_buffer = io.BytesIO(original_bytes)
            output_buffer = io.BytesIO()

            convert_image(
                input_buffer,
                target_format=fmt_upper,
                output_path=output_buffer,
                quality=quality,
                resize=resize_spec,
                keep_metadata=keep_metadata,
            )

            converted_bytes = output_buffer.getvalue()
            extension = SUPPORTED_EXPORT_FORMATS[fmt_upper][0]
            output_name = f"{Path(uploaded.name).stem}{extension}"
            results.append(
                {
                    "original_name": uploaded.name,
                    "converted_name": output_name,
                    "original_bytes": original_bytes,
                    "converted_bytes": converted_bytes,
                }
            )
        except Exception as exc:
            st.error(f"Failed to convert {uploaded.name}: {exc}")

        progress.progress(index / total)

    status.empty()
    progress.empty()

    if not results:
        st.warning("No files were converted.")
        return

    st.success(f"Converted {len(results)} file{'s' if len(results) > 1 else ''}.")

    for item in results:
        st.subheader(item["converted_name"])
        col_original, col_converted = st.columns(2)
        col_original.image(item["original_bytes"], caption=f"Original: {item['original_name']}")
        col_converted.image(item["converted_bytes"], caption="Converted preview")

        mime = MIME_TYPES.get(fmt_upper, "application/octet-stream")
        st.download_button(
            label=f"Download {item['converted_name']}",
            data=item["converted_bytes"],
            file_name=item["converted_name"],
            mime=mime,
        )


__all__ = ["render"]


if __name__ == "__main__":
    render()
