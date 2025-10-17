"""pdftag.py - Modern PDF metadata tagging utility.

Enhancements:
  * Uses `pypdf` (preferred) with fallback to PyPDF3 if available
  * Dataclasses for metadata & operation config
  * CLI supports: set fields, list existing metadata, overwrite toggle, JSON summary export
  * Graceful handling of encrypted PDFs (attempts decryption with empty password)
  * Consolidated single-pass read/write
  * Clear exit codes: 0 success, 1 operational error, 2 bad arguments, 3 dependency missing
  * Inline documentation for maintainability

Examples:
  List existing metadata:
    python pdftag.py document.pdf --list

  Update title & author, write summary JSON:
    python pdftag.py document.pdf --title "Sample" --author "Alice" --json meta.json

  Overwrite existing metadata (rather than merging):
    python pdftag.py document.pdf --title "Only Title" --overwrite
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from dataclasses import dataclass, asdict
from pathlib import Path
from typing import Dict, Optional, Any, Protocol, runtime_checkable, Union

# ---------------- Dependency Handling ---------------- #
try:  # Preferred modern library
    import pypdf  # type: ignore
    from pypdf import PdfReader, PdfWriter

    PYPDF_AVAILABLE = True
    PYPDF3_AVAILABLE = False
except Exception:  # pragma: no cover - fallback path
    PYPDF_AVAILABLE = False
    try:
        from PyPDF3 import PdfFileReader as PdfReader, PdfFileWriter as PdfWriter  # type: ignore
        from PyPDF3.errors import PdfReadError  # type: ignore

        PYPDF3_AVAILABLE = True
    except Exception:
        PYPDF3_AVAILABLE = False
        PdfReader = PdfWriter = None  # type: ignore

MISSING_MESSAGE = (
    "Error: No PDF library found. Install one of:\n"
    "  pip install pypdf\n"
    "or\n"
    "  pip install PyPDF3"
)


# Add lightweight protocols for static checkers
@runtime_checkable
class _ReaderProto(Protocol):
    pages: Any


@runtime_checkable
class _WriterProto(Protocol):
    def write(self, stream: Any) -> None: ...


PDFReaderType = Any  # dynamic; runtime selection
PDFWriterType = Any

# ---------------- Data Models ---------------- #


@dataclass(slots=True)
class PDFMetadata:
    title: Optional[str] = None
    author: Optional[str] = None
    subject: Optional[str] = None
    keywords: Optional[str] = None

    def to_pdf_dict(self) -> Dict[str, str]:
        mapping = {
            "/Title": self.title,
            "/Author": self.author,
            "/Subject": self.subject,
            "/Keywords": self.keywords,
        }
        return {k: v for k, v in mapping.items() if v}

    @classmethod
    def from_pdf(cls, info: Any) -> "PDFMetadata":
        # pypdf and PyPDF3 present metadata differently; both support dict-like access
        def _get(key: str) -> Optional[str]:  # normalize extraction
            if not info:
                return None
            # pypdf exposes metadata as attributes or dictionary, PyPDF3 returns DocumentInformation
            val = None
            try:
                if hasattr(info, "get"):
                    val = info.get(key)
                if val is None and hasattr(
                    info, key.strip("/").lower()
                ):  # attribute style
                    val = getattr(info, key.strip("/").lower())
            except Exception:
                val = None
            if val is None:
                return None
            return str(val)

        return cls(
            title=_get("/Title"),
            author=_get("/Author"),
            subject=_get("/Subject"),
            keywords=_get("/Keywords"),
        )


@dataclass(slots=True)
class TagConfig:
    input_pdf: Path
    output_pdf: Path
    metadata: PDFMetadata
    overwrite: bool = False
    list_only: bool = False
    json_summary: Optional[Path] = None


# ---------------- Core Logic ---------------- #


def read_metadata(path: Path) -> PDFMetadata:
    if PdfReader is None:  # type: ignore
        raise RuntimeError("No PDF backend available")
    reader = PdfReader(str(path))  # type: ignore[operator]
    # Attempt simple decryption if encrypted attribute exists
    encrypted = getattr(reader, "is_encrypted", getattr(reader, "isEncrypted", False))
    if encrypted:
        try:
            decrypt = getattr(reader, "decrypt", None)
            if callable(decrypt):
                decrypt("")
        except Exception:
            pass
    info = getattr(reader, "metadata", None)
    if not info:
        get_info = getattr(reader, "getDocumentInfo", None)
        if callable(get_info):
            try:
                info = get_info()
            except Exception:
                info = None
    return PDFMetadata.from_pdf(info)


def update_pdf(config: TagConfig) -> Dict[str, Any]:
    if PdfReader is None or PdfWriter is None:  # type: ignore
        return {"success": False, "error": "No PDF backend"}
    reader = PdfReader(str(config.input_pdf))  # type: ignore[operator]
    encrypted = getattr(reader, "is_encrypted", getattr(reader, "isEncrypted", False))
    if encrypted:
        try:
            decrypt = getattr(reader, "decrypt", None)
            if callable(decrypt):
                decrypt("")
        except Exception:
            return {"success": False, "error": "Encrypted PDF could not be decrypted"}
    writer = PdfWriter()  # type: ignore[operator]
    appended = False
    append_new = getattr(writer, "append_pages_from_reader", None)
    if callable(append_new):
        try:
            append_new(reader)  # type: ignore[arg-type]
            appended = True
        except Exception:
            appended = False
    if not appended:
        append_old = getattr(writer, "appendPagesFromReader", None)
        if callable(append_old):
            try:
                append_old(reader)  # type: ignore
                appended = True
            except Exception:
                appended = False
    if not appended:  # manual copy
        pages = getattr(reader, "pages", [])
        add_page = getattr(writer, "add_page", getattr(writer, "addPage", None))
        if callable(add_page):
            for page in pages:
                try:
                    add_page(page)
                except Exception:
                    return {"success": False, "error": "Failed copying pages"}

    existing = (
        read_metadata(config.input_pdf) if not config.overwrite else PDFMetadata()
    )
    new_dict = existing.to_pdf_dict()
    incoming = config.metadata.to_pdf_dict()
    new_dict.update(incoming)

    added = False
    add_meta = getattr(writer, "add_metadata", getattr(writer, "addMetadata", None))
    if callable(add_meta):
        try:
            add_meta(new_dict)  # type: ignore[arg-type]
            added = True
        except Exception:
            added = False
    if not added:
        return {"success": False, "error": "Failed to apply metadata"}

    try:
        with open(config.output_pdf, "wb") as f_out:
            writer.write(f_out)  # type: ignore[arg-type]
    except Exception as e:
        return {"success": False, "error": f"Write failed: {e}"}

    return {
        "success": True,
        "output": str(config.output_pdf),
        "merged": not config.overwrite,
        "applied_keys": list(incoming.keys()),
        "total_keys": len(new_dict),
    }


# ---------------- CLI ---------------- #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Add, update, or list PDF metadata.",
        epilog='Example: python pdftag.py book.pdf --title "My Book" --author "Me" --json out.json',
    )
    p.add_argument("input_pdf", help="Path to input PDF")
    p.add_argument("-o", "--output", help="Output PDF path (defaults to *_tagged.pdf)")
    p.add_argument("--title", help="Set Title metadata")
    p.add_argument("--author", help="Set Author metadata")
    p.add_argument("--subject", help="Set Subject metadata")
    p.add_argument("--keywords", help="Set Keywords metadata")
    p.add_argument(
        "--list", action="store_true", help="List existing metadata and exit"
    )
    p.add_argument(
        "--overwrite",
        action="store_true",
        help="Overwrite instead of merging existing metadata",
    )
    p.add_argument("--json", type=Path, help="Write JSON summary to path")
    return p


def parse_args(argv: Optional[list[str]]) -> TagConfig:
    parser = build_parser()
    args = parser.parse_args(argv)
    input_path = Path(args.input_pdf)
    if not input_path.exists():
        print(f"Input PDF not found: {input_path}", file=sys.stderr)
        raise SystemExit(2)

    output_path = Path(args.output) if args.output else create_output_path(input_path)

    meta = PDFMetadata(
        title=args.title,
        author=args.author,
        subject=args.subject,
        keywords=args.keywords,
    )

    # If list mode and no new metadata provided, we still proceed to list.
    if not args.list and not meta.to_pdf_dict():
        print(
            "No metadata provided (use --title/--author/--subject/--keywords or --list)",
            file=sys.stderr,
        )
        raise SystemExit(2)

    return TagConfig(
        input_pdf=input_path,
        output_pdf=output_path,
        metadata=meta,
        overwrite=args.overwrite,
        list_only=args.list,
        json_summary=args.json,
    )


# ---------------- Utility ---------------- #


def create_output_path(input_path: Path) -> Path:
    name = input_path.stem
    return input_path.with_name(f"{name}_tagged.pdf")


# ---------------- Main ---------------- #


def main(argv: Optional[list[str]] = None) -> int:
    if not (PYPDF_AVAILABLE or PYPDF3_AVAILABLE):
        print(MISSING_MESSAGE, file=sys.stderr)
        return 3
    try:
        cfg = parse_args(argv)
    except SystemExit as e:  # propagate specific exit codes
        return int(e.code) if isinstance(e.code, int) else 2

    # List existing metadata
    if cfg.list_only:
        try:
            meta = read_metadata(cfg.input_pdf)
        except Exception as e:
            print(f"Error reading metadata: {e}", file=sys.stderr)
            return 1
        data = asdict(meta)
        printable = {k: v for k, v in data.items() if v}
        if not printable:
            print("No metadata present.")
        else:
            for k, v in printable.items():
                print(f"{k.capitalize()}: {v}")
        if cfg.json_summary:
            try:
                with open(cfg.json_summary, "w", encoding="utf-8") as fh:
                    json.dump({"mode": "list", "metadata": printable}, fh, indent=2)
                print(f"JSON written to {cfg.json_summary}")
            except OSError as e:
                print(f"Warning: Could not write JSON: {e}", file=sys.stderr)
        return 0

    summary = update_pdf(cfg)
    if cfg.json_summary:
        try:
            with open(cfg.json_summary, "w", encoding="utf-8") as fh:
                json.dump({"mode": "update", **summary}, fh, indent=2)
            print(f"JSON written to {cfg.json_summary}")
        except OSError as e:
            print(f"Warning: Could not write JSON: {e}", file=sys.stderr)
    if not summary.get("success"):
        print(f"Error: {summary.get('error', 'Unknown failure')}", file=sys.stderr)
        return 1
    print(
        f"Updated metadata -> {summary['output']} (keys applied: {summary['applied_keys']})"
    )
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
