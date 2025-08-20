import sys
import os
import argparse
from typing import Dict

# Note: PyPDF3 is a fork of PyPDF2. The community has since consolidated back
# to PyPDF2 and its successor, `pypdf`. For new projects, consider using `pypdf`.
try:
    from PyPDF3 import PdfFileReader, PdfFileWriter
    from PyPDF3.errors import PdfReadError
except ImportError:
    print("Error: The 'PyPDF3' library is required.", file=sys.stderr)
    print("Please install it using: pip install PyPDF3", file=sys.stderr)
    sys.exit(1)

def update_pdf_metadata(input_path: str, output_path: str, metadata: Dict[str, str]):
    """
    Reads a PDF, updates its metadata, and saves it to a new file.

    Args:
        input_path: The path to the source PDF file.
        output_path: The path to save the modified PDF file.
        metadata: A dictionary of metadata to add or update.
    """
    try:
        with open(input_path, 'rb') as f_in:
            reader = PdfFileReader(f_in)
            writer = PdfFileWriter()

            # Copy all pages from the original PDF
            writer.appendPagesFromReader(reader)

            # Copy existing metadata from the original PDF
            existing_metadata = reader.getDocumentInfo()
            if existing_metadata:
                writer.addMetadata(existing_metadata)

            # Add or update with the new metadata provided by the user
            writer.addMetadata(metadata)

            # Write the new PDF to the output file
            with open(output_path, 'wb') as f_out:
                writer.write(f_out)

            print(f"Successfully updated metadata. New file saved to '{output_path}'")

    except FileNotFoundError:
        print(f"Error: Input file not found at '{input_path}'", file=sys.stderr)
    except PdfReadError:
        print(f"Error: Could not read PDF file. It may be corrupt or encrypted.", file=sys.stderr)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

def create_output_path(input_path: str) -> str:
    """Creates a default output path by adding '_tagged' to the filename."""
    directory, filename = os.path.split(input_path)
    name, ext = os.path.splitext(filename)
    return os.path.join(directory, f"{name}_tagged{ext}")

def main():
    """
    Main function to parse command-line arguments and tag a PDF file.
    """
    parser = argparse.ArgumentParser(
        description="Add or update metadata tags (Title, Author, etc.) for a PDF file.",
        epilog="Example: python pdftag.py 1984.pdf --title 'Nineteen Eighty-Four' --author 'George Orwell'"
    )
    parser.add_argument("input_pdf", help="Path to the input PDF file.")
    parser.add_argument("-o", "--output_pdf", help="Optional path for the output file. Defaults to adding '_tagged' to the input filename.")

    # Metadata arguments
    parser.add_argument("--title", help="Set the document's Title.")
    parser.add_argument("--author", help="Set the document's Author.")
    parser.add_argument("--subject", help="Set the document's Subject.")
    parser.add_argument("--keywords", help="Set the document's Keywords.")

    args = parser.parse_args()

    # Collect the new metadata from the arguments
    new_metadata = {}
    if args.title: new_metadata['/Title'] = args.title
    if args.author: new_metadata['/Author'] = args.author
    if args.subject: new_metadata['/Subject'] = args.subject
    if args.keywords: new_metadata['/Keywords'] = args.keywords

    if not new_metadata:
        print("No metadata provided to update. Use --title, --author, etc. to set tags.")
        sys.exit(0)

    # Determine the output path
    output_path = args.output_pdf if args.output_pdf else create_output_path(args.input_pdf)

    update_pdf_metadata(args.input_pdf, output_path, new_metadata)

if __name__ == "__main__":
    main()
