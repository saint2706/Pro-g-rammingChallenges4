# Master Boot Record Parser

## Problem Statement
Inspect the 512-byte Master Boot Record sector of legacy-partitioned disks, decode partition entries, and present them in a readable or JSON-friendly format. This tool reads disk images or generated dummy data to explore MBR structure.

## Usage
- Parse an image and print a table:
  ```bash
  python mbr.py --file disk.img
  ```
- Emit JSON suitable for scripts:
  ```bash
  python mbr.py --file disk.img --json
  ```
- Create a synthetic demo image and immediately inspect it:
  ```bash
  python mbr.py --create-dummy demo_mbr.bin --json
  ```

## Debugging Tips
- The MBR signature bytes (`0x55AA`) at offsets 510–511 must be present; the parser will raise an error otherwise.
- Run the regression tests for quick feedback:
  ```bash
  pytest test_mbr.py
  ```
- When debugging CHS decoding, pass `--log DEBUG` to surface the raw byte triples alongside computed cylinder/head/sector values.

## Implementation Notes
- Uses dataclasses to represent partition entries and ensures table bounds via constants such as `PARTITION_ENTRY_SIZE`.
- Confines itself to the standard library (`struct`, `argparse`, `json`) for portability.
- Handles both human-readable reporting and JSON serialization from the same parse result.

## Further Reading
- [Intel, *BIOS Enhanced Disk Drive Services - MBR Specification*](https://download.intel.com/support/motherboards/desktop/sb/specscs.pdf)
- [Microsoft, *Windows and GPT/MBR Partition Schemes*](https://learn.microsoft.com/windows-hardware/manufacture/desktop/configure-uefi-gpt-based-hard-drive-partitions)
