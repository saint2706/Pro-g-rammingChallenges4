import struct
import sys
import os
from typing import Dict, Any, List

def parse_partition_entry(entry_bytes: bytes) -> Dict[str, Any]:
    """
    Parses a 16-byte partition table entry from an MBR.

    Args:
        entry_bytes: The 16 bytes of the partition entry.

    Returns:
        A dictionary containing the decoded fields of the partition entry.
    """
    if len(entry_bytes) != 16:
        return {}

    # MBR Partition Entry Structure (offsets from start of the 16-byte entry):
    # 0x00 (1 byte): Boot Indicator (0x80 for active/bootable, 0x00 for inactive)
    # 0x01 (3 bytes): Starting CHS Address (legacy, often ignored)
    # 0x04 (1 byte): Partition Type Code (e.g., 0x07 for NTFS, 0x83 for Linux)
    # 0x05 (3 bytes): Ending CHS Address (legacy)
    # 0x08 (4 bytes): Starting LBA (Logical Block Address)
    # 0x0C (4 bytes): Size of the partition in sectors (32-bit value)

    # We use '<' for little-endian, 'B' for unsigned char (1 byte), 'x' for padding,
    # and 'I' for unsigned int (4 bytes).
    fields = struct.unpack_from('<B3xB3xII', entry_bytes)

    status = fields[0]
    partition_type = fields[1]
    start_lba = fields[2]
    size_in_sectors = fields[3]

    # Calculate size in Megabytes (assuming 512 bytes/sector)
    size_in_mb = (size_in_sectors * 512) / (1024 * 1024) if size_in_sectors > 0 else 0

    return {
        "status": "Active" if status == 0x80 else "Inactive",
        "type_code": f"0x{partition_type:02x}",
        "start_lba": start_lba,
        "size_in_sectors": size_in_sectors,
        "size_in_mb": f"{size_in_mb:.2f} MB"
    }

def parse_and_display_mbr(mbr_data: bytes):
    """Parses and displays the contents of the given MBR data."""
    if len(mbr_data) != 512:
        print("Error: MBR data must be exactly 512 bytes long.", file=sys.stderr)
        return

    # The MBR signature (0xAA55) is at the end of the sector.
    signature = struct.unpack('<H', mbr_data[510:512])[0]
    print(f"MBR Signature: {signature:#04x} {'(Valid)' if signature == 0xAA55 else '(Invalid)'}")

    print("\n--- Partition Table ---")
    partition_table_bytes = mbr_data[446:510]

    is_empty = True
    for i in range(4):
        entry_bytes = partition_table_bytes[i*16 : (i+1)*16]
        if all(b == 0 for b in entry_bytes):
            continue # Skip empty entries
        is_empty = False

        partition_info = parse_partition_entry(entry_bytes)
        print(f"\n[Partition Entry #{i+1}]")
        print(f"  - Status:           {partition_info['status']}")
        print(f"  - Type Code:        {partition_info['type_code']}")
        print(f"  - Start Sector (LBA): {partition_info['start_lba']}")
        print(f"  - Total Sectors:    {partition_info['size_in_sectors']}")
        print(f"  - Size:             {partition_info['size_in_mb']}")

    if is_empty:
        print("Partition table is empty.")

def create_dummy_mbr_file(filepath: str):
    """Creates a dummy MBR file for demonstration purposes."""
    print(f"Creating a dummy MBR file at '{filepath}'...")
    mbr = bytearray(512)
    # Partition 1: Active, Type 0x0c (FAT32), Start 2048, Size 1,000,000 sectors (~512MB)
    part1 = struct.pack('<B3xB3xII', 0x80, 0x0c, 2048, 1000000)
    # Partition 2: Inactive, Type 0x83 (Linux), Start 1002048, Size 4,000,000 sectors (~2GB)
    part2 = struct.pack('<B3xB3xII', 0x00, 0x83, 1002048, 4000000)

    mbr[446:462] = part1
    mbr[462:478] = part2

    # Set the mandatory MBR signature
    mbr[510:512] = struct.pack('<H', 0xAA55)

    with open(filepath, 'wb') as f:
        f.write(mbr)

def main():
    """Main function to parse an MBR from a file."""
    print("--- MBR (Master Boot Record) Parser ---")

    if len(sys.argv) > 1:
        disk_image_path = sys.argv[1]
        print(f"Reading MBR from specified file: {disk_image_path}")
    else:
        disk_image_path = "dummy_mbr.bin"
        if not os.path.exists(disk_image_path):
            create_dummy_mbr_file(disk_image_path)

    if not os.path.exists(disk_image_path):
        print(f"Error: Disk image file not found at '{disk_image_path}'", file=sys.stderr)
        sys.exit(1)

    try:
        with open(disk_image_path, "rb") as f:
            mbr_data = f.read(512)
        parse_and_display_mbr(mbr_data)
    except IOError as e:
        print(f"Error reading file: {e}", file=sys.stderr)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
