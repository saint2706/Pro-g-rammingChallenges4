import struct


def display_mbr_contents(disk_image_path):
    with open(disk_image_path, "rb") as f:
        # Read the first sector (512 bytes) which contains the MBR
        mbr_data = f.read(512)

        # Parse the MBR data structure
        boot_code = mbr_data[:446]  # Boot code (first 446 bytes)
        partition_entries = mbr_data[
            446 : 446 + 64 * 4
        ]  # 64 bytes for each partition entry (4 entries)
        signature = struct.unpack("<H", mbr_data[510:512])[
            0
        ]  # MBR signature at offset 510-511

        # Display MBR contents
        print("Boot Code (first 446 bytes):")
        print(boot_code.hex())

        print("\nPartition Entries (each entry is 16 bytes):")
        for i in range(0, len(partition_entries), 16):
            partition_entry = partition_entries[i : i + 16]
            print(partition_entry.hex())

        print("\nMBR Signature (Bytes 510-511):")
        print(hex(signature))


# Specify the disk image file path
disk_image_path = "path_to_your_disk_image.bin"

# Display MBR contents
display_mbr_contents(disk_image_path)
