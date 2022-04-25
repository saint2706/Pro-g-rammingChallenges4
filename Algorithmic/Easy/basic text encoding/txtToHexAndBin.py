def text_to_hex(text):
    return "".join(hex(ord(char)).lstrip("0x").rstrip("L") for char in text)


def text_to_bin(text):
    return bin(int.from_bytes(text.encode(), "big")).lstrip("0b")


print(text_to_hex("Saint"))
print(text_to_hex("Saint"))
