def txtToHex(text):
    return "".join(hex(ord(char)).lstrip("0x").rstrip("L") for char in text)


def txtToBin(text):
    return bin(int.from_bytes(text.encode(), "big")).lstrip("0b")


print(txtToHex("Saint"))
print(txtToBin("Saint"))
