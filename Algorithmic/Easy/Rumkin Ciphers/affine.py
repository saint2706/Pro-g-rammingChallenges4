def gcd(a, b):
    x, y, u, v = 0, 1, 1, 0
    while a:
        q, r = b // a, b % a
        m, n = x - u * q, y - v * q
        b, a, x, y, u, v = a, r, u, v, m, n
    return b, x, y


def inv(a, m):
    b, x, _ = gcd(a, m)
    if b != 1:
        return None
    else:
        return x % m


def enc(text, key):
    return "".join(
        [
            chr(((key[0] * (ord(t) - ord("A")) + key[1]) % 26) + ord("A"))
            for t in text.upper().replace(" ", "")
        ]
    )


def dec(text, key):
    return "".join(
        [
            chr(((inv(key[0], 26) * (ord(c) - ord("A") - key[1])) % 26) + ord("A"))
            for c in text
        ]
    )


text = str("Saint")
key = [7, 23]
enc_text = enc(text, key)
print(enc_text)
print(dec(enc_text, key))
