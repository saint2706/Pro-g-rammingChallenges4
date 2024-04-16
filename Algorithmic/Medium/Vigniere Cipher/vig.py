def vigenere_encrypt(plain_text, key):
    encrypted_text = ""
    key_index = 0

    for char in plain_text:
        if char.isalpha():
            key_char = key[key_index % len(key)]
            key_index += 1

            shift = ord(key_char.lower()) - ord("a")
            if char.isupper():
                encrypted_char = chr((ord(char) - ord("A") + shift) % 26 + ord("A"))
            else:
                encrypted_char = chr((ord(char) - ord("a") + shift) % 26 + ord("a"))

            encrypted_text += encrypted_char
        else:
            encrypted_text += char

    return encrypted_text


def vigenere_decrypt(encrypted_text, key):
    decrypted_text = ""
    key_index = 0

    for char in encrypted_text:
        if char.isalpha():
            key_char = key[key_index % len(key)]
            key_index += 1

            shift = ord(key_char.lower()) - ord("a")
            if char.isupper():
                decrypted_char = chr((ord(char) - ord("A") - shift) % 26 + ord("A"))
            else:
                decrypted_char = chr((ord(char) - ord("a") - shift) % 26 + ord("a"))

            decrypted_text += decrypted_char
        else:
            decrypted_text += char

    return decrypted_text


# Usage example
plain_text = "Hello, world! 🌎"
key = "key"

encrypted_text = vigenere_encrypt(plain_text, key)
print("Encrypted text:", encrypted_text)

decrypted_text = vigenere_decrypt(encrypted_text, key)
print("Decrypted text:", decrypted_text)
