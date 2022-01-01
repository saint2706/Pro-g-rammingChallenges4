key = 13

encrypt_dict = {chr(a % 26 + 97): chr((a + key) % 26 + 97) for a in range(27)}
decrypt_dict = {v: k for k, v in encrypt_dict.items()}

to_encrypt = input("Enter text to encrypt:").lower()
crypt = ""
for i in to_encrypt:
    crypt += encrypt_dict[i]
print(crypt)

to_crack = input("Enter string to be cracked: ").lower()
soln = ""
for i in to_crack:
    soln += decrypt_dict[i]
print(soln)
