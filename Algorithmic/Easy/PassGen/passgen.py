from string import ascii_letters, digits, punctuation
from random import sample

print("Default value is 1")
symbols = int(input("Do you want symbols: (1/0)\n"))
digit = int(input("Do you want numbers: (1/0)\n"))
letters = int(input("Do you want letters: (1/0)\n"))
length = int(input("Length of the password: (8-32)\n"))

char_list = ""
if symbols == 1:
    char_list += punctuation
if digit == 1:
    char_list += digits
if letters == 1:
    char_list += ascii_letters
elif digit == 0 and letters == 0 and symbols == 0:
    char_list += punctuation
    char_list += digits
    char_list += ascii_letters

print("".join(sample(char_list, length)))
