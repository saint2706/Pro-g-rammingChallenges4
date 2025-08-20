import collections

sentence = input("Enter sentence: ")
counts = collections.Counter(sentence)

print("Character counts:")
for char, count in sorted(counts.items()):
    print(f"'{char}': {count}")
