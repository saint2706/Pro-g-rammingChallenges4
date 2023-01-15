atbashdict = {chr(a % 26 + 97): chr(122 - a) for a in range(26)}

print("".join(atbashdict[i] for i in input("Enter text:\n").lower().replace(" ", "")))
