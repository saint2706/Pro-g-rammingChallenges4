import time, os

s = " "
p = "."
a = s * 3
b = " _ "
c = "|_|"
d = "| |"
e = "|  "
f = "  |"
g = "|_ "
h = " _|"
P = [s, p, p]
z = [
    [b, d, c],
    [a, f, f],
    [b, h, g],
    [b, h, h],
    [a, c, f],
    [b, g, h],
    [b, g, c],
    [b, f, f],
    [b, c, c],
    [b, c, h],
]
A = range(10)
B = A[:6]
C = A[:3]
while 1:
    for i in C:
        for j in A:
            if i == 2 & (j == 4):
                break
            for k in B:
                for l in A:
                    for m in C:
                        print(z[i][m] + s + z[j][m] + P[m] + z[k][m] + z[l][m])
                    time.sleep(1)
                    os.system("cls" if os.name == "nt" else "clear")
