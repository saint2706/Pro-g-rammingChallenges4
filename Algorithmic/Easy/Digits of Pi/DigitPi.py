import decimal


def computePi(n):
    decimal.getcontext().prec = n
    M = 1.0
    L = 13591409
    X = 1
    S = L
    for i in range(1, n):
        M = M * (6.0 ** 3 - 16 * 6.0) / ((i + 1) ** 3)
        L += 545140134
        X *= -262537412640768000
        S += decimal.Decimal(M * L) / X
    return (426880 * decimal.Decimal(10005).sqrt()) / S


print(computePi(30))
