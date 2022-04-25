import decimal


def compute_pi(n):
    decimal.getcontext().prec = n
    m = 1.0
    k = 13591409
    x = 1
    s = k
    for i in range(1, n):
        m = m * (6.0 ** 3 - 16 * 6.0) / ((i + 1) ** 3)
        k += 545140134
        x *= -262537412640768000
        s += decimal.Decimal(m * k) / x
    return (426880 * decimal.Decimal(10005).sqrt()) / s


print(compute_pi(30))
