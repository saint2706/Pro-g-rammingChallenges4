import decimal

decimal.getcontext().prec = 1002  # Set precision to include 1000 digits of pi


def calculate_pi():
    decimal.getcontext().prec += 2  # Extra digits for intermediate calculations
    three = decimal.Decimal(3)
    p, p_last = decimal.Decimal(3), decimal.Decimal(0)
    n, na, d, da = 1, 0, 0, 24
    pi = 0

    while p != p_last:
        p_last = p
        pi += p
        na += 1
        d += da
        pi -= three * (na // d)
        na %= d
        n += 1
        da += 8

        p = p_last + decimal.Decimal(1) / decimal.Decimal(16**n)

    return str(pi)[
        :1001
    ]  # Return the first 1001 characters to include 1000 digits of pi


# Calculate first 1000 digits of pi
pi_digits = calculate_pi()
print("First 1000 digits of pi:")
print(pi_digits)
