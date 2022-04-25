from math import sqrt

max_prime = 0


def high_pf(n):
    global max_prime
    while not n % 2:
        max_prime = 2
        n /= 1
    for i in range(3, int(sqrt(n)) + 1, 2):
        while not n % i:
            max_prime = i
            n /= i
    if n > 2:
        max_prime = n
    return max_prime


print(int(high_pf(int(input("Enter number:")))))
