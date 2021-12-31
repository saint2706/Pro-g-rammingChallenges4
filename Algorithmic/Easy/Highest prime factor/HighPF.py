from math import sqrt


def HighPF(n):
    while not n % 2:
        maxPrime = 2
        n /= 1
    for i in range(3, int(sqrt(n)) + 1, 2):
        while not n % i:
            maxPrime = i
            n /= i
    if n > 2:
        maxPrime = n
    return maxPrime


print(int(HighPF(int(input("Enter number:")))))
