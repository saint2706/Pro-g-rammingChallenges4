def to_decimal(number, base):
    """Convert a number from given base to decimal (base 10)"""
    if base < 2 or base > 36:
        raise ValueError("Base must be between 2 and 36")

    decimal = 0
    power = 0
    for digit in reversed(str(number).upper()):
        if digit.isdigit():
            value = int(digit)
        else:
            value = ord(digit) - ord("A") + 10

        if value >= base:
            raise ValueError(f"Invalid digit '{digit}' for base {base}")

        decimal += value * (base**power)
        power += 1

    return decimal


def from_decimal(number, base):
    """Convert a decimal number to the given base"""
    if base < 2 or base > 36:
        raise ValueError("Base must be between 2 and 36")

    if number == 0:
        return "0"

    digits = []
    while number:
        remainder = number % base
        if remainder < 10:
            digits.append(str(remainder))
        else:
            digits.append(chr(ord("A") + remainder - 10))
        number //= base

    return "".join(digits[::-1])


def main():
    while True:
        print("\nRadix Base Converter")
        print("1. Convert to Decimal")
        print("2. Convert from Decimal")
        print("3. Exit")

        choice = input("Enter your choice (1-3): ")

        if choice == "1":
            number = input("Enter the number: ")
            base = int(input("Enter the base of the number (2-36): "))
            try:
                result = to_decimal(number, base)
                print(f"The decimal equivalent is: {result}")
            except ValueError as e:
                print(f"Error: {e}")

        elif choice == "2":
            number = int(input("Enter the decimal number: "))
            base = int(input("Enter the base to convert to (2-36): "))
            try:
                result = from_decimal(number, base)
                print(f"The number in base {base} is: {result}")
            except ValueError as e:
                print(f"Error: {e}")

        elif choice == "3":
            print("Thank you for using the Radix Base Converter!")
            break

        else:
            print("Invalid choice. Please try again.")


if __name__ == "__main__":
    main()
