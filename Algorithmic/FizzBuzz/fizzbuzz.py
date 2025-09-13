import sys

def fizzbuzz(limit: int) -> None:
    """
    Prints the FizzBuzz sequence up to a given limit.
    This version is easily extensible for more conditions.
    """
    for i in range(1, limit + 1):
        output = ""
        if i % 3 == 0:
            output += "Fizz"
        if i % 5 == 0:
            output += "Buzz"

        if not output:
            print(i)
        else:
            print(output)

def main():
    """
    Handles user input for the FizzBuzz limit and runs the sequence.
    """
    print("FizzBuzz Generator")

    default_limit = 100

    if len(sys.argv) > 1:
        try:
            limit = int(sys.argv[1])
            if limit < 1:
                print("The limit must be a positive integer.", file=sys.stderr)
                sys.exit(1)
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [limit]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            limit_str = input(f"Enter the upper limit (default: {default_limit}): ")
            if not limit_str:
                limit = default_limit
            else:
                limit = int(limit_str)

            if limit < 1:
                print("Please enter a positive integer.")
                return
        except (ValueError, EOFError):
            print(f"\nInvalid input. Using default limit of {default_limit}.")
            limit = default_limit

    fizzbuzz(limit)

if __name__ == "__main__":
    main()
