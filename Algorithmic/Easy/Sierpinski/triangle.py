import sys

def generate_sierpinski(size: int) -> None:
    """
    Prints a Sierpinski triangle of a given size using a bitwise AND operation.
    The algorithm is based on the property that a point (x, y) in a grid is
    part of the triangle if (x & y) == 0.

    Args:
        size: The height of the triangle. A power of 2 is recommended.
    """
    if size <= 0:
        print("Size must be a positive integer.")
        return

    # Iterate through rows from top to bottom (0 to size-1)
    for y in range(size):
        # Invert y for the bitwise calculation to draw the triangle upright
        # This corresponds to the original code's y-loop running from n-1 down to 0
        calc_y = size - 1 - y

        # Print leading spaces for centering
        print(" " * y, end="")

        # Iterate through columns for this row
        for x in range(size - y):
            # The bitwise AND trick determines if a character is printed
            if (x & calc_y) == 0:
                print("* ", end="")
            else:
                # Print spaces to maintain the triangle shape
                print("  ", end="")
        print() # Newline at the end of the row

def main():
    """
    Main function to get user input and generate the Sierpinski triangle.
    """
    print("--- Sierpinski Triangle Generator ---")

    default_size = 8
    size = default_size

    if len(sys.argv) > 1:
        try:
            size = int(sys.argv[1])
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [size]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            size_str = input(f"Enter the triangle size (power of 2 like 8 or 16 is best, default: {default_size}): ")
            if size_str:
                size = int(size_str)
        except (ValueError, EOFError, KeyboardInterrupt):
            print(f"\nInvalid input or operation cancelled. Using default size of {default_size}.")

    # Warn the user if the size is not a power of two
    if size > 0 and (size & (size - 1)) != 0:
        print("\nWarning: For the best visual result, use a size that is a power of 2 (e.g., 4, 8, 16).")

    print()
    generate_sierpinski(size)

if __name__ == "__main__":
    main()
