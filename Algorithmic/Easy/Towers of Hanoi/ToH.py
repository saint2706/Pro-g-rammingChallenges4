import sys
from typing import Generator

def towers_of_hanoi(disks: int, source: str, target: str, auxiliary: str) -> Generator[str, None, None]:
    """
    Solves the Towers of Hanoi problem recursively and yields each move.

    This function is a generator that produces the sequence of moves required
    to transfer `disks` from the `source` rod to the `target` rod using the
    `auxiliary` rod.

    Args:
        disks: The number of disks to move.
        source: The name of the source rod.
        target: The name of the destination rod.
        auxiliary: The name of the auxiliary rod.

    Yields:
        A string describing the next move in the solution.
    """
    if disks > 0:
        # 1. Move n-1 disks from the source rod to the auxiliary rod.
        #    The target rod becomes the temporary auxiliary for this step.
        yield from towers_of_hanoi(disks - 1, source, auxiliary, target)

        # 2. Move the nth disk from the source rod to the target rod.
        yield f"Move disk {disks} from {source} to {target}"

        # 3. Move the n-1 disks from the auxiliary rod to the target rod.
        #    The source rod becomes the temporary auxiliary for this step.
        yield from towers_of_hanoi(disks - 1, auxiliary, target, source)

def main():
    """
    Main function to get user input and print the Towers of Hanoi solution.
    """
    print("--- Towers of Hanoi Solver ---")

    default_disks = 4
    num_disks = default_disks

    if len(sys.argv) > 1:
        try:
            num_disks = int(sys.argv[1])
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [number_of_disks]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            disks_str = input(f"Enter the number of disks (default: {default_disks}): ")
            if disks_str:
                num_disks = int(disks_str)
        except (ValueError, EOFError, KeyboardInterrupt):
            print(f"\nInvalid input or operation cancelled. Using default of {default_disks} disks.")

    if num_disks < 1:
        print("Number of disks must be a positive integer.", file=sys.stderr)
        return

    print(f"\nSolution for {num_disks} disks (from A to C):")
    instructions = towers_of_hanoi(num_disks, source='A', target='C', auxiliary='B')

    # Print the instructions with step numbers
    for i, instruction in enumerate(instructions, 1):
        print(f"{i}. {instruction}")

    print(f"\nTotal moves required: {2**num_disks - 1}")

if __name__ == "__main__":
    main()
