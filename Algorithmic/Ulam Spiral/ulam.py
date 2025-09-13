import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import sys
from typing import Tuple

def sieve_of_eratosthenes(n: int) -> np.ndarray:
    """
    Generates all prime numbers up to n using the Sieve of Eratosthenes.

    Args:
        n: The upper limit for the sieve.

    Returns:
        A boolean numpy array of size n+1, where array[i] is True if i is prime.
    """
    primes = np.ones(n + 1, dtype=bool)
    primes[0:2] = False  # 0 and 1 are not prime
    for i in range(2, int(np.sqrt(n)) + 1):
        if primes[i]:
            # Mark all multiples of i as not prime
            primes[i*i : n+1 : i] = False
    return primes

def make_spiral(arr: np.ndarray) -> np.ndarray:
    """
    Rearranges a flat 1D array into a 2D spiral matrix.
    The input array must be a perfect square.
    """
    # Get the dimensions of the square matrix
    edge_size = int(np.sqrt(arr.size))
    if edge_size * edge_size != arr.size:
        raise ValueError("Input array must be a perfect square.")

    grid = arr.reshape((edge_size, edge_size))

    # Create an index grid to map positions for the spiral
    index_grid = np.arange(edge_size * edge_size).reshape((edge_size, edge_size))[::-1]

    spiral_indices = []
    while index_grid.size > 0:
        # Take the top row of the index grid
        spiral_indices.append(index_grid[0])
        # Rotate the rest of the grid counter-clockwise to bring the next row to the top
        index_grid = index_grid[1:].T[::-1]

    spiral_index_flat = np.concatenate(spiral_indices)

    # Create the final spiral grid
    spiral_grid = np.empty_like(grid)
    # Place the elements from the input array into their spiral positions
    spiral_grid.flat[spiral_index_flat] = grid.flat[::-1]

    return spiral_grid

def main():
    """
    Main function to generate and display an Ulam spiral.
    """
    print("--- Ulam Spiral Generator ---")

    default_size = 101  # A larger, odd number shows the pattern better
    edge_size = default_size

    if len(sys.argv) > 1:
        try:
            edge_size = int(sys.argv[1])
        except ValueError:
            print(f"Usage: python {sys.argv[0]} [edge_size]", file=sys.stderr)
            sys.exit(1)
    else:
        try:
            size_str = input(f"Enter the spiral edge size (odd number recommended, default: {default_size}): ")
            if size_str:
                edge_size = int(size_str)
        except (ValueError, EOFError, KeyboardInterrupt):
            print(f"\nInvalid input or operation cancelled. Using default size of {edge_size}.")

    if edge_size <= 0:
        print("Edge size must be a positive integer.", file=sys.stderr)
        return
    if edge_size % 2 == 0:
        print("Warning: An even edge size will result in an off-center spiral. Odd sizes are better.")

    # 1. Generate primes up to the maximum number in the spiral (edge_size^2)
    limit = edge_size ** 2
    is_prime = sieve_of_eratosthenes(limit)

    # 2. Create a flat array where 1 represents a prime, 0 otherwise
    # We want to represent numbers 1 to limit. is_prime[i] is for number i.
    # So we take the slice from 1 to limit+1.
    prime_markers = is_prime[1:limit+1].astype("u1")

    # 3. Form the spiral
    print("Generating spiral...")
    spiral_grid = make_spiral(prime_markers)

    # 4. Display the spiral using matplotlib
    print("Displaying plot...")
    plt.figure(figsize=(10, 10))
    plt.matshow(spiral_grid, cmap=cm.binary, fignum=1)
    plt.title(f"Ulam Spiral ({edge_size}x{edge_size})")
    plt.axis("off")
    plt.show()

if __name__ == "__main__":
    main()
