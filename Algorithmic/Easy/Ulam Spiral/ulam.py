import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm


def make_spiral(arr):
    rows, cols = arr.shape
    index = np.arange(rows * cols).reshape(rows, cols)[::-1]
    spiral_index = []
    while index.size:
        spiral_index.append(index[0])
        index = index[1:]
        index = index.T[::-1]
    spiral_index = np.hstack(spiral_index)
    spiral = np.empty_like(arr)
    spiral.flat[spiral_index] = arr.flat[::-1]
    return spiral


edgeSize = 13
primes = np.array(
    [
        n
        for n in range(2, edgeSize**2 + 1)
        if all((n % m) != 0 for m in range(2, int(np.sqrt(n)) + 1))
    ]
)

array = np.zeros(edgeSize**2, dtype="u1")
array[primes - 1] = 1
array = make_spiral(array.reshape((edgeSize, edgeSize)))
plt.matshow(array, cmap=cm.binary)
plt.axis("off")
plt.show()
