import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

def dot_product(v1, v2):
    return sum(x * y for x, y in zip(v1, v2))

def vector_product(v1, v2):
    if len(v1) == 2:  # For 2D vectors
        return v1[0] * v2[1] - v1[1] * v2[0]
    elif len(v1) == 3:  # For 3D vectors
        return [v1[1] * v2[2] - v1[2] * v2[1],
                v1[2] * v2[0] - v1[0] * v2[2],
                v1[0] * v2[1] - v1[1] * v2[0]]

def plot_vectors(v1, v2, v3):
    if len(v1) == 2:  # For 2D vectors
        plt.quiver(0, 0, v1[0], v1[1], angles='xy', scale_units='xy', scale=1, color='r', label='Vector 1')
        plt.quiver(0, 0, v2[0], v2[1], angles='xy', scale_units='xy', scale=1, color='b', label='Vector 2')
        # plt.quiver(0, 0, v3[0], v3[1], angles='xy', scale_units='xy', scale=1, color='g', label='Cross Product')
        plt.xlim(-10, 10)
        plt.ylim(-10, 10)
        plt.xlabel('X')
        plt.ylabel('Y')
        plt.axhline(0, color='k', linestyle='--')
        plt.axvline(0, color='k', linestyle='--')
        plt.grid()
        plt.legend()
        plt.title('Vectors on Cartesian Plane')
        plt.show()
    elif len(v1) == 3:  # For 3D vectors
        fig = plt.figure()
        ax = fig.add_subplot(111, projection='3d')
        ax.quiver(0, 0, 0, v1[0], v1[1], v1[2], color='r', label='Vector 1')
        ax.quiver(0, 0, 0, v2[0], v2[1], v2[2], color='b', label='Vector 2')
        ax.quiver(0, 0, 0, v3[0], v3[1], v3[2], color='g', label='Cross Product')
        ax.set_xlabel('X')
        ax.set_ylabel('Y')
        ax.set_zlabel('Z')
        ax.legend()
        plt.title('Vectors in 3D Space')
        plt.show()

# Define 2D vectors
vector1_2d = [3, 4]
vector2_2d = [1, -2]

# Define 3D vectors
vector1_3d = [3, 4, 5]
vector2_3d = [1, -2, 3]

# Calculate dot product and vector product
dot_result_2d = dot_product(vector1_2d, vector2_2d)
vector_result_2d = vector_product(vector1_2d, vector2_2d)

dot_result_3d = dot_product(vector1_3d, vector2_3d)
vector_result_3d = vector_product(vector1_3d, vector2_3d)

print("Dot product (2D):", dot_result_2d)
print("Vector product (2D):", vector_result_2d)

print("Dot product (3D):", dot_result_3d)
print("Vector product (3D):", vector_result_3d)

# Plot vectors
plot_vectors(vector1_2d, vector2_2d, vector_result_2d)
plot_vectors(vector1_3d, vector2_3d, vector_result_3d)
