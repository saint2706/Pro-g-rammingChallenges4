import numpy as np
import matplotlib.pyplot as plt


def mandelbrot(c, max_iter):
    z = 0
    n = 0

    while abs(z) <= 2 and n < max_iter:
        z = z * z + c
        n += 1

    return n


def generate_mandelbrot_set(width, height, x_min, x_max, y_min, y_max, max_iter):
    image = np.zeros((height, width))

    for x in range(width):
        for y in range(height):
            real = x * (x_max - x_min) / (width - 1) + x_min
            imaginary = y * (y_max - y_min) / (height - 1) + y_min
            c = complex(real, imaginary)
            color_value = mandelbrot(c, max_iter)
            image[y, x] = color_value

    return image


# Parameters for the Mandelbrot set
width, height = 800, 600
x_min, x_max = -2.5, 1.5
y_min, y_max = -1.5, 1.5
max_iter = 255

# Generate the Mandelbrot set image
mandelbrot_image = generate_mandelbrot_set(
    width, height, x_min, x_max, y_min, y_max, max_iter
)

# Display the Mandelbrot set
plt.figure(figsize=(10, 8))
plt.imshow(mandelbrot_image.T, cmap="viridis", extent=(x_min, x_max, y_min, y_max))
plt.colorbar()
plt.title("Mandelbrot Set")
plt.xlabel("Re(c)")
plt.ylabel("Im(c)")
plt.show()
