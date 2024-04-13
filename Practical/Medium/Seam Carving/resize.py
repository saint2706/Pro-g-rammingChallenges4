import numpy as np
import cv2

def energy(image):
    # Compute the energy of each pixel in the image
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    gradient_x = cv2.Sobel(gray, cv2.CV_64F, 1, 0, ksize=3)
    gradient_y = cv2.Sobel(gray, cv2.CV_64F, 0, 1, ksize=3)
    energy_map = np.abs(gradient_x) + np.abs(gradient_y)
    return energy_map

def find_seam(energy_map):
    # Find the least energy seam in the image
    rows, cols = energy_map.shape
    seam = np.zeros((rows,), dtype=int)
    seam[-1] = np.argmin(energy_map[-1])
    for i in range(rows - 2, -1, -1):
        prev_col = seam[i + 1]
        if prev_col == 0:
            seam[i] = np.argmin(energy_map[i, :2])
        elif prev_col == cols - 1:
            seam[i] = np.argmin(energy_map[i, -2:]) + cols - 2
        else:
            seam[i] = np.argmin(energy_map[i, prev_col - 1:prev_col + 2]) + prev_col - 1
    return seam

def remove_seam(image, seam):
    # Remove the least energy seam from the image
    rows, cols, _ = image.shape
    new_image = np.zeros((rows, cols - 1, 3), dtype=np.uint8)
    for i in range(rows):
        new_image[i, :seam[i]] = image[i, :seam[i]]
        new_image[i, seam[i]:] = image[i, seam[i] + 1:]
    return new_image

def seam_carve(image, new_width):
    # Seam carving to resize the image
    for _ in range(image.shape[1] - new_width):
        energy_map = energy(image)
        seam = find_seam(energy_map)
        image = remove_seam(image, seam)
    return image

# Read the input image
input_image = cv2.imread(r"Practical\Medium\Seam Carving\qwFWZv8.jpeg")

# Resize the image using seam carving
resized_image = seam_carve(input_image, new_width=300)

# Save the resized image
cv2.imwrite(r"Practical\Medium\Seam Carving\resize.jpg", resized_image)

