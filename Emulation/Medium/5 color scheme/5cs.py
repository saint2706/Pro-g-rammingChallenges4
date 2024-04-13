import cv2
import numpy as np
from sklearn.cluster import KMeans
import matplotlib.pyplot as plt
import matplotlib.patches as patches

def get_dominant_colors(image_path, num_colors):
    # Load the image
    image = cv2.imread(image_path)
    if image is None:
        raise ValueError("Error loading image")

    # Convert the image to RGB color space
    image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)

    # Flatten the image
    pixels = image.reshape(-1, 3)

    # Apply K-means clustering
    kmeans = KMeans(n_clusters=num_colors)
    kmeans.fit(pixels)

    # Get the cluster centers
    dominant_colors = kmeans.cluster_centers_.astype(np.uint8)

    return dominant_colors

def display_color_scheme(colors):
    num_colors = len(colors)
    fig, ax = plt.subplots(figsize=(num_colors, 1))
    for i, color in enumerate(colors):
        rect = patches.Rectangle((i, 0), 1, 1, color=color / 255)
        ax.add_patch(rect)
    ax.set_xlim(0, num_colors)
    ax.set_ylim(0, 1)
    ax.axis('off')
    plt.show()

# Example usage
image_path = r"Emulation\Medium\5 color scheme\qwFWZv8.jpg"
num_colors = 5
try:
    dominant_colors = get_dominant_colors(image_path, num_colors)
    print("Dominant colors:")
    print(dominant_colors)
    display_color_scheme(dominant_colors)
except ValueError as e:
    print("Error:", e)
