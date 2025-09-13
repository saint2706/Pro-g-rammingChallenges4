import numpy as np
import matplotlib.pyplot as plt
import sys
from typing import Tuple

def least_squares_fit(x_coords: np.ndarray, y_coords: np.ndarray) -> Tuple[float, float]:
    """
    Calculates the slope (m) and y-intercept (b) for a set of points
    using the least squares linear regression method.

    Args:
        x_coords: A numpy array of the x-coordinates of the points.
        y_coords: A numpy array of the y-coordinates of the points.

    Returns:
        A tuple containing the slope (m) and y-intercept (b).

    Raises:
        ValueError: If input arrays are mismatched, empty, or would result
                    in a division by zero (vertical line).
    """
    if x_coords.size != y_coords.size:
        raise ValueError("Input arrays must be of the same size.")
    if x_coords.size == 0:
        raise ValueError("Input arrays must not be empty.")

    n = x_coords.size

    # Use numpy for efficient sum calculations
    sum_x = np.sum(x_coords)
    sum_y = np.sum(y_coords)
    sum_xy = np.sum(x_coords * y_coords)
    sum_x_sq = np.sum(x_coords**2)

    # Calculate slope (m) and intercept (b) using the standard formulas
    numerator = n * sum_xy - sum_x * sum_y
    denominator = n * sum_x_sq - sum_x**2

    if np.isclose(denominator, 0):
        # This occurs if all x-values are the same, forming a vertical line.
        raise ValueError("Cannot compute regression for a vertical set of points (denominator is zero).")

    m = numerator / denominator
    b = (sum_y - m * sum_x) / n

    return m, b

def plot_regression(x: np.ndarray, y: np.ndarray, m: float, b: float, save_path: str = None):
    """
    Creates, displays, and optionally saves a plot of the data points,
    the regression line, and the error lines.

    Args:
        x: The x-coordinates of the data points.
        y: The y-coordinates of the data points.
        m: The slope of the regression line.
        b: The y-intercept of the regression line.
        save_path: The path to save the plot image to. If None, not saved.
    """
    plt.figure(figsize=(12, 8))
    plt.scatter(x, y, color="blue", s=30, zorder=5, label="Data Points")

    # Determine the range for the regression line
    line_x = np.array([np.min(x) - 5, np.max(x) + 5])
    line_y = m * line_x + b
    plt.plot(line_x, line_y, color="red", linewidth=2, label="Regression Line (y=mx+b)")

    # Plot the error lines (vertical distance from point to line)
    for i in range(x.size):
        label = "Error (Residuals)" if i == 0 else ""
        plt.plot([x[i], x[i]], [y[i], m * x[i] + b], color="green", linestyle="--", linewidth=1, label=label)

    plt.title("Least Squares Linear Regression", fontsize=16)
    plt.xlabel("X Values", fontsize=12)
    plt.ylabel("Y Values", fontsize=12)
    plt.grid(True, which='both', linestyle='--', linewidth=0.5)
    plt.legend(fontsize=10)
    plt.tight_layout()

    if save_path:
        try:
            plt.savefig(save_path, dpi=300)
            print(f"Plot successfully saved to '{save_path}'")
        except Exception as e:
            print(f"Error: Could not save the plot. {e}", file=sys.stderr)

    plt.show()

def main():
    """
    Main function to generate data and run the linear regression analysis.
    """
    print("--- Least Squares Fitting Demonstration ---")

    # Generate some semi-random data for a more realistic demonstration
    num_points = 20
    # Create x-data with a bit of spread
    x_data = np.linspace(0, 100, num_points)

    # Create y-data based on a known line (y = 0.75x + 10) and add noise
    true_m, true_b = 0.75, 10
    noise = np.random.normal(0, 10, num_points) # Gaussian noise
    y_data = true_m * x_data + true_b + noise

    try:
        m, b = least_squares_fit(x_data, y_data)
        print(f"Original Line: y = {true_m:.2f}x + {true_b:.2f}")
        print(f"Calculated Regression Line: y = {m:.2f}x + {b:.2f}")

        # Define the save path for the plot
        save_path = "Algorithmic/Medium/Least Squares Fitting/plot.png"
        plot_regression(x_data, y_data, m, b, save_path)

    except ValueError as e:
        print(f"Error during calculation: {e}", file=sys.stderr)
    except Exception as e:
        print(f"An unexpected error occurred: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
