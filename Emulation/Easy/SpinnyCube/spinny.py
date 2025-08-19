import math
import sys

# Add a check for the vpython library and provide a helpful message if it's missing.
try:
    from vpython import scene, box, rate, vector
except ImportError:
    print("Error: The 'vpython' library is required to run this script.", file=sys.stderr)
    print("Please install it using: pip install vpython", file=sys.stderr)
    sys.exit(1)

# --- Constants ---
ROTATION_SPEED = 0.005  # Radians per frame, controlling the speed of the spin

def setup_scene():
    """Initializes the VPython scene and sets its properties."""
    scene.title = "Spinny Cube"
    scene.range = 2  # Set the camera's zoom level
    scene.autocenter = True
    print("--- VPython Spinning Cube ---")
    print("Drag with your right mouse button to rotate the camera.")
    print("Scroll with your mouse wheel to zoom in and out.")

def main():
    """
    Main function to set up the scene, create the cube, and run the animation loop.
    """
    setup_scene()

    # Create the cube at the origin
    the_cube = box()

    # Set the initial rotation of the cube for a more interesting starting view
    angle_45_rad = math.pi / 4
    the_cube.rotate(angle=angle_45_rad, axis=vector(1, 0, 0))
    the_cube.rotate(angle=angle_45_rad, axis=vector(0, 0, 1))

    print("Starting animation... Close the VPython window to stop.")

    # Main animation loop
    try:
        while True:
            # The rate() function limits the loop to 50 iterations per second,
            # creating a smooth animation.
            rate(50)

            # Rotate the cube around the y-axis
            the_cube.rotate(angle=ROTATION_SPEED, axis=vector(0, 1, 0))
    except Exception as e:
        # VPython's window being closed can sometimes raise an exception.
        # This ensures the program exits gracefully.
        print(f"\nAnimation window closed. Exiting. ({e})")

if __name__ == "__main__":
    main()
