import os
import time
import sys
from typing import List

# ASCII art components for each segment of a digit
EMPTY_PART = " " * 3
TOP = " _ "
MIDDLE = "|_|"
BOTTOM = "|_|"
PIPE_MIDDLE = "| |"
PIPE_LEFT = "|  "
PIPE_RIGHT = "  |"
MIDDLE_LEFT = "|_ "
MIDDLE_RIGHT = " _|"
COLON_DOT = "."

# A data structure mapping each digit (0-9) to its three-line ASCII art representation.
DIGIT_ART = [
    [TOP, PIPE_MIDDLE, MIDDLE],      # 0
    [EMPTY_PART, PIPE_RIGHT, PIPE_RIGHT],  # 1
    [TOP, MIDDLE_RIGHT, MIDDLE_LEFT],    # 2
    [TOP, MIDDLE_RIGHT, MIDDLE_RIGHT],   # 3
    [EMPTY_PART, MIDDLE, PIPE_RIGHT],      # 4
    [TOP, MIDDLE_LEFT, MIDDLE_RIGHT],    # 5
    [TOP, MIDDLE_LEFT, MIDDLE],      # 6
    [TOP, PIPE_RIGHT, PIPE_RIGHT],     # 7
    [TOP, MIDDLE, MIDDLE],      # 8
    [TOP, MIDDLE, MIDDLE_RIGHT],   # 9
]

# The three lines for the colon separator
COLON_ART = [EMPTY_PART, COLON_DOT, COLON_DOT]

def get_digit_art(digit: int) -> List[str]:
    """Returns the three-line ASCII art for a single digit."""
    if 0 <= digit <= 9:
        return DIGIT_ART[digit]
    return [EMPTY_PART] * 3  # Return empty space for invalid digits

def display_time(hour: int, minute: int):
    """Clears the screen and displays the given time in ASCII art."""
    # Clear the terminal screen
    os.system("cls" if os.name == "nt" else "clear")

    # Get the individual digits for HH:MM
    h1, h2 = divmod(hour, 10) if hour > 9 else (0, hour)
    m1, m2 = divmod(minute, 10) if minute > 9 else (0, minute)

    # Get the ASCII art for each digit
    art_h1 = get_digit_art(h1)
    art_h2 = get_digit_art(h2)
    art_m1 = get_digit_art(m1)
    art_m2 = get_digit_art(m2)

    # Print the clock line by line
    for i in range(3):
        print(f"{art_h1[i]} {art_h2[i]} {COLON_ART[i]} {art_m1[i]} {art_m2[i]}")

def main():
    """
    Main function to run the simulated clock loop.
    This clock is NOT synced with the system time and simulates a full 24-hour
    cycle, advancing by one minute for every second of real time.
    """
    print("Starting simulated ASCII clock... Press Ctrl+C to stop.")
    time.sleep(2)

    try:
        # Loop through every minute in a 24-hour day
        for total_minutes in range(24 * 60):
            hour = total_minutes // 60
            minute = total_minutes % 60

            display_time(hour, minute)

            # Wait for 1 second to simulate the next minute
            time.sleep(1)

    except KeyboardInterrupt:
        print("\nClock simulation stopped by user.")
        sys.exit(0)

if __name__ == "__main__":
    main()
