import os
import time
import sys
from typing import List, Tuple

# A single data structure containing the 6-line ASCII art for each digit (0-9).
DIGIT_ART = [
    ["  ___  ", " / _ \\ ", "| | | |", "| | | |", "| |_| |", " \\___/ "],  # 0
    [" __ ", "/_ | ", " | | ", " | | ", " | | ", " |_| "],    # 1
    [" ___   ", "|__ \\  ", "   ) | ", "  / /  ", " / /_  ", "|____| "],  # 2
    [" ____  ", "|___ \\ ", "  __) |", " |__ < ", " ___) |", "|____/ "],  # 3
    [" _  _  ", "| || | ", "| || |_ ", "|__   _|", "   | | ", "   |_| "],  # 4
    [" _____ ", "| ____|", "| |__  ", "|___ \\ ", " ___) |", "|____/ "],  # 5
    ["  __   ", " / /   ", "/ /_   ", "|  _ \\  ", "| (_) |", " \\___/ "],  # 6
    ["______ ", "|____  |", "    / /", "   / / ", "  / /  ", " /_/   "],  # 7
    ["  ___  ", " / _ \\ ", "| (_) |", " > _ < ", "| (_) |", " \\___/ "],  # 8
    ["  ___  ", " / _ \\ ", "| (_) |", " \\__, |", "   / / ", "  /_/  "],  # 9
]

# ASCII art for the colon separator
COLON_ART = ["   ", " _ ", "(_)", "   ", " _ ", "(_)"]
SPACE_ART = [" " * 3] * 6 # A 3-space gap between digit groups

def get_current_time_digits() -> Tuple[int, ...]:
    """Gets the current system time and returns its digits as (h1, h2, m1, m2, s1, s2)."""
    now = time.localtime()
    hour, minute, second = now.tm_hour, now.tm_min, now.tm_sec

    h1, h2 = divmod(hour, 10)
    m1, m2 = divmod(minute, 10)
    s1, s2 = divmod(second, 10)

    return h1, h2, m1, m2, s1, s2

def display_time(h1: int, h2: int, m1: int, m2: int, s1: int, s2: int):
    """Clears the console and prints the ASCII representation of the time."""
    # Clear the terminal screen. Corrected to not show 'date' and 'cal' on Linux/macOS.
    os.system("cls" if os.name == "nt" else "clear")

    # Get the ASCII art for each part of the clock
    art_pieces = [
        DIGIT_ART[h1], DIGIT_ART[h2], COLON_ART,
        DIGIT_ART[m1], DIGIT_ART[m2], COLON_ART,
        DIGIT_ART[s1], DIGIT_ART[s2]
    ]

    # Print the clock line by line
    for i in range(6): # Each digit has 6 lines
        line = "".join(piece[i] for piece in art_pieces)
        print(line)

    # Also print the standard time format below for reference
    print(f"\n{h1}{h2}:{m1}{m2}:{s1}{s2}")

def main():
    """Main loop to display the synced ASCII clock every second."""
    print("Starting synced ASCII clock... Press Ctrl+C to stop.")

    try:
        while True:
            digits = get_current_time_digits()
            display_time(*digits)
            time.sleep(1)
    except KeyboardInterrupt:
        print("\nClock stopped by user.")
        sys.exit(0)

if __name__ == "__main__":
    main()
