"""ClockNotSynced.py — Simulated ASCII Clock (Minute Ticker)
===========================================================
Simulates a full 24-hour clock (HH:MM) without using the system time.
Instead, it advances one minute per real-time interval (default: 1 second).

This script serves as a teaching example to demonstrate:
 - Basic ASCII art rendering from component pieces.
 - Simple time arithmetic for simulating a clock.
 - Command-line argument parsing with `argparse`.
 - Cross-platform terminal clearing for clean animations.

You can control the simulation speed, start time, duration, and display
format (12/24 hour) through command-line arguments.

Examples:
    # Run the clock faster in 12-hour format.
    python ClockNotSynced.py --speed 0.05 --twelve-hour

    # Start at 1:45 PM, run for 120 minutes, and color the output cyan.
    python ClockNotSynced.py --start 13:45 --duration 120 --color cyan

Press Ctrl+C to exit early.
"""

from __future__ import annotations

import argparse
import os
import sys
import time
from dataclasses import dataclass
from typing import List, Tuple

# --------------------------- ASCII Digit Definitions --------------------------- #

# Define 3-character wide components for building ASCII digits.
EMPTY_PART = " " * 3
TOP = " _ "
MIDDLE = "|_|"
BOTTOM = "|_|"  # Kept for clarity, though not distinct from MIDDLE.
PIPE_MIDDLE = "| |"
PIPE_LEFT = "|  "
PIPE_RIGHT = "  |"
MIDDLE_LEFT = "|_ "
MIDDLE_RIGHT = " _|"
COLON_DOT = "."

# Each digit is defined as a list of three strings, representing its top, middle, and bottom rows.
DIGIT_ART: List[List[str]] = [
    [TOP, PIPE_MIDDLE, MIDDLE],  # 0
    [EMPTY_PART, PIPE_RIGHT, PIPE_RIGHT],  # 1
    [TOP, MIDDLE_RIGHT, MIDDLE_LEFT],  # 2
    [TOP, MIDDLE_RIGHT, MIDDLE_RIGHT],  # 3
    [EMPTY_PART, MIDDLE, PIPE_RIGHT],  # 4
    [TOP, MIDDLE_LEFT, MIDDLE_RIGHT],  # 5
    [TOP, MIDDLE_LEFT, MIDDLE],  # 6
    [TOP, PIPE_RIGHT, PIPE_RIGHT],  # 7
    [TOP, MIDDLE, MIDDLE],  # 8
    [TOP, MIDDLE, MIDDLE_RIGHT],  # 9
]

COLON_ART = [EMPTY_PART, COLON_DOT, COLON_DOT]


# --------------------------- Configuration --------------------------- #


@dataclass(slots=True)
class Config:
    """Runtime settings for the simulated clock.

    Attributes:
        speed: Seconds of real time per simulated minute.
        start_minutes: Starting minute offset into the 24h day (0-1439).
        duration: How many simulated minutes to run (None = full 24h from start).
        twelve_hour: Whether to display time in 12-hour format.
        color: Optional ANSI color name.
        no_clear: If True, don't clear between frames (useful for logging/testing).
    """
    speed: float = 1.0
    start_minutes: int = 0
    duration: int | None = None
    twelve_hour: bool = False
    color: str | None = None
    no_clear: bool = False


ANSI_COLORS = {
    "black": "\033[30m",
    "red": "\033[31m",
    "green": "\033[32m",
    "yellow": "\033[33m",
    "blue": "\033[34m",
    "magenta": "\033[35m",
    "cyan": "\033[36m",
    "white": "\033[37m",
}
ANSI_RESET = "\033[0m"


# --------------------------- Utility Functions --------------------------- #


def clear_screen(enabled: bool = True) -> None:
    """Clear the terminal screen in a cross-platform way.

    Args:
        enabled: If False, the function does nothing.
    """
    if not enabled:
        return
    if os.name != "nt":
        # For Unix-like systems, use ANSI escape codes.
        sys.stdout.write("\033[2J\033[H")
        sys.stdout.flush()
    else:
        # For Windows, use the 'cls' command.
        os.system("cls")


def parse_time_string(ts: str) -> int:
    """Parse an "HH:MM" string into total minutes from midnight.

    Args:
        ts: The time string to parse (e.g., "13:45").

    Returns:
        The total number of minutes from midnight.

    Raises:
        ValueError: If the time string is in an invalid format.
    """
    try:
        parts = ts.split(":")
        if len(parts) != 2:
            raise ValueError
        h = int(parts[0])
        m = int(parts[1])
        if not (0 <= h < 24 and 0 <= m < 60):
            raise ValueError
    except ValueError as e:
        raise ValueError("Time must be in HH:MM 24-hour format (e.g., 13:45)") from e
    return h * 60 + m


def get_digit_art(digit: int) -> List[str]:
    """Retrieve the three-line ASCII art for a single digit.

    Args:
        digit: The integer digit (0-9).

    Returns:
        A list of three strings representing the ASCII art for the digit.
    """
    if 0 <= digit <= 9:
        return DIGIT_ART[digit]
    return [EMPTY_PART] * 3


def format_hour(hour24: int, twelve_hour: bool) -> Tuple[int, int]:
    """Convert a 24-hour format hour to its display digits.

    This function handles the conversion to 12-hour format if required.

    Args:
        hour24: The hour in 24-hour format (0-23).
        twelve_hour: A boolean indicating whether to use 12-hour format.

    Returns:
        A tuple containing the tens and ones digits of the hour.
    """
    if twelve_hour:
        hour = hour24 % 12
        if hour == 0:
            hour = 12  # 12 AM or 12 PM.
    else:
        hour = hour24
    if hour >= 10:
        return divmod(hour, 10)
    return 0, hour


def render_time(hour: int, minute: int, cfg: Config) -> str:
    """Build the multi-line ASCII clock string for a given time.

    Args:
        hour: The hour to display.
        minute: The minute to display.
        cfg: The runtime configuration.

    Returns:
        A string containing the rendered ASCII clock.
    """
    h1, h2 = format_hour(hour, cfg.twelve_hour)
    m1, m2 = divmod(minute, 10) if minute > 9 else (0, minute)

    # Assemble the ASCII art pieces for the clock face.
    parts = [
        get_digit_art(h1),
        get_digit_art(h2),
        COLON_ART,
        get_digit_art(m1),
        get_digit_art(m2),
    ]
    lines = []
    for i in range(3):
        lines.append(" ".join(p[i] for p in parts))

    # Add an AM/PM indicator if in 12-hour mode.
    if cfg.twelve_hour:
        am_pm = "AM" if hour < 12 else "PM"
        lines.append(am_pm.center(len(lines[0])))
    return "\n".join(lines)


def colorize(text: str, color: str | None) -> str:
    """Apply ANSI color codes to the given text.

    Args:
        text: The text to colorize.
        color: The name of the color to apply.

    Returns:
        The colorized text string.
    """
    if not color:
        return text
    code = ANSI_COLORS.get(color.lower())
    if not code:
        return text
    return f"{code}{text}{ANSI_RESET}"


# --------------------------- Core Simulation --------------------------- #


def run(cfg: Config) -> None:
    """Run the simulated clock according to the configuration.

    Args:
        cfg: The runtime configuration.
    """
    total_minutes = (
        cfg.duration if cfg.duration is not None else (24 * 60 - cfg.start_minutes)
    )
    end_minute = cfg.start_minutes + total_minutes

    try:
        # Loop through each minute of the simulation.
        for current in range(cfg.start_minutes, end_minute):
            hour = current // 60 % 24
            minute = current % 60
            clear_screen(not cfg.no_clear)
            ascii_time = render_time(hour, minute, cfg)
            print(colorize(ascii_time, cfg.color))
            # The sleep interval determines the speed of the simulation.
            time.sleep(cfg.speed)
    except KeyboardInterrupt:
        print("\nClock simulation stopped by user.")
        sys.exit(0)


# --------------------------- CLI Interface --------------------------- #


def build_parser() -> argparse.ArgumentParser:
    """Build the command-line argument parser.

    Returns:
        An `argparse.ArgumentParser` instance.
    """
    p = argparse.ArgumentParser(description="Simulated (unsynced) ASCII clock")
    p.add_argument(
        "--speed",
        type=float,
        default=1.0,
        help="Seconds of real time per simulated minute",
    )
    p.add_argument(
        "--start", type=str, default="00:00", help="Starting time (HH:MM) 24h format"
    )
    p.add_argument(
        "--duration",
        type=int,
        help="Duration in simulated minutes (default = until 24h reached)",
    )
    p.add_argument(
        "--twelve-hour",
        action="store_true",
        help="Display clock in 12-hour format with AM/PM",
    )
    p.add_argument(
        "--color",
        choices=sorted(ANSI_COLORS.keys()),
        help="Optional ANSI color for digits",
    )
    p.add_argument(
        "--no-clear", action="store_true", help="Do not clear screen between updates"
    )
    return p


def main(argv: List[str] | None = None) -> int:
    """The main entry point for the script.

    Args:
        argv: A list of command-line arguments.

    Returns:
        An integer exit code.
    """
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        start_minutes = parse_time_string(args.start)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1

    cfg = Config(
        speed=max(0.001, args.speed),
        start_minutes=start_minutes,
        duration=args.duration,
        twelve_hour=args.twelve_hour,
        color=args.color,
        no_clear=args.no_clear,
    )
    run(cfg)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
