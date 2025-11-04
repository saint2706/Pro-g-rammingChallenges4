"""ClockSynced.py — Live System ASCII Clock
==========================================
Displays the current system time (HH:MM:SS or HH:MM) using multi-line ASCII
art digits. The clock is synchronized with the system time and updates
every second.

This script provides a real-time, visually appealing ASCII clock that can be
customized with various command-line options.

Examples:
    python ClockSynced.py --twelve-hour --color cyan
    python ClockSynced.py --no-seconds --blink-colon --color yellow
    python ClockSynced.py --twelve-hour --no-clear
"""

from __future__ import annotations

import argparse
import os
import sys
import time
from dataclasses import dataclass
from typing import List, Tuple

# --------------------------- ASCII Digit Definitions --------------------------- #

# Each digit is represented as a list of 6 strings, forming a 6-line character.
DIGIT_ART: List[List[str]] = [
    ["  ___  ", " / _ \\ ", "| | | |", "| | | |", "| |_| |", " \\___/ "],  # 0
    [" __ ", "/_ | ", " | | ", " | | ", " | | ", " |_| "],  # 1
    [" ___   ", "|__ \\  ", "   ) | ", "  / /  ", " / /_  ", "|____| "],  # 2
    [" ____  ", "|___ \\ ", "  __) |", " |__ < ", " ___) |", "|____/ "],  # 3
    [" _  _  ", "| || | ", "| || |_ ", "|__   _|", "   | | ", "   |_| "],  # 4
    [" _____ ", "| ____|", "| |__  ", "|___ \\ ", " ___) |", "|____/ "],  # 5
    ["  __   ", " / /   ", "/ /_   ", "|  _ \\  ", "| (_) |", " \\___/ "],  # 6
    ["______ ", "|____  |", "    / /", "   / / ", "  / /  ", " /_/   "],  # 7
    ["  ___  ", " / _ \\ ", "| (_) |", " > _ < ", "| (_) |", " \\___/ "],  # 8
    ["  ___  ", " / _ \\ ", "| (_) |", " \\__, |", "   / / ", "  /_/  "],  # 9
]

COLON_ON = ["   ", " _ ", "(_)", "   ", " _ ", "(_)"]
COLON_OFF = ["   ", "   ", "   ", "   ", "   ", "   "]  # Used for blinking effect.

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


# --------------------------- Configuration --------------------------- #


@dataclass(slots=True)
class Config:
    """Runtime configuration for the ASCII clock.

    Attributes:
        interval: The update interval in seconds.
        twelve_hour: Whether to use 12-hour format.
        show_seconds: Whether to display seconds.
        color: The ANSI color for the clock digits.
        blink_colon: Whether to make the colon blink.
        no_clear: Whether to disable clearing the screen between updates.
    """
    interval: float = 1.0
    twelve_hour: bool = False
    show_seconds: bool = True
    color: str | None = None
    blink_colon: bool = False
    no_clear: bool = False


# --------------------------- Utility -------------------------------- #


def clear_screen(enabled: bool = True) -> None:
    """Clears the terminal screen.

    Args:
        enabled: If False, the function does nothing.
    """
    if not enabled:
        return
    # For Unix-like systems (Linux, macOS).
    if os.name != "nt":
        sys.stdout.write("\033[2J\033[H")
        sys.stdout.flush()
    # For Windows.
    else:
        os.system("cls")


def colorize(text: str, color: str | None) -> str:
    """Applies ANSI color codes to the given text.

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


def get_current_time_digits(now: time.struct_time) -> Tuple[int, int, int, int, int, int]:
    """Extracts the digits of the current time.

    Args:
        now: The current time as a `time.struct_time` object.

    Returns:
        A tuple of six integers representing the digits of the time (h1, h2, m1, m2, s1, s2).
    """
    h, m, s = now.tm_hour, now.tm_min, now.tm_sec
    return *divmod(h, 10), *divmod(m, 10), *divmod(s, 10)


def format_hour_digits(hour24: int, twelve_hour: bool) -> Tuple[int, int]:
    """Formats the hour digits for 12-hour or 24-hour time.

    Args:
        hour24: The hour in 24-hour format.
        twelve_hour: Whether to format for 12-hour time.

    Returns:
        A tuple of two integers representing the hour digits.
    """
    if twelve_hour:
        hour = hour24 % 12
        if hour == 0:
            hour = 12
    else:
        hour = hour24
    return divmod(hour, 10) if hour >= 10 else (0, hour)


def render(cfg: Config, colon_visible: bool) -> str:
    """Renders the ASCII clock for the current time.

    Args:
        cfg: The runtime configuration.
        colon_visible: Whether the colon should be visible.

    Returns:
        A string representing the rendered ASCII clock.
    """
    now = time.localtime()
    h1, h2, m1, m2, s1, s2 = get_current_time_digits(now)
    h1, h2 = format_hour_digits(h1 * 10 + h2, cfg.twelve_hour)
    colon = COLON_ON if colon_visible else COLON_OFF if cfg.blink_colon else COLON_ON

    # Assemble the ASCII art pieces for the clock face.
    pieces: List[List[str]] = [
        DIGIT_ART[h1],
        DIGIT_ART[h2],
        colon,
        DIGIT_ART[m1],
        DIGIT_ART[m2],
    ]
    if cfg.show_seconds:
        pieces.extend([colon, DIGIT_ART[s1], DIGIT_ART[s2]])

    # Combine the pieces into a single multi-line string.
    lines: List[str] = []
    for i in range(6):
        lines.append("".join(p[i] for p in pieces))

    # Add a textual representation of the time below the ASCII art.
    hour_display = (now.tm_hour % 12 or 12) if cfg.twelve_hour else now.tm_hour
    suffix = (
        " AM"
        if cfg.twelve_hour and now.tm_hour < 12
        else (" PM" if cfg.twelve_hour else "")
    )
    if cfg.show_seconds:
        lines.append(f"\n{hour_display:02d}:{now.tm_min:02d}:{now.tm_sec:02d}{suffix}")
    else:
        lines.append(f"\n{hour_display:02d}:{now.tm_min:02d}{suffix}")
    return "\n".join(lines)


def build_parser() -> argparse.ArgumentParser:
    """Builds the command-line argument parser.

    Returns:
        An `argparse.ArgumentParser` instance.
    """
    p = argparse.ArgumentParser(description="Live ASCII clock (synced)")
    p.add_argument(
        "--twelve-hour", action="store_true", help="Display 12-hour format with AM/PM"
    )
    p.add_argument(
        "--no-seconds",
        dest="show_seconds",
        action="store_false",
        help="Hide seconds display",
    )
    p.add_argument(
        "--interval", type=float, default=1.0, help="Update interval in seconds"
    )
    p.add_argument(
        "--color", choices=sorted(ANSI_COLORS.keys()), help="ANSI color for digits"
    )
    p.add_argument(
        "--blink-colon", action="store_true", help="Blink the colon each interval"
    )
    p.add_argument(
        "--no-clear", action="store_true", help="Do not clear between frames"
    )
    return p


def run(cfg: Config) -> None:
    """Runs the main loop for the ASCII clock.

    Args:
        cfg: The runtime configuration.
    """
    print("Starting synced ASCII clock... Press Ctrl+C to stop.")
    colon_visible = True
    try:
        while True:
            clear_screen(not cfg.no_clear)
            frame = render(cfg, colon_visible)
            print(colorize(frame, cfg.color))
            if cfg.blink_colon:
                colon_visible = not colon_visible
            time.sleep(cfg.interval)
    except KeyboardInterrupt:
        print("\nClock stopped by user.")
        sys.exit(0)


def main(argv: List[str] | None = None) -> int:
    """The main entry point for the script.

    Args:
        argv: A list of command-line arguments.

    Returns:
        An integer exit code.
    """
    parser = build_parser()
    args = parser.parse_args(argv)
    cfg = Config(
        interval=max(0.05, args.interval),
        twelve_hour=args.twelve_hour,
        show_seconds=args.show_seconds,
        color=args.color,
        blink_colon=args.blink_colon,
        no_clear=args.no_clear,
    )
    run(cfg)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
