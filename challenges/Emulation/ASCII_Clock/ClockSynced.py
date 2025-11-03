"""ClockSynced.py — Live System ASCII Clock
==========================================
Displays the current system time (HH:MM:SS or HH:MM) using multi-line ASCII
art digits. Includes options for 12/24 hour format, blinking colon, color, and
optional hiding of seconds for a cleaner look.

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
COLON_OFF = ["   ", "   ", "   ", "   ", "   ", "   "]  # Used for blinking

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
    interval: float = 1.0
    twelve_hour: bool = False
    show_seconds: bool = True
    color: str | None = None
    blink_colon: bool = False
    no_clear: bool = False


# --------------------------- Utility -------------------------------- #


def clear_screen(enabled: bool = True) -> None:
    if not enabled:
        return
    if os.name != "nt":
        sys.stdout.write("\033[2J\033[H")
        sys.stdout.flush()
    else:
        os.system("cls")


def colorize(text: str, color: str | None) -> str:
    if not color:
        return text
    code = ANSI_COLORS.get(color.lower())
    if not code:
        return text
    return f"{code}{text}{ANSI_RESET}"


def get_current_time_digits(now: time.struct_time) -> Tuple[int, int, int, int, int, int]:
    h, m, s = now.tm_hour, now.tm_min, now.tm_sec
    return *divmod(h, 10), *divmod(m, 10), *divmod(s, 10)


def format_hour_digits(hour24: int, twelve_hour: bool) -> Tuple[int, int]:
    if twelve_hour:
        hour = hour24 % 12
        if hour == 0:
            hour = 12
    else:
        hour = hour24
    return divmod(hour, 10) if hour >= 10 else (0, hour)


def render(cfg: Config, colon_visible: bool) -> str:
    now = time.localtime()
    h1, h2, m1, m2, s1, s2 = get_current_time_digits(now)
    h1, h2 = format_hour_digits(h1 * 10 + h2, cfg.twelve_hour)
    colon = COLON_ON if colon_visible else COLON_OFF if cfg.blink_colon else COLON_ON

    pieces: List[List[str]] = [
        DIGIT_ART[h1],
        DIGIT_ART[h2],
        colon,
        DIGIT_ART[m1],
        DIGIT_ART[m2],
    ]
    if cfg.show_seconds:
        pieces.extend([colon, DIGIT_ART[s1], DIGIT_ART[s2]])

    lines: List[str] = []
    for i in range(6):
        lines.append("".join(p[i] for p in pieces))

    # Add textual time for quick copy/paste
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
