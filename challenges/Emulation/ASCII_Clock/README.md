# ASCII Clock

This solution provides two scripts for displaying the current time as multi-line ASCII art in the terminal.

## Description

The `ASCII_Clock` directory contains two Python scripts:
- `ClockSynced.py`: Displays the current system time, updating every second. It is synchronized with the actual time.
- `ClockNotSynced.py`: Displays a continuously running clock that is not synchronized to the system time. It's more of a visual demonstration.

Both scripts render the time using large ASCII digits and offer several command-line options for customization.

## Dependencies

- Python 3

No external libraries are required to run these scripts.

## Usage

You can run both scripts from the command line with various options to customize the appearance and behavior of the clock.

### `ClockSynced.py`

This script is ideal for displaying an accurate, real-time clock in your terminal.

**Basic Example:**
```bash
python "challenges/Emulation/ASCII_Clock/ClockSynced.py"
```

**Advanced Example:**
To display the time in 12-hour format, in cyan, with a blinking colon, and without showing seconds:
```bash
python "challenges/Emulation/ASCII_Clock/ClockSynced.py" --twelve-hour --color cyan --blink-colon --no-seconds
```

**Command-Line Arguments:**
- `--twelve-hour`: Display the time in 12-hour format with AM/PM.
- `--no-seconds`: Hide the seconds display.
- `--interval`: Set the update interval in seconds.
- `--color`: Set the ANSI color for the digits.
- `--blink-colon`: Make the colon blink with each update.
- `--no-clear`: Do not clear the screen between updates.

### `ClockNotSynced.py`

This script is more of a visual demonstration and does not keep accurate time.

**Basic Example:**
```bash
python "challenges/Emulation/ASCII_Clock/ClockNotSynced.py"
```

**Advanced Example:**
To display the clock in a different color and with a faster refresh rate:
```bash
python "challenges/Emulation/ASCII_Clock/ClockNotSynced.py" --color green --refresh 0.1
```

**Command-Line Arguments:**
- `--refresh`: The refresh rate in seconds (default is 0.05).
- `--color`: The ANSI color for the digits.
- `--twelve-hour`: Display the time in 12-hour format.
