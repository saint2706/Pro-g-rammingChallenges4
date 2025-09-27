# Mastermind (Challenge #128)

Python implementation of the classic Mastermind puzzle with both a rich
terminal experience and a tkinter GUI. The build ships a reusable logic
module so you can embed the scoring engine into other challenges or test
harnesses.

## Features

- **Two play modes**: break the computer's secret code or become the code
  maker and watch the computer deduce your pattern.
- **Configurable difficulty**: adjust peg count, colour palette size, and
  duplicate allowance from either interface.
- **Feedback pegs**: guesses are scored with the classic black/white
  (exact/partial) peg system represented by `●` and `○`.
- **History export**: CLI session logs can be written to JSON for later
  analysis or sharing.

## Requirements

- Python 3.10+
- tkinter (bundled with most desktop Python installs)

Optional (for development):

```bash
python -m pip install -e .[games,developer]
```

## Command-line interface

```bash
cd Games/Mastermind
python mastermind_cli.py [options]
```

### Key arguments

| Argument | Description |
|----------|-------------|
| `--difficulty {easy,standard,hard,extreme}` | Preset peg and colour counts. |
| `--peg-count N` | Override the number of pegs. |
| `--color-count N` | Override palette size. |
| `--no-duplicates` | Disallow repeated colours. |
| `--mode {breaker,codemaker}` | Choose to guess the computer's code or set one for it. |
| `--history path.json` | Write turn-by-turn history to a JSON file. |
| `--max-turns N` | Limit the number of turns (default 12). |

### Sample sessions

```text
$ python mastermind_cli.py --difficulty easy --mode breaker
You are the code breaker. Try to guess the secret combination!
Available colours: Red, Blue, Green, Yellow, Orange, Purple
Enter your guess (4 colours) [1:Red, 2:Blue, 3:Green, 4:Yellow, 5:Orange, 6:Purple]: red blue green yellow
Turn 01: Red, Blue, Green, Yellow        | Feedback: ●○
...
Congratulations! You cracked the code in 6 turns.
```

```text
$ python mastermind_cli.py --mode codemaker --peg-count 5 --color-count 7 --history history.json
Set a secret code for the computer to solve.
Enter your guess (5 colours) [1:Red, 2:Blue, 3:Green, 4:Yellow, 5:Orange, 6:Purple, 7:Colour 7]: 1 1 2 3 4
Computer is thinking...
Turn 01: Red, Red, Blue, Green, Yellow   | Feedback: ●●○
Turn 02: Red, Red, Blue, Yellow, Orange  | Feedback: ●●●○
Turn 03: Red, Red, Blue, Yellow, Purple  | Feedback: ●●●●○
Turn 04: Red, Red, Blue, Yellow, Colour 7 | Feedback: ●●●●●
The computer solved your code in 4 turns!
History written to /path/to/history.json
```

## GUI

```bash
cd Games/Mastermind
python mastermind_gui.py
```

- Use the **Configuration** panel to choose mode, peg count, colours, and
  whether duplicates are allowed.
- Click **Start new game** to generate a secret or prepare to set one.
- In breaker mode, select colours for each peg and press **Submit Guess**
  to receive feedback.
- In code maker mode, set your secret with the dedicated comboboxes and
  watch the computer iterate through guesses (one every ~0.6 s).
- The **History** panel mirrors the CLI output, displaying each guess with
  its feedback peg string.

## Tests

```bash
cd Games/Mastermind
python -m pytest
```

Unit tests cover the scoring logic and candidate filtering so regression
runs remain fast even without the GUI.
