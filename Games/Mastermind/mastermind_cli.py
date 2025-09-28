"""Command-line Mastermind with configurable difficulty and history export."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import List, Sequence

from mastermind_logic import (
    ComputerBreaker,
    MastermindConfig,
    MastermindGame,
    Score,
    generate_palette,
    normalise_guess,
)


DIFFICULTY_PRESETS = {
    "easy": dict(peg_count=4, color_count=6, allow_duplicates=True),
    "standard": dict(peg_count=4, color_count=8, allow_duplicates=True),
    "hard": dict(peg_count=5, color_count=8, allow_duplicates=True),
    "extreme": dict(peg_count=6, color_count=10, allow_duplicates=True),
}


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Play Mastermind in your terminal.")
    parser.add_argument(
        "--difficulty",
        choices=sorted(DIFFICULTY_PRESETS.keys()),
        default="standard",
        help="Preset controlling peg count and palette size (default: standard)",
    )
    parser.add_argument(
        "--peg-count",
        type=int,
        help="Override the number of pegs for custom games",
    )
    parser.add_argument(
        "--color-count",
        type=int,
        help="Override the number of colours for custom games",
    )
    parser.add_argument(
        "--no-duplicates",
        action="store_true",
        help="Disallow repeated colours in the secret code",
    )
    parser.add_argument(
        "--mode",
        choices=("breaker", "codemaker"),
        default="breaker",
        help="Play as the code breaker or set the secret for the computer",
    )
    parser.add_argument(
        "--history",
        type=Path,
        help="Write the guess history to the provided JSON file",
    )
    parser.add_argument(
        "--max-turns",
        type=int,
        default=12,
        help="Maximum number of turns before the game ends (default: 12)",
    )
    return parser.parse_args()


def build_config(args: argparse.Namespace) -> MastermindConfig:
    preset = DIFFICULTY_PRESETS[args.difficulty].copy()
    if args.peg_count:
        preset["peg_count"] = args.peg_count
    if args.color_count:
        preset["color_count"] = args.color_count
    config = MastermindConfig(
        peg_count=preset["peg_count"],
        colors=generate_palette(preset["color_count"]),
        allow_duplicates=not args.no_duplicates,
    )
    return config


def prompt_guess(palette: Sequence[str], peg_count: int) -> List[str]:
    colours_display = ", ".join(
        f"{idx+1}:{colour}" for idx, colour in enumerate(palette)
    )
    while True:
        raw = input(f"Enter your guess ({peg_count} colours) [{colours_display}]: ")
        try:
            guess = normalise_guess(raw, palette)
        except ValueError as exc:
            print(f"Error: {exc}")
            continue
        if len(guess) != peg_count:
            print(f"Please provide exactly {peg_count} colours.")
            continue
        return guess


def play_breaker(game: MastermindGame, max_turns: int, history: List[dict]) -> None:
    print("You are the code breaker. Try to guess the secret combination!")
    print("Available colours:", ", ".join(game.config.palette()))
    for turn in range(1, max_turns + 1):
        guess = prompt_guess(game.config.palette(), game.config.peg_count)
        score = game.evaluate_guess(guess)
        history.append({"turn": turn, "guess": list(guess), "score": score.__dict__})
        display_feedback(turn, guess, score)
        if game.is_win(score):
            print(f"Congratulations! You cracked the code in {turn} turns.")
            return
    print("Out of turns! The secret code was:", ", ".join(game.secret_code))


def request_secret(game: MastermindGame) -> List[str]:
    print("Set a secret code for the computer to solve.")
    return prompt_guess(game.config.palette(), game.config.peg_count)


def play_codemaker(game: MastermindGame, max_turns: int, history: List[dict]) -> None:
    secret = request_secret(game)
    game.set_secret(secret)
    print("Computer is thinking...")
    breaker = ComputerBreaker(game.config)
    for turn in range(1, max_turns + 1):
        guess = breaker.next_guess()
        score = game.evaluate_guess(guess)
        history.append({"turn": turn, "guess": list(guess), "score": score.__dict__})
        display_feedback(turn, guess, score)
        if game.is_win(score):
            print(f"The computer solved your code in {turn} turns!")
            return
        breaker.register_feedback(guess, score)
    print("Computer failed to deduce the code within the turn limit.")


def display_feedback(turn: int, guess: Sequence[str], score: Score) -> None:
    pegs = score.as_pegs()
    guess_text = ", ".join(guess)
    print(f"Turn {turn:02d}: {guess_text:<30} | Feedback: {pegs or '—'}")


def maybe_write_history(path: Path | None, history: List[dict]) -> None:
    if path is None:
        return
    data = {"history": history}
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2))
    print(f"History written to {path.resolve()}")


def main() -> None:
    args = parse_args()
    config = build_config(args)
    history: List[dict] = []
    game = MastermindGame(config)
    if args.mode == "breaker":
        play_breaker(game, args.max_turns, history)
    else:
        play_codemaker(game, args.max_turns, history)
    maybe_write_history(args.history, history)


if __name__ == "__main__":
    main()
