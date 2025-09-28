import pytest
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
if str(ROOT) not in sys.path:
    sys.path.append(str(ROOT))

from Games.Mastermind.mastermind_logic import (
    ComputerBreaker,
    MastermindConfig,
    MastermindGame,
    Score,
    evaluate_candidate,
    generate_palette,
    normalise_guess,
    score_guess,
)


@pytest.mark.parametrize(
    "secret, guess, expected",
    [
        (
            ("Red", "Blue", "Green", "Yellow"),
            ("Red", "Blue", "Green", "Yellow"),
            Score(4, 0),
        ),
        (
            ("Red", "Blue", "Green", "Yellow"),
            ("Blue", "Red", "Yellow", "Green"),
            Score(0, 4),
        ),
        (("Red", "Red", "Blue", "Blue"), ("Red", "Blue", "Red", "Blue"), Score(2, 2)),
        (
            ("Red", "Green", "Blue", "Yellow"),
            ("Orange", "Purple", "Red", "Green"),
            Score(0, 2),
        ),
    ],
)
def test_score_guess(secret, guess, expected):
    assert score_guess(secret, guess) == expected


def test_normalise_guess_handles_indices_and_names():
    palette = generate_palette(6)
    result = normalise_guess("1 blue 3, yellow", palette)
    assert result == [palette[0], palette[1], palette[2], palette[3]]


def test_mastermind_game_evaluate_matches_standalone_score():
    config = MastermindConfig(peg_count=4, colors=generate_palette(6))
    game = MastermindGame(config, secret_code=("Red", "Blue", "Green", "Yellow"))
    guess = ("Red", "Yellow", "Blue", "Purple")
    assert game.evaluate_guess(guess) == score_guess(game.secret_code, guess)


@pytest.mark.parametrize("allow_duplicates", [True, False])
def test_computer_breaker_eliminates_inconsistent_codes(allow_duplicates):
    config = MastermindConfig(
        peg_count=3, colors=("Red", "Blue", "Green"), allow_duplicates=allow_duplicates
    )
    breaker = ComputerBreaker(config)
    guess = ("Red", "Blue", "Green")
    score = Score(1, 1)
    breaker.register_feedback(guess, score)
    for candidate in breaker.candidates:
        assert evaluate_candidate(candidate, guess, score)


def test_normalise_guess_rejects_invalid_token():
    palette = generate_palette(4)
    with pytest.raises(ValueError):
        normalise_guess("red teal blue", palette)


def test_score_guess_handles_longer_codes():
    secret = ("Red", "Blue", "Green", "Yellow", "Orange")
    guess = ("Red", "Green", "Blue", "Purple", "Orange")
    score = score_guess(secret, guess)
    assert score == Score(exact=2, partial=2)
