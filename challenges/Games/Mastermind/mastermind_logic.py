"""Core logic for Mastermind game variants.

The module exposes reusable helpers for scoring guesses, generating
candidate codes, and running simple computer strategies. The logic is
completely UI-agnostic so it can power both CLI and tkinter front-ends
as well as future language ports.
"""

from __future__ import annotations

from dataclasses import dataclass, field
import random
from typing import Iterable, List, Optional, Sequence, Tuple

DEFAULT_COLORS: Tuple[str, ...] = (
    "Red",
    "Blue",
    "Green",
    "Yellow",
    "Orange",
    "Purple",
)


@dataclass(frozen=True)
class Score:
    """Feedback pegs for a guess.

    Attributes:
        exact: Number of pegs that are the correct colour in the correct position.
        partial: Number of pegs that are the correct colour but in the wrong position.
    """

    exact: int
    partial: int

    def as_pegs(self) -> str:
        """Return a unicode representation using ● (exact) and ○ (partial)."""

        return "".join(["●" * self.exact, "○" * self.partial])


@dataclass
class MastermindConfig:
    """Game configuration options."""

    peg_count: int = 4
    colors: Sequence[str] = field(default_factory=lambda: DEFAULT_COLORS)
    allow_duplicates: bool = True

    def palette(self) -> Tuple[str, ...]:
        return tuple(self.colors)

    def validate(self) -> None:
        if self.peg_count <= 0:
            raise ValueError("peg_count must be positive")
        if len(self.colors) < 2:
            raise ValueError("at least two colours are required")
        if not self.allow_duplicates and self.peg_count > len(self.colors):
            raise ValueError(
                "peg_count cannot exceed number of colours when duplicates are disallowed"
            )


class MastermindGame:
    """Encapsulates a single Mastermind session."""

    def __init__(
        self,
        config: MastermindConfig,
        secret_code: Optional[Sequence[str]] = None,
        rng: Optional[random.Random] = None,
    ) -> None:
        self.config = config
        self.config.validate()
        self._rng = rng or random.Random()
        self.secret_code: Tuple[str, ...] = ()
        if secret_code is not None:
            self.set_secret(secret_code)
        else:
            self.generate_secret()

    # ------------------------------------------------------------------
    # Secret handling
    # ------------------------------------------------------------------
    def generate_secret(self) -> Tuple[str, ...]:
        """Generate a new secret code according to the configuration."""

        palette = self.config.palette()
        if self.config.allow_duplicates:
            self.secret_code = tuple(
                self._rng.choice(palette) for _ in range(self.config.peg_count)
            )
        else:
            self.secret_code = tuple(self._rng.sample(palette, self.config.peg_count))
        return self.secret_code

    def set_secret(self, secret: Sequence[str]) -> None:
        """Set a manual secret code (used for code-maker mode)."""

        if len(secret) != self.config.peg_count:
            raise ValueError("secret length does not match peg count")
        palette = set(self.config.palette())
        if any(color not in palette for color in secret):
            raise ValueError("secret contains colours outside the palette")
        if not self.config.allow_duplicates and len(set(secret)) != len(secret):
            raise ValueError("duplicate colours not allowed for this configuration")
        self.secret_code = tuple(secret)

    # ------------------------------------------------------------------
    # Guess evaluation
    # ------------------------------------------------------------------
    def evaluate_guess(self, guess: Sequence[str]) -> Score:
        """Evaluate a guess against the secret code."""

        if len(guess) != self.config.peg_count:
            raise ValueError("guess length does not match peg count")
        palette = set(self.config.palette())
        if any(color not in palette for color in guess):
            raise ValueError("guess contains colours outside the palette")

        secret = list(self.secret_code)
        guess_list = list(guess)

        exact = sum(s == g for s, g in zip(secret, guess_list))
        # Remove exact matches before counting partials
        secret_remaining = []
        guess_remaining = []
        for s, g in zip(secret, guess_list):
            if s != g:
                secret_remaining.append(s)
                guess_remaining.append(g)

        partial = 0
        secret_counts: dict[str, int] = {}
        for colour in secret_remaining:
            secret_counts[colour] = secret_counts.get(colour, 0) + 1
        for colour in guess_remaining:
            if secret_counts.get(colour, 0) > 0:
                partial += 1
                secret_counts[colour] -= 1

        return Score(exact=exact, partial=partial)

    def is_win(self, score: Score) -> bool:
        return score.exact == self.config.peg_count


# ----------------------------------------------------------------------
# Helper utilities
# ----------------------------------------------------------------------


def normalise_guess(text: str, palette: Sequence[str]) -> List[str]:
    """Parse user input into a sequence of colours.

    Accepts comma or whitespace separated tokens. Matching is case-insensitive
    and also supports numeric indices (1-based) referencing the palette.
    """

    tokens = [
        token.strip() for token in text.replace(",", " ").split() if token.strip()
    ]
    if not tokens:
        raise ValueError("empty guess provided")

    normalised = []
    palette_lower = {colour.lower(): colour for colour in palette}
    for token in tokens:
        if token.lower() in palette_lower:
            normalised.append(palette_lower[token.lower()])
            continue
        if token.isdigit():
            idx = int(token) - 1
            if 0 <= idx < len(palette):
                normalised.append(palette[idx])
                continue
        raise ValueError(f"Unknown colour token: {token}")
    return normalised


def generate_palette(count: int) -> Tuple[str, ...]:
    """Return a palette of the requested size.

    Extends DEFAULT_COLORS with generic labels if a larger palette is required.
    """

    if count <= len(DEFAULT_COLORS):
        return tuple(DEFAULT_COLORS[:count])
    extras = tuple(f"Colour {i}" for i in range(len(DEFAULT_COLORS) + 1, count + 1))
    return tuple(DEFAULT_COLORS) + extras


# ----------------------------------------------------------------------
# Computer code-breaker
# ----------------------------------------------------------------------


class ComputerBreaker:
    """Basic elimination strategy for Mastermind."""

    def __init__(self, config: MastermindConfig):
        self.config = config
        self.candidates = list(generate_all_codes(config))
        self._last_guess: Optional[Tuple[str, ...]] = None

    def next_guess(self) -> Tuple[str, ...]:
        if not self.candidates:
            raise RuntimeError("No candidates remaining; inconsistent feedback")
        self._last_guess = self.candidates[0]
        return self._last_guess

    def register_feedback(self, guess: Sequence[str], score: Score) -> None:
        self.candidates = [
            candidate
            for candidate in self.candidates
            if evaluate_candidate(candidate, guess, score)
        ]


def evaluate_candidate(
    candidate: Sequence[str], guess: Sequence[str], score: Score
) -> bool:
    """Return True if candidate would produce the provided score for guess."""

    candidate_score = score_guess(candidate, guess)
    return candidate_score == score


def score_guess(secret: Sequence[str], guess: Sequence[str]) -> Score:
    """Standalone scoring helper used by tests and candidate evaluation."""

    game = MastermindGame(
        MastermindConfig(
            peg_count=len(secret), colors=tuple(sorted(set(secret) | set(guess)))
        ),
        secret_code=secret,
    )
    return game.evaluate_guess(guess)


def generate_all_codes(config: MastermindConfig) -> Iterable[Tuple[str, ...]]:
    """Generate every possible code for the provided configuration."""

    palette = config.palette()
    if config.allow_duplicates:
        return _product_repeat(palette, config.peg_count)
    return _product_without_replacement(palette, config.peg_count)


def _product_repeat(options: Sequence[str], length: int) -> Iterable[Tuple[str, ...]]:
    if length == 0:
        yield ()
    else:
        for option in options:
            for suffix in _product_repeat(options, length - 1):
                yield (option, *suffix)


def _product_without_replacement(
    options: Sequence[str], length: int
) -> Iterable[Tuple[str, ...]]:
    if length == 0:
        yield ()
    else:
        for idx, option in enumerate(options):
            remaining = options[:idx] + options[idx + 1 :]
            for suffix in _product_without_replacement(remaining, length - 1):
                yield (option, *suffix)
