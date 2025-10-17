"""Entry point for the RPG engine demo."""

from __future__ import annotations

from engine.game import Game


def main() -> None:
    game = Game()
    game.run()


if __name__ == "__main__":
    main()
