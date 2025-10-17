"""
Simon Game (Pygame Implementation)
----------------------------------
Modern, well-documented, and beginner-friendly implementation of the classic Simon memory game using Pygame.
Features:
- Modular class-based design
- Cross-platform asset loading
- Clear comments and docstrings
- Optimized for readability and maintainability
"""

import os
import random
import sys
import time
from enum import Enum
from pathlib import Path
from typing import Dict, List, Tuple

import pygame

# --- Constants ---
# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
# Each button has a normal and a light color for flashing
BUTTON_COLORS = {
    "GREEN": ((0, 100, 0), (0, 255, 0)),
    "RED": ((100, 0, 0), (255, 0, 0)),
    "YELLOW": ((150, 150, 0), (255, 255, 0)),
    "BLUE": ((0, 0, 100), (0, 0, 255)),
}


class GameState(Enum):
    """
    Enumeration for the different states of the game.
    """

    MENU = 1
    PLAYING = 2
    GAME_OVER = 3


class SimonGame:
    """
    Encapsulates the entire Simon game logic, state, and rendering.
    Handles menu, gameplay, and game over screens.
    """

    def __init__(self, screen_width: int = 600, screen_height: int = 700) -> None:
        pygame.init()
        pygame.mixer.init()

        self.screen = pygame.display.set_mode((screen_width, screen_height))
        pygame.display.set_caption("Simon")
        self.clock = pygame.time.Clock()

        self.game_state: GameState = GameState.MENU
        self.score: int = 0
        self.pattern: List[str] = []
        self.player_input: List[str] = []

        self._load_assets()
        self._create_buttons()

    def _load_assets(self) -> None:
        """
        Loads all game assets (fonts, images, sounds) using cross-platform paths.
        Optimized for maintainability and clarity.
        """
        base_asset_path = Path(__file__).resolve().parent / "Assets"
        font_path = base_asset_path / "Fonts" / "vermin_vibes.ttf"
        img_path = base_asset_path / "Images"
        audio_path = base_asset_path / "Audio"

        self.font = pygame.font.Font(str(font_path), 20)
        self.title_font = pygame.font.Font(str(font_path), 50)

        # Load and set window icon
        icon_img = pygame.image.load(str(img_path / "simon_logo.png"))
        pygame.display.set_icon(icon_img)

        # Load images and scale as needed
        def load_scaled(name: str, size: Tuple[int, int]) -> pygame.Surface:
            return pygame.transform.scale(
                pygame.image.load(str(img_path / name)), size
            )

        self.images: Dict[str, pygame.Surface] = {
            "logo": pygame.transform.scale(icon_img, (300, 300)),
            "start": load_scaled("start_button.png", (240, 90)),
            "again": load_scaled("again_button.png", (240, 90)),
            "exit": load_scaled("exit_button.png", (240, 90)),
        }

        # Load sounds
        def load_sound(name: str) -> pygame.mixer.Sound:
            return pygame.mixer.Sound(str(audio_path / name))

        self.sounds: Dict[str, pygame.mixer.Sound] = {
            "GREEN": load_sound("green.wav"),
            "RED": load_sound("red.wav"),
            "YELLOW": load_sound("yellow.wav"),
            "BLUE": load_sound("blue.wav"),
            "LOSE": load_sound("Fail-sound-effect.wav"),
            "MENU": load_sound("wethands.ogg"),
        }

    def _create_buttons(self) -> None:
        """
        Creates Rect objects for all clickable areas (color buttons and menu/gameover buttons).
        """
        self.buttons: Dict[str, pygame.Rect] = {
            "GREEN": pygame.Rect(50, 150, 250, 250),
            "RED": pygame.Rect(300, 150, 250, 250),
            "YELLOW": pygame.Rect(50, 400, 250, 250),
            "BLUE": pygame.Rect(300, 400, 250, 250),
            "START_MENU": pygame.Rect(180, 530, 240, 90),
            "AGAIN_GAMEOVER": pygame.Rect(180, 300, 240, 90),
            "EXIT_GAMEOVER": pygame.Rect(180, 450, 240, 90),
        }

    def run(self) -> None:
        """
        The main game loop, managed by a state machine.
        Handles transitions between menu, playing, and game over states.
        """
        while True:
            if self.game_state == GameState.MENU:
                self._run_menu()
            elif self.game_state == GameState.PLAYING:
                self._run_playing()
            elif self.game_state == GameState.GAME_OVER:
                self._run_game_over()

    def _run_menu(self) -> None:
        """
        Handles the main menu screen and transitions to gameplay.
        """
        self.sounds["MENU"].play(-1)
        while self.game_state == GameState.MENU:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self._quit_game()
                if event.type == pygame.MOUSEBUTTONUP and self.buttons[
                    "START_MENU"
                ].collidepoint(event.pos):
                    self.sounds["MENU"].stop()
                    self.game_state = GameState.PLAYING
                    return

            self.screen.fill(BLACK)
            self.screen.blit(self.images["logo"], (150, 150))
            self.screen.blit(self.images["start"], (180, 530))
            pygame.display.update()
            self.clock.tick(60)

    def _run_playing(self) -> None:
        """
        Handles the main gameplay loop, alternating between computer and player turns.
        """
        self.score = 0
        self.pattern = []
        self.player_input = []
        turn = "COMPUTER"  # Computer shows pattern first

        while self.game_state == GameState.PLAYING:
            self._draw_game_screen()

            if turn == "COMPUTER":
                self.player_input = []
                pygame.time.delay(1000)
                self.pattern.append(random.choice(list(BUTTON_COLORS.keys())))
                self._show_pattern()
                turn = "PLAYER"
            else:  # Player's turn
                self._handle_player_turn()
                if len(self.player_input) == len(self.pattern):
                    self.score = len(self.pattern)
                    turn = "COMPUTER"

    def _handle_player_turn(self) -> None:
        """
        Handles the player's turn, collecting input and checking correctness.
        Player has 3 seconds per move; resets timer after each correct press.
        """
        start_time = time.time()
        while time.time() - start_time < 3:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self._quit_game()
                if event.type == pygame.MOUSEBUTTONUP:
                    for color, rect in self.buttons.items():
                        if color in BUTTON_COLORS and rect.collidepoint(event.pos):
                            self._flash_color(color)
                            self.player_input.append(color)
                            if not self._check_player_input():
                                self.game_state = GameState.GAME_OVER
                                return
                            start_time = (
                                time.time()
                            )  # Reset timer after each correct press
                            if len(self.player_input) == len(self.pattern):
                                return
            self._draw_game_screen()
        # If the loop finishes due to timeout
        self.game_state = GameState.GAME_OVER

    def _check_player_input(self) -> bool:
        """
        Checks if the player's input so far matches the pattern.
        Returns True if correct, False otherwise.
        """
        return self.pattern[: len(self.player_input)] == self.player_input

    def _run_game_over(self) -> None:
        """
        Handles the game over screen and transitions to menu or exit.
        """
        self.sounds["LOSE"].play()
        while self.game_state == GameState.GAME_OVER:
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self._quit_game()
                if event.type == pygame.MOUSEBUTTONUP:
                    if self.buttons["AGAIN_GAMEOVER"].collidepoint(event.pos):
                        self.game_state = GameState.MENU
                        return
                    if self.buttons["EXIT_GAMEOVER"].collidepoint(event.pos):
                        self._quit_game()

            self.screen.fill(BLACK)
            lose_text = self.title_font.render("You Lose", True, WHITE)
            score_text = self.title_font.render(f"Score: {self.score}", True, WHITE)
            self.screen.blit(
                lose_text,
                (lose_text.get_rect(center=(self.screen.get_width() / 2, 100))),
            )
            self.screen.blit(
                score_text,
                (score_text.get_rect(center=(self.screen.get_width() / 2, 170))),
            )
            self.screen.blit(
                self.images["again"], self.buttons["AGAIN_GAMEOVER"].topleft
            )
            self.screen.blit(self.images["exit"], self.buttons["EXIT_GAMEOVER"].topleft)
            pygame.display.update()

    def _show_pattern(self) -> None:
        """
        Animates the computer showing the pattern to the player.
        Delay decreases as score increases for added challenge.
        """
        delay = max(100, 500 - 20 * self.score)
        for color in self.pattern:
            self._flash_color(color, delay)
            pygame.time.delay(delay // 2)

    def _flash_color(self, color: str, delay: int = 250) -> None:
        """
        Flashes a color button (light color) and plays its sound.
        Args:
            color: The color to flash (e.g., 'GREEN').
            delay: Duration to show the flash in milliseconds.
        """
        self.sounds[color].play()
        self._draw_game_screen(
            {color: BUTTON_COLORS[color][1]}
        )  # Draw with light color
        pygame.time.delay(delay)
        self._draw_game_screen()  # Draw with normal colors

    def _draw_game_screen(
        self, override_colors: Dict[str, Tuple[int, int, int]] = {}
    ) -> None:
        """
        Draws the main game screen, including color buttons and score.
        Optionally flashes a button by overriding its color.
        Args:
            override_colors: Dict mapping color names to RGB tuples for flashing.
        """
        self.screen.fill(BLACK)
        score_text = self.font.render(f"Score: {self.score}", True, WHITE)
        self.screen.blit(score_text, (450, 50))
        for color, rect in self.buttons.items():
            if color in BUTTON_COLORS:
                # Use override color if provided (for flashing), otherwise use normal dark color
                button_color = override_colors.get(color, BUTTON_COLORS[color][0])
                pygame.draw.rect(self.screen, button_color, rect)
        pygame.display.update()

    def _quit_game(self) -> None:
        """
        Quits the game cleanly.
        """
        pygame.quit()
        sys.exit()


if __name__ == "__main__":
    game = SimonGame()
    game.run()
