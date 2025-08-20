import pygame
import random
import time
import sys
import os
from enum import Enum
from typing import List, Tuple

# --- Constants ---
# Colors
BLACK = (0, 0, 0)
WHITE = (255, 255, 255)
# Each button has a normal and a light color for flashing
BUTTON_COLORS = {
    'GREEN': ((0, 100, 0), (0, 255, 0)),
    'RED': ((100, 0, 0), (255, 0, 0)),
    'YELLOW': ((150, 150, 0), (255, 255, 0)),
    'BLUE': ((0, 0, 100), (0, 0, 255)),
}

class GameState(Enum):
    """Enumeration for the different states of the game."""
    MENU = 1
    PLAYING = 2
    GAME_OVER = 3

class SimonGame:
    """A class to encapsulate the entire Simon game logic and state."""
    def __init__(self, screen_width: int = 600, screen_height: int = 700):
        pygame.init()
        pygame.mixer.init()

        self.screen = pygame.display.set_mode((screen_width, screen_height))
        pygame.display.set_caption("Simon")
        self.clock = pygame.time.Clock()

        self.game_state = GameState.MENU
        self.score = 0
        self.pattern: List[str] = []
        self.player_input: List[str] = []

        self.load_assets()
        self.create_buttons()

    def load_assets(self):
        """Loads all game assets (fonts, images, sounds) using cross-platform paths."""
        base_asset_path = os.path.join("Games", "Easy", "Simon", "Assets")
        font_path = os.path.join(base_asset_path, "Fonts", "vermin_vibes.ttf")
        img_path = os.path.join(base_asset_path, "Images")
        audio_path = os.path.join(base_asset_path, "Audio")

        self.font = pygame.font.Font(font_path, 20)
        self.title_font = pygame.font.Font(font_path, 50)

        icon_img = pygame.image.load(os.path.join(img_path, "simon_logo.png"))
        pygame.display.set_icon(icon_img)

        self.images = {
            'logo': pygame.transform.scale(icon_img, (300, 300)),
            'start': pygame.transform.scale(pygame.image.load(os.path.join(img_path, "start_button.png")), (240, 90)),
            'again': pygame.transform.scale(pygame.image.load(os.path.join(img_path, "again_button.png")), (240, 90)),
            'exit': pygame.transform.scale(pygame.image.load(os.path.join(img_path, "exit_button.png")), (240, 90)),
        }

        self.sounds = {
            'GREEN': pygame.mixer.Sound(os.path.join(audio_path, "green.wav")),
            'RED': pygame.mixer.Sound(os.path.join(audio_path, "red.wav")),
            'YELLOW': pygame.mixer.Sound(os.path.join(audio_path, "yellow.wav")),
            'BLUE': pygame.mixer.Sound(os.path.join(audio_path, "blue.wav")),
            'LOSE': pygame.mixer.Sound(os.path.join(audio_path, "Fail-sound-effect.wav")),
            'MENU': pygame.mixer.Sound(os.path.join(audio_path, "wethands.ogg")),
        }

    def create_buttons(self):
        """Creates Rect objects for all clickable areas."""
        self.buttons = {
            'GREEN': pygame.Rect(50, 150, 250, 250),
            'RED': pygame.Rect(300, 150, 250, 250),
            'YELLOW': pygame.Rect(50, 400, 250, 250),
            'BLUE': pygame.Rect(300, 400, 250, 250),
            'START_MENU': pygame.Rect(180, 530, 240, 90),
            'AGAIN_GAMEOVER': pygame.Rect(180, 300, 240, 90),
            'EXIT_GAMEOVER': pygame.Rect(180, 450, 240, 90),
        }

    def run(self):
        """The main game loop, managed by a state machine."""
        while True:
            if self.game_state == GameState.MENU:
                self.run_menu()
            elif self.game_state == GameState.PLAYING:
                self.run_playing()
            elif self.game_state == GameState.GAME_OVER:
                self.run_game_over()

    def run_menu(self):
        self.sounds['MENU'].play(-1)
        while self.game_state == GameState.MENU:
            for event in pygame.event.get():
                if event.type == pygame.QUIT: self.quit_game()
                if event.type == pygame.MOUSEBUTTONUP and self.buttons['START_MENU'].collidepoint(event.pos):
                    self.sounds['MENU'].stop()
                    self.game_state = GameState.PLAYING
                    return

            self.screen.fill(BLACK)
            self.screen.blit(self.images['logo'], (150, 150))
            self.screen.blit(self.images['start'], (180, 530))
            pygame.display.update()
            self.clock.tick(60)

    def run_playing(self):
        self.score = 0
        self.pattern = []
        self.player_input = []
        turn = "COMPUTER" # Computer shows pattern first

        while self.game_state == GameState.PLAYING:
            self.draw_game_screen()

            if turn == "COMPUTER":
                self.player_input = []
                pygame.time.delay(1000)
                self.pattern.append(random.choice(list(BUTTON_COLORS.keys())))
                self.show_pattern()
                turn = "PLAYER"

            else: # Player's turn
                self.handle_player_turn()
                if len(self.player_input) == len(self.pattern):
                    self.score = len(self.pattern)
                    turn = "COMPUTER"

    def handle_player_turn(self):
        start_time = time.time()
        while time.time() - start_time < 3: # 3 seconds to make a move
            for event in pygame.event.get():
                if event.type == pygame.QUIT: self.quit_game()
                if event.type == pygame.MOUSEBUTTONUP:
                    for color, rect in self.buttons.items():
                        if color in BUTTON_COLORS and rect.collidepoint(event.pos):
                            self.flash_color(color)
                            self.player_input.append(color)
                            if not self.check_player_input():
                                self.game_state = GameState.GAME_OVER
                                return
                            start_time = time.time() # Reset timer after each correct press
                            # If sequence is complete, return to computer's turn
                            if len(self.player_input) == len(self.pattern):
                                return
            # Allows screen to update while waiting for input
            self.draw_game_screen()

        # If the loop finishes due to timeout
        self.game_state = GameState.GAME_OVER

    def check_player_input(self) -> bool:
        return self.pattern[:len(self.player_input)] == self.player_input

    def run_game_over(self):
        self.sounds['LOSE'].play()
        while self.game_state == GameState.GAME_OVER:
            for event in pygame.event.get():
                if event.type == pygame.QUIT: self.quit_game()
                if event.type == pygame.MOUSEBUTTONUP:
                    if self.buttons['AGAIN_GAMEOVER'].collidepoint(event.pos):
                        self.game_state = GameState.MENU
                        return
                    if self.buttons['EXIT_GAMEOVER'].collidepoint(event.pos):
                        self.quit_game()

            self.screen.fill(BLACK)
            lose_text = self.title_font.render("You Lose", True, WHITE)
            score_text = self.title_font.render(f"Score: {self.score}", True, WHITE)
            self.screen.blit(lose_text, (lose_text.get_rect(center=(self.screen.get_width()/2, 100))))
            self.screen.blit(score_text, (score_text.get_rect(center=(self.screen.get_width()/2, 170))))
            self.screen.blit(self.images['again'], self.buttons['AGAIN_GAMEOVER'].topleft)
            self.screen.blit(self.images['exit'], self.buttons['EXIT_GAMEOVER'].topleft)
            pygame.display.update()

    def show_pattern(self):
        delay = max(100, 500 - 20 * self.score)
        for color in self.pattern:
            self.flash_color(color, delay)
            pygame.time.delay(delay // 2)

    def flash_color(self, color: str, delay: int = 250):
        self.sounds[color].play()
        self.draw_game_screen({color: BUTTON_COLORS[color][1]}) # Draw with light color
        pygame.time.delay(delay)
        self.draw_game_screen() # Draw with normal colors

    def draw_game_screen(self, override_colors: dict = {}):
        self.screen.fill(BLACK)
        score_text = self.font.render(f"Score: {self.score}", True, WHITE)
        self.screen.blit(score_text, (450, 50))
        for color, rect in self.buttons.items():
            if color in BUTTON_COLORS:
                # Use override color if provided (for flashing), otherwise use normal dark color
                button_color = override_colors.get(color, BUTTON_COLORS[color][0])
                pygame.draw.rect(self.screen, button_color, rect)
        pygame.display.update()

    def quit_game(self):
        pygame.quit()
        sys.exit()

if __name__ == "__main__":
    game = SimonGame()
    game.run()
