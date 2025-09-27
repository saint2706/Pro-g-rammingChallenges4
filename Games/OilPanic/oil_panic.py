"""Oil Panic clone implemented with pygame.

This module recreates the dual-screen gameplay of the classic Game & Watch
"Oil Panic" unit. The top screen tasks the player with catching dripping oil
in a three-slot bucket while the bottom screen challenges players to pour the
caught oil into a storage tower without spilling it onto the patrolling
policeman below. Difficulty settings change drip cadence and guard speed, and
high scores persist per difficulty.
"""
from __future__ import annotations

import json
import math
import random
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Tuple

import pygame

BASE_RESOLUTION = (256, 352)  # (width, height) for two stacked screens
TOP_SCREEN_HEIGHT = 176
BOTTOM_SCREEN_HEIGHT = BASE_RESOLUTION[1] - TOP_SCREEN_HEIGHT
CONFIG_PATH = Path(__file__).with_name("config.json")
HIGHSCORE_PATH = Path(__file__).with_name("high_scores.json")


@dataclass(frozen=True)
class DifficultySettings:
    """Configuration bundle controlling spawn cadence and speeds."""

    name: str
    drop_interval: float
    drop_speed: float
    policeman_speed: float


DIFFICULTIES: Dict[str, DifficultySettings] = {
    "Easy": DifficultySettings("Easy", drop_interval=1.6, drop_speed=55.0, policeman_speed=28.0),
    "Normal": DifficultySettings("Normal", drop_interval=1.2, drop_speed=70.0, policeman_speed=35.0),
    "Hard": DifficultySettings("Hard", drop_interval=0.85, drop_speed=90.0, policeman_speed=46.0),
}


class OilDrop:
    """Falling oil drop tracked by column index and vertical position."""

    def __init__(self, x: float, y: float, speed: float) -> None:
        self.x = x
        self.y = y
        self.speed = speed

    def update(self, dt: float) -> None:
        self.y += self.speed * dt


class Policeman:
    """Simple patrol sprite that sweeps left and right below the tower."""

    def __init__(self, bounds: Tuple[int, int], speed: float) -> None:
        self.left, self.right = bounds
        self.speed = speed
        self.x = float(self.left)
        self.direction = 1

    def update(self, dt: float) -> None:
        self.x += self.speed * dt * self.direction
        if self.x <= self.left:
            self.x = float(self.left)
            self.direction = 1
        elif self.x >= self.right:
            self.x = float(self.right)
            self.direction = -1


def load_config() -> Tuple[int, str]:
    """Load scaling and difficulty defaults from ``config.json``."""

    if not CONFIG_PATH.exists():
        CONFIG_PATH.write_text(json.dumps({"window_scale": 2, "difficulty": "Normal"}, indent=2))
    try:
        data = json.loads(CONFIG_PATH.read_text())
    except json.JSONDecodeError:
        data = {"window_scale": 2, "difficulty": "Normal"}
    scale = int(data.get("window_scale", 2))
    difficulty_name = str(data.get("difficulty", "Normal"))
    return max(1, min(scale, 5)), difficulty_name if difficulty_name in DIFFICULTIES else "Normal"


def load_high_scores() -> Dict[str, int]:
    """Read high scores from disk, creating the file on first run."""

    if not HIGHSCORE_PATH.exists():
        HIGHSCORE_PATH.write_text(json.dumps({k: 0 for k in DIFFICULTIES}, indent=2))
    try:
        data = json.loads(HIGHSCORE_PATH.read_text())
    except json.JSONDecodeError:
        data = {k: 0 for k in DIFFICULTIES}
    for name in DIFFICULTIES:
        data.setdefault(name, 0)
    return {name: int(score) for name, score in data.items() if name in DIFFICULTIES}


def save_high_scores(scores: Dict[str, int]) -> None:
    HIGHSCORE_PATH.write_text(json.dumps(scores, indent=2))


def make_tone(frequency: float, duration: float = 0.18, volume: float = 0.45) -> pygame.mixer.Sound:
    """Create a short sine-wave tone for feedback cues."""

    sample_rate = 22050
    amplitude = int(32767 * volume)
    length = int(sample_rate * duration)
    samples = bytearray()
    for i in range(length):
        t = i / sample_rate
        value = int(amplitude * math.sin(2 * math.pi * frequency * t))
        samples += value.to_bytes(2, byteorder="little", signed=True)
    return pygame.mixer.Sound(buffer=samples)


class OilPanicGame:
    """Main game controller handling state transitions, drawing, and input."""

    bucket_positions = [48, 128, 208]

    def __init__(self, scale: int, difficulty_name: str) -> None:
        pygame.init()
        self.scale = scale
        self.window = pygame.display.set_mode(
            (BASE_RESOLUTION[0] * scale, BASE_RESOLUTION[1] * scale)
        )
        pygame.display.set_caption("Oil Panic - Game & Watch tribute")
        self.base_surface = pygame.Surface(BASE_RESOLUTION)
        self.clock = pygame.time.Clock()
        self.font = pygame.font.SysFont("fira mono", 14)
        self.big_font = pygame.font.SysFont("fira mono", 20, bold=True)
        self.move_cooldown = 0.0
        self.high_scores = load_high_scores()
        self.difficulty = DIFFICULTIES.get(difficulty_name, DIFFICULTIES["Normal"])
        self.sound_enabled = False
        self.sounds: Dict[str, pygame.mixer.Sound] = {}
        self._init_mixer()
        self.reset()

    def _init_mixer(self) -> None:
        try:
            pygame.mixer.init(frequency=22050, size=-16, channels=1)
        except pygame.error:
            self.sound_enabled = False
        else:
            self.sound_enabled = True
            self.sounds = {
                "catch": make_tone(880),
                "pour": make_tone(660, duration=0.22),
                "spill": make_tone(220, duration=0.25, volume=0.6),
                "switch": make_tone(440, duration=0.12),
            }

    def play_sound(self, name: str) -> None:
        if self.sound_enabled and name in self.sounds:
            self.sounds[name].play()

    def set_difficulty(self, name: str) -> None:
        self.difficulty = DIFFICULTIES[name]
        self.reset()

    def reset(self) -> None:
        self.active_screen = "top"
        self.bucket_index = 1
        self.bucket_contents = 0
        self.bucket_capacity = 3
        self.spawn_timer = 0.0
        self.drops: List[OilDrop] = []
        self.score = 0
        self.time_limit = 90.0
        self.time_left = self.time_limit
        self.lives = 3
        self.spills = 0
        self.game_over = False
        self.message = ""
        self.policeman = Policeman(bounds=(28, BASE_RESOLUTION[0] - 40), speed=self.difficulty.policeman_speed)
        self.deposit_zone = (BASE_RESOLUTION[0] // 2 - 18, BASE_RESOLUTION[0] // 2 + 18)
        self.move_cooldown = 0.0
        self.play_sound("switch")

    def spawn_drop(self) -> None:
        column = random.choice(self.bucket_positions)
        # Nudge x slightly for variety
        x = column + random.uniform(-6, 6)
        self.drops.append(OilDrop(x=x, y=20.0, speed=self.difficulty.drop_speed))

    def handle_input(self, dt: float) -> None:
        if self.game_over:
            return
        keys = pygame.key.get_pressed()
        self.move_cooldown = max(0.0, self.move_cooldown - dt)
        if self.move_cooldown <= 0.0:
            if keys[pygame.K_LEFT]:
                if self.bucket_index > 0:
                    self.bucket_index -= 1
                    self.move_cooldown = 0.12
            elif keys[pygame.K_RIGHT]:
                if self.bucket_index < len(self.bucket_positions) - 1:
                    self.bucket_index += 1
                    self.move_cooldown = 0.12
        if self.active_screen == "bottom" and keys[pygame.K_SPACE] and self.bucket_contents > 0:
            self.pour_oil()

    def pour_oil(self) -> None:
        policeman_in_zone = self.deposit_zone[0] <= self.policeman.x <= self.deposit_zone[1]
        if policeman_in_zone:
            self.register_spill()
            self.bucket_contents = max(0, self.bucket_contents - 1)
        else:
            self.bucket_contents -= 1
            score_gain = 25 + int(self.time_left)
            self.score += score_gain
            self.message = f"Deposited! +{score_gain}"
            self.play_sound("pour")
        if self.bucket_contents <= 0:
            self.bucket_contents = 0
            self.message = "Bucket empty. Press Up to return."

    def register_spill(self) -> None:
        self.spills += 1
        self.lives -= 1
        self.message = "Spill!" if self.lives > 0 else "Spill! Game Over"
        self.play_sound("spill")
        if self.lives <= 0:
            self.trigger_game_over()

    def trigger_game_over(self) -> None:
        if self.game_over:
            return
        self.game_over = True
        best = self.high_scores.get(self.difficulty.name, 0)
        if self.score > best:
            self.high_scores[self.difficulty.name] = self.score
            save_high_scores(self.high_scores)
            self.message = "New high score! Press Enter to restart."
        else:
            self.message = "Press Enter to restart."

    def update(self, dt: float) -> None:
        if self.game_over:
            return
        self.time_left = max(0.0, self.time_left - dt)
        if self.time_left <= 0:
            self.trigger_game_over()
            return
        self.spawn_timer -= dt
        if self.spawn_timer <= 0:
            self.spawn_drop()
            jitter = random.uniform(-0.25, 0.25)
            self.spawn_timer = max(0.35, self.difficulty.drop_interval + jitter)
        if self.active_screen == "top":
            self.update_top_screen(dt)
        else:
            self.update_bottom_screen(dt)

    def update_top_screen(self, dt: float) -> None:
        bucket_x = self.bucket_positions[self.bucket_index]
        catch_y = TOP_SCREEN_HEIGHT - 28
        caught: List[OilDrop] = []
        for drop in self.drops:
            drop.update(dt)
            if drop.y >= catch_y and abs(drop.x - bucket_x) <= 14:
                if self.bucket_contents < self.bucket_capacity:
                    self.bucket_contents += 1
                    caught.append(drop)
                    self.message = "" if self.bucket_contents < self.bucket_capacity else "Bucket full!"
                    self.play_sound("catch")
                else:
                    # overflow triggers spill
                    self.register_spill()
                    caught.append(drop)
            elif drop.y >= TOP_SCREEN_HEIGHT - 8:
                caught.append(drop)
                self.register_spill()
        for drop in caught:
            if drop in self.drops:
                self.drops.remove(drop)

    def update_bottom_screen(self, dt: float) -> None:
        self.policeman.update(dt)

    def draw(self) -> None:
        self.base_surface.fill((22, 22, 30))
        self.draw_top_screen()
        self.draw_bottom_screen()
        self.draw_hud()
        scaled = pygame.transform.scale(self.base_surface, self.window.get_size())
        self.window.blit(scaled, (0, 0))
        pygame.display.flip()

    def draw_top_screen(self) -> None:
        top_rect = pygame.Rect(0, 0, BASE_RESOLUTION[0], TOP_SCREEN_HEIGHT)
        pygame.draw.rect(self.base_surface, (36, 46, 70), top_rect)
        pygame.draw.rect(
            self.base_surface, (18, 28, 45), (0, TOP_SCREEN_HEIGHT - 10, BASE_RESOLUTION[0], 10)
        )
        # Draw pipes and windows reminiscent of the LCD backdrop
        for idx, x in enumerate(self.bucket_positions):
            pygame.draw.rect(self.base_surface, (80, 90, 120), (x - 10, 32, 20, 24), border_radius=6)
            pygame.draw.rect(self.base_surface, (140, 160, 200), (x - 8, 34, 16, 12), border_radius=4)
            pygame.draw.rect(self.base_surface, (40, 50, 80), (x - 4, 46, 8, 40))
        # Draw oil drops
        for drop in self.drops:
            pygame.draw.circle(self.base_surface, (24, 220, 180), (int(drop.x), int(drop.y)), 4)
            pygame.draw.circle(self.base_surface, (4, 120, 90), (int(drop.x) - 1, int(drop.y) - 1), 4, 1)
        # Draw bucket handler (two-armed worker)
        bucket_x = self.bucket_positions[self.bucket_index]
        base_y = TOP_SCREEN_HEIGHT - 12
        pygame.draw.rect(self.base_surface, (90, 90, 100), (bucket_x - 16, base_y - 30, 32, 26), border_radius=8)
        pygame.draw.circle(self.base_surface, (230, 230, 210), (bucket_x, base_y - 34), 10)
        pygame.draw.rect(self.base_surface, (60, 60, 90), (bucket_x - 22, base_y - 12, 44, 12))
        # Bucket
        fill_ratio = self.bucket_contents / self.bucket_capacity
        bucket_color = (200, 200, 50) if fill_ratio < 1 else (230, 80, 60)
        pygame.draw.rect(self.base_surface, (20, 20, 20), (bucket_x - 18, base_y - 6, 36, 10), width=2)
        pygame.draw.rect(
            self.base_surface,
            bucket_color,
            (
                bucket_x - 16,
                base_y - 4 - int(6 * fill_ratio),
                32,
                int(8 * fill_ratio) + 2,
            ),
        )

    def draw_bottom_screen(self) -> None:
        offset_y = TOP_SCREEN_HEIGHT
        bottom_rect = pygame.Rect(0, offset_y, BASE_RESOLUTION[0], BOTTOM_SCREEN_HEIGHT)
        pygame.draw.rect(self.base_surface, (30, 40, 60), bottom_rect)
        pygame.draw.rect(
            self.base_surface,
            (45, 55, 85),
            (0, offset_y + BOTTOM_SCREEN_HEIGHT - 18, BASE_RESOLUTION[0], 18),
        )
        # Storage tower
        tower_rect = pygame.Rect(BASE_RESOLUTION[0] // 2 - 24, offset_y + 32, 48, 96)
        pygame.draw.rect(self.base_surface, (120, 110, 80), tower_rect)
        pygame.draw.rect(self.base_surface, (90, 70, 40), tower_rect, width=4)
        pygame.draw.rect(
            self.base_surface,
            (200, 190, 120),
            (
                tower_rect.x + 8,
                tower_rect.y + tower_rect.height - 10 - min(60, self.score // 10),
                tower_rect.width - 16,
                min(60, self.score // 10),
            ),
        )
        # Pour guide
        pygame.draw.rect(
            self.base_surface,
            (200, 200, 230) if self.active_screen == "bottom" else (100, 110, 130),
            (
                tower_rect.centerx - 12,
                offset_y + 14,
                24,
                24,
            ),
            border_radius=6,
        )
        # Policeman sprite
        pygame.draw.rect(
            self.base_surface,
            (40, 100, 200),
            (
                int(self.policeman.x) - 12,
                offset_y + BOTTOM_SCREEN_HEIGHT - 36,
                24,
                28,
            ),
            border_radius=6,
        )
        pygame.draw.circle(
            self.base_surface,
            (230, 230, 210),
            (int(self.policeman.x), offset_y + BOTTOM_SCREEN_HEIGHT - 40),
            8,
        )
        # Indicate danger zone
        pygame.draw.rect(
            self.base_surface,
            (255, 120, 120),
            (
                self.deposit_zone[0],
                offset_y + BOTTOM_SCREEN_HEIGHT - 24,
                self.deposit_zone[1] - self.deposit_zone[0],
                4,
            ),
        )
        # Bucket preview when the player is downstairs
        if self.active_screen == "bottom":
            bucket_x = self.bucket_positions[self.bucket_index]
            base_y = offset_y + 28
            pygame.draw.rect(self.base_surface, (90, 90, 100), (bucket_x - 16, base_y - 30, 32, 26), border_radius=8)
            pygame.draw.circle(self.base_surface, (230, 230, 210), (bucket_x, base_y - 34), 10)
            fill_ratio = self.bucket_contents / self.bucket_capacity if self.bucket_capacity else 0
            bucket_color = (200, 200, 50) if fill_ratio < 1 else (230, 80, 60)
            pygame.draw.rect(self.base_surface, (20, 20, 20), (bucket_x - 18, base_y - 6, 36, 10), width=2)
            pygame.draw.rect(
                self.base_surface,
                bucket_color,
                (
                    bucket_x - 16,
                    base_y - 4 - int(6 * fill_ratio),
                    32,
                    int(8 * fill_ratio) + 2,
                ),
            )

    def draw_hud(self) -> None:
        info_lines = [
            f"Difficulty: {self.difficulty.name}",
            f"Score: {self.score}",
            f"High: {self.high_scores.get(self.difficulty.name, 0)}",
            f"Time: {int(self.time_left)}s",
            f"Bucket: {self.bucket_contents}/{self.bucket_capacity}",
            f"Spills: {self.spills}",
            f"Lives: {'❤' * self.lives}{'·' * (3 - self.lives)}",
        ]
        for idx, text in enumerate(info_lines):
            surface = self.font.render(text, True, (230, 230, 230))
            self.base_surface.blit(surface, (8, TOP_SCREEN_HEIGHT + 132 + idx * 16))
        instructions = [
            "Left/Right: Move",
            "Down: Go downstairs",
            "Up: Go upstairs",
            "Space: Pour (bottom)",
            "1/2/3: Easy/Normal/Hard",
            "Enter: Restart",
            "Esc: Quit",
        ]
        for idx, text in enumerate(instructions):
            surface = self.font.render(text, True, (200, 200, 210))
            self.base_surface.blit(surface, (BASE_RESOLUTION[0] - 150, TOP_SCREEN_HEIGHT + 96 + idx * 16))
        if self.message:
            surface = self.big_font.render(self.message, True, (255, 230, 90))
            rect = surface.get_rect(center=(BASE_RESOLUTION[0] // 2, 24))
            self.base_surface.blit(surface, rect)
        if self.game_over:
            overlay = pygame.Surface(BASE_RESOLUTION, pygame.SRCALPHA)
            overlay.fill((0, 0, 0, 140))
            self.base_surface.blit(overlay, (0, 0))
            text = self.big_font.render("Game Over", True, (250, 220, 120))
            self.base_surface.blit(text, text.get_rect(center=(BASE_RESOLUTION[0] // 2, BASE_RESOLUTION[1] // 2 - 20)))
            prompt = self.font.render("Press Enter to play again", True, (255, 255, 255))
            self.base_surface.blit(prompt, prompt.get_rect(center=(BASE_RESOLUTION[0] // 2, BASE_RESOLUTION[1] // 2 + 12)))

    def handle_events(self) -> bool:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    return False
                if event.key == pygame.K_RETURN and self.game_over:
                    self.reset()
                if event.key == pygame.K_DOWN and not self.game_over and self.active_screen == "top":
                    self.active_screen = "bottom"
                    self.message = "Pour carefully!"
                    self.play_sound("switch")
                if event.key == pygame.K_UP and not self.game_over and self.active_screen == "bottom":
                    self.active_screen = "top"
                    self.message = "Back upstairs."
                    self.play_sound("switch")
                if event.key == pygame.K_1:
                    self.set_difficulty("Easy")
                if event.key == pygame.K_2:
                    self.set_difficulty("Normal")
                if event.key == pygame.K_3:
                    self.set_difficulty("Hard")
        return True

    def run(self) -> None:
        running = True
        while running:
            dt = self.clock.tick(60) / 1000.0
            running = self.handle_events()
            self.handle_input(dt)
            self.update(dt)
            self.draw()
        pygame.quit()


def main() -> None:
    scale, default_difficulty = load_config()
    game = OilPanicGame(scale=scale, difficulty_name=default_difficulty)
    game.run()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        pygame.quit()
        sys.exit(0)
