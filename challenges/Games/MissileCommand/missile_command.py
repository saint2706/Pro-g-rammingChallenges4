"""Missile Command clone implemented with pygame.

This module exposes a `main()` entry point that starts a fully playable
Missile Command style arcade game. Features include level progression,
resource management with limited interceptor missiles, difficulty scaling,
and an optional two-player cooperative mode with distinct controls.
"""

from __future__ import annotations

import argparse
import math
import random
import sys
from dataclasses import dataclass
from typing import List, Optional, Sequence, Tuple

import numpy as np
import pygame

# Ensure the mixer is ready before pygame.init() to avoid latency on some SDL
# backends. Mono output keeps the generated sounds simple.
pygame.mixer.pre_init(44100, -16, 1, 512)

SCREEN_WIDTH = 900
SCREEN_HEIGHT = 650
GROUND_HEIGHT = 80

CITY_COUNT = 6
BASE_POSITIONS = (
    (SCREEN_WIDTH * 0.1, SCREEN_HEIGHT - GROUND_HEIGHT),
    (SCREEN_WIDTH * 0.5, SCREEN_HEIGHT - GROUND_HEIGHT),
    (SCREEN_WIDTH * 0.9, SCREEN_HEIGHT - GROUND_HEIGHT),
)

BACKGROUND_COLOR = (9, 8, 26)
GROUND_COLOR = (48, 40, 70)
CITY_COLOR = (235, 226, 197)
DESTROYED_CITY_COLOR = (90, 80, 100)
BASE_COLOR = (120, 200, 255)
BASE_EMPTY_COLOR = (80, 120, 160)
MISSILE_COLOR = (255, 110, 82)
INTERCEPTOR_COLOR = (120, 255, 220)
PLAYER_TWO_CURSOR_COLOR = (255, 200, 64)
TEXT_COLOR = (225, 228, 255)
WARNING_COLOR = (255, 100, 100)


@dataclass
class DifficultySettings:
    """Parameters that scale with level progression."""

    enemy_wave_size: int = 10
    enemy_wave_growth: int = 4
    enemy_speed: float = 90.0
    enemy_speed_growth: float = 14.0
    spawn_delay_range: Tuple[float, float] = (0.4, 1.6)
    spawn_delay_decay: float = 0.04
    interceptor_speed: float = 280.0
    explosion_growth: float = 220.0
    explosion_duration: float = 0.6
    base_ammo: int = 12
    ammo_growth: int = 2
    score_per_missile: int = 25
    survival_bonus: int = 50


class SoundBoard:
    """Procedurally generated sound effects for launches and explosions."""

    def __init__(self) -> None:
        self.available = pygame.mixer.get_init() is not None
        if not self.available:
            return
        self.launch = self._synth_sound(880, 0.16)
        self.explosion = self._synth_sound(220, 0.35, decay=0.996)
        self.warning = self._synth_sound(440, 0.12)

    def _synth_sound(
        self, frequency: float, duration: float, *, decay: float = 0.999
    ) -> pygame.mixer.Sound:
        sample_rate = 44100
        samples = int(sample_rate * duration)
        times = np.linspace(0, duration, samples, endpoint=False)
        envelope = decay ** (np.arange(samples))
        waveform = np.sin(2 * np.pi * frequency * times) * envelope
        audio = np.int16(waveform * 32767)
        return pygame.sndarray.make_sound(audio)

    def play_launch(self) -> None:
        if self.available:
            self.launch.play()

    def play_explosion(self) -> None:
        if self.available:
            self.explosion.play()

    def play_warning(self) -> None:
        if self.available:
            self.warning.play()


@dataclass
class City:
    x: float
    width: float
    height: float
    alive: bool = True

    def rect(self) -> pygame.Rect:
        return pygame.Rect(
            int(self.x - self.width / 2),
            int(SCREEN_HEIGHT - GROUND_HEIGHT - self.height),
            int(self.width),
            int(self.height),
        )

    def draw(self, surface: pygame.Surface) -> None:
        color = CITY_COLOR if self.alive else DESTROYED_CITY_COLOR
        pygame.draw.rect(surface, color, self.rect())


class Base:
    def __init__(self, x: float, y: float) -> None:
        self.x = x
        self.y = y
        self.ammo = 0
        self.capacity = 0
        self.cooldown = 0.0

    def reset_ammo(self, level: int, settings: DifficultySettings) -> None:
        self.capacity = settings.base_ammo + settings.ammo_growth * (level - 1)
        self.ammo = self.capacity
        self.cooldown = 0.0

    def update(self, dt: float) -> None:
        if self.cooldown > 0:
            self.cooldown = max(0.0, self.cooldown - dt)

    def can_fire(self) -> bool:
        return self.ammo > 0 and self.cooldown == 0.0

    def fire(self) -> bool:
        if self.can_fire():
            self.ammo -= 1
            self.cooldown = 0.25
            return True
        return False

    def draw(self, surface: pygame.Surface) -> None:
        size = 36
        rect = pygame.Rect(int(self.x - size / 2), int(self.y - size / 2), size, size)
        color = BASE_COLOR if self.ammo > 0 else BASE_EMPTY_COLOR
        pygame.draw.rect(surface, color, rect, border_radius=6)
        if self.capacity:
            ammo_ratio = self.ammo / self.capacity
            bar_width = size
            bar_height = 8
            bar_rect = pygame.Rect(
                int(self.x - bar_width / 2),
                int(self.y + size / 2 + 6),
                bar_width,
                bar_height,
            )
            pygame.draw.rect(surface, (45, 45, 65), bar_rect)
            fill_rect = pygame.Rect(
                bar_rect.left, bar_rect.top, int(bar_width * ammo_ratio), bar_height
            )
            pygame.draw.rect(surface, (120, 240, 255), fill_rect)


class Missile:
    def __init__(
        self, start: Tuple[float, float], target: Tuple[float, float], speed: float
    ) -> None:
        self.x, self.y = start
        self.target_x, self.target_y = target
        dx = self.target_x - self.x
        dy = self.target_y - self.y
        dist = max(math.hypot(dx, dy), 1e-5)
        self.vx = dx / dist * speed
        self.vy = dy / dist * speed
        self.active = True

    def update(self, dt: float) -> None:
        if not self.active:
            return
        self.x += self.vx * dt
        self.y += self.vy * dt

    def draw(self, surface: pygame.Surface, color: Tuple[int, int, int]) -> None:
        if not self.active:
            return
        pygame.draw.circle(surface, color, (int(self.x), int(self.y)), 3)

    def has_reached_target(self) -> bool:
        if not self.active:
            return False
        reached_x = (self.vx >= 0 and self.x >= self.target_x) or (
            self.vx < 0 and self.x <= self.target_x
        )
        reached_y = (self.vy >= 0 and self.y >= self.target_y) or (
            self.vy < 0 and self.y <= self.target_y
        )
        return reached_x and reached_y


class Explosion:
    def __init__(self, x: float, y: float, settings: DifficultySettings) -> None:
        self.x = x
        self.y = y
        self.radius = 4
        self.age = 0.0
        self.settings = settings
        self.active = True

    @property
    def max_radius(self) -> float:
        return self.settings.explosion_growth * self.settings.explosion_duration / 2

    def update(self, dt: float) -> None:
        if not self.active:
            return
        self.age += dt
        if self.age < self.settings.explosion_duration / 2:
            self.radius += self.settings.explosion_growth * dt
        else:
            self.radius -= self.settings.explosion_growth * dt
        if self.radius <= 0:
            self.active = False

    def draw(self, surface: pygame.Surface) -> None:
        if not self.active:
            return
        pygame.draw.circle(
            surface,
            (240, 200, 120),
            (int(self.x), int(self.y)),
            int(max(2, self.radius)),
            width=2,
        )

    def collides_with(self, missile: Missile) -> bool:
        if not self.active or not missile.active:
            return False
        return math.hypot(missile.x - self.x, missile.y - self.y) <= self.radius


class CooperativeCursor:
    def __init__(self) -> None:
        self.x = SCREEN_WIDTH * 0.5
        self.y = SCREEN_HEIGHT * 0.5
        self.speed = 320.0

    def update(self, keys: Sequence[bool], dt: float) -> None:
        dx = dy = 0.0
        if keys[pygame.K_w] or keys[pygame.K_UP]:
            dy -= 1
        if keys[pygame.K_s] or keys[pygame.K_DOWN]:
            dy += 1
        if keys[pygame.K_a] or keys[pygame.K_LEFT]:
            dx -= 1
        if keys[pygame.K_d] or keys[pygame.K_RIGHT]:
            dx += 1
        if dx or dy:
            norm = math.hypot(dx, dy) or 1.0
            self.x += self.speed * dt * dx / norm
            self.y += self.speed * dt * dy / norm
            self.x = max(0, min(SCREEN_WIDTH, self.x))
            self.y = max(0, min(SCREEN_HEIGHT - GROUND_HEIGHT, self.y))

    def draw(self, surface: pygame.Surface) -> None:
        pygame.draw.circle(
            surface, PLAYER_TWO_CURSOR_COLOR, (int(self.x), int(self.y)), 6, width=1
        )
        pygame.draw.line(
            surface,
            PLAYER_TWO_CURSOR_COLOR,
            (int(self.x) - 10, int(self.y)),
            (int(self.x) + 10, int(self.y)),
        )
        pygame.draw.line(
            surface,
            PLAYER_TWO_CURSOR_COLOR,
            (int(self.x), int(self.y) - 10),
            (int(self.x), int(self.y) + 10),
        )


class MissileCommandGame:
    def __init__(
        self, *, two_player: bool = False, settings: Optional[DifficultySettings] = None
    ) -> None:
        self.settings = settings or DifficultySettings()
        self.screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
        pygame.display.set_caption("Missile Command - Pygame Edition")
        self.clock = pygame.time.Clock()
        self.font = pygame.font.SysFont("consolas", 20)
        self.large_font = pygame.font.SysFont("consolas", 48)
        self.sound = SoundBoard()

        self.cities: List[City] = []
        self.bases: List[Base] = [Base(x, y) for x, y in BASE_POSITIONS]
        self.enemy_missiles: List[Missile] = []
        self.interceptors: List[Missile] = []
        self.explosions: List[Explosion] = []

        self.level = 1
        self.score = 0
        self.score_multiplier = 1.0
        self.two_player_mode = two_player
        self.coop_cursor = CooperativeCursor()

        self.wave_to_spawn = 0
        self.spawned_this_wave = 0
        self.spawn_timer = 0.0
        self.game_over = False
        self.warning_played = False

        self.reset_level()

    def reset_level(self) -> None:
        self.cities = [
            City(x, width=50, height=28)
            for x in np.linspace(100, SCREEN_WIDTH - 100, CITY_COUNT)
        ]
        for base in self.bases:
            base.reset_ammo(self.level, self.settings)
        self.enemy_missiles.clear()
        self.interceptors.clear()
        self.explosions.clear()
        self.score_multiplier = 1.0 + 0.25 * (self.level - 1)
        self.wave_to_spawn = (
            self.settings.enemy_wave_size
            + self.settings.enemy_wave_growth * (self.level - 1)
        )
        self.spawned_this_wave = 0
        self.spawn_timer = 0.0
        self.warning_played = False

    def restart_game(self) -> None:
        self.level = 1
        self.score = 0
        self.game_over = False
        self.reset_level()

    def find_targetable_objects(self) -> List[Tuple[float, float, str]]:
        targets: List[Tuple[float, float, str]] = []
        for city in self.cities:
            if city.alive:
                rect = city.rect()
                targets.append((rect.centerx, rect.bottom, "city"))
        for idx, base in enumerate(self.bases):
            if base.ammo > 0:
                targets.append((base.x, base.y, f"base-{idx}"))
        return targets

    def spawn_enemy_missile(self) -> None:
        targets = self.find_targetable_objects()
        if not targets:
            return
        start_x = random.uniform(50, SCREEN_WIDTH - 50)
        start = (start_x, -20)
        target = random.choice(targets)
        base_speed = self.settings.enemy_speed + self.settings.enemy_speed_growth * (
            self.level - 1
        )
        speed = random.uniform(base_speed * 0.8, base_speed * 1.2)
        missile = Missile(start, (target[0], target[1]), speed)
        self.enemy_missiles.append(missile)
        self.spawned_this_wave += 1

    def handle_enemy_impacts(self) -> None:
        for missile in list(self.enemy_missiles):
            if not missile.active:
                continue
            if missile.has_reached_target():
                missile.active = False
                self.sound.play_explosion()
                self.explosions.append(
                    Explosion(missile.target_x, missile.target_y, self.settings)
                )
                self.damage_target_at((missile.target_x, missile.target_y))

    def damage_target_at(self, position: Tuple[float, float]) -> None:
        x, y = position
        for city in self.cities:
            if city.alive and city.rect().collidepoint(int(x), int(y)):
                city.alive = False
                break
        else:
            for base in self.bases:
                base_rect = pygame.Rect(int(base.x - 20), int(base.y - 20), 40, 40)
                if base_rect.collidepoint(int(x), int(y)):
                    base.ammo = 0
                    break

    def handle_explosions(self) -> None:
        for explosion in self.explosions:
            for missile in self.enemy_missiles:
                if explosion.collides_with(missile):
                    if missile.active:
                        missile.active = False
                        self.sound.play_explosion()
                        self.score += int(
                            self.settings.score_per_missile * self.score_multiplier
                        )
        self.enemy_missiles = [m for m in self.enemy_missiles if m.active]
        self.explosions = [e for e in self.explosions if e.active]

    def fire_interceptor(
        self, target_x: float, target_y: float, *, player_two: bool = False
    ) -> None:
        available_bases = [base for base in self.bases if base.can_fire()]
        if not available_bases:
            if not self.warning_played:
                self.sound.play_warning()
                self.warning_played = True
            return
        if player_two:
            base = self.bases[1]
            if not base.can_fire():
                self.sound.play_warning()
                return
        else:
            base = min(available_bases, key=lambda b: abs(b.x - target_x))
        if base.fire():
            interceptor = Missile(
                (base.x, base.y), (target_x, target_y), self.settings.interceptor_speed
            )
            self.interceptors.append(interceptor)
            self.sound.play_launch()

    def update_interceptors(self, dt: float) -> None:
        for interceptor in list(self.interceptors):
            interceptor.update(dt)
            if interceptor.has_reached_target():
                interceptor.active = False
                self.explosions.append(
                    Explosion(interceptor.target_x, interceptor.target_y, self.settings)
                )
                self.sound.play_explosion()
        self.interceptors = [i for i in self.interceptors if i.active]

    def update_enemy_missiles(self, dt: float) -> None:
        for missile in self.enemy_missiles:
            missile.update(dt)
        self.handle_enemy_impacts()

    def update(self, dt: float) -> None:
        if self.game_over:
            return
        keys = pygame.key.get_pressed()
        for base in self.bases:
            base.update(dt)
        if self.two_player_mode:
            self.coop_cursor.update(keys, dt)
        self.spawn_timer -= dt
        delay_decay = self.settings.spawn_delay_decay * (self.level - 1)
        spawn_min = max(0.05, self.settings.spawn_delay_range[0] * (1 - delay_decay))
        spawn_max = max(
            spawn_min, self.settings.spawn_delay_range[1] * (1 - delay_decay)
        )
        if self.spawned_this_wave < self.wave_to_spawn and self.spawn_timer <= 0:
            self.spawn_enemy_missile()
            self.spawn_timer = random.uniform(spawn_min, spawn_max)
        self.update_enemy_missiles(dt)
        self.update_interceptors(dt)
        for explosion in self.explosions:
            explosion.update(dt)
        self.handle_explosions()
        self.check_level_complete()
        self.check_game_over()

    def check_level_complete(self) -> None:
        missiles_remaining = self.spawned_this_wave < self.wave_to_spawn or any(
            m.active for m in self.enemy_missiles
        )
        if (
            missiles_remaining
            or self.interceptors
            or any(e.active for e in self.explosions)
        ):
            return
        surviving_cities = sum(1 for city in self.cities if city.alive)
        self.score += (
            surviving_cities * self.settings.survival_bonus * int(self.score_multiplier)
        )
        self.level += 1
        self.reset_level()

    def check_game_over(self) -> None:
        if any(city.alive for city in self.cities):
            return
        self.game_over = True

    def draw_ground(self) -> None:
        ground_rect = pygame.Rect(
            0, SCREEN_HEIGHT - GROUND_HEIGHT, SCREEN_WIDTH, GROUND_HEIGHT
        )
        pygame.draw.rect(self.screen, GROUND_COLOR, ground_rect)

    def draw_hud(self) -> None:
        ammo_text = " | ".join(
            f"Base {idx + 1}: {base.ammo}" for idx, base in enumerate(self.bases)
        )
        hud_lines = [
            f"Level {self.level}",
            f"Score {self.score}",
            f"Multiplier x{self.score_multiplier:.2f}",
            f"Missiles {ammo_text}",
            "2P Mode ON" if self.two_player_mode else "Press F2 for 2P Mode",
        ]
        for idx, line in enumerate(hud_lines):
            surface = self.font.render(line, True, TEXT_COLOR)
            self.screen.blit(surface, (16, 16 + 24 * idx))
        if all(base.ammo == 0 for base in self.bases):
            warning_surface = self.font.render("OUT OF MISSILES", True, WARNING_COLOR)
            self.screen.blit(warning_surface, (SCREEN_WIDTH - 220, 20))

    def draw(self) -> None:
        self.screen.fill(BACKGROUND_COLOR)
        self.draw_ground()
        for city in self.cities:
            city.draw(self.screen)
        for base in self.bases:
            base.draw(self.screen)
        for missile in self.enemy_missiles:
            missile.draw(self.screen, MISSILE_COLOR)
        for interceptor in self.interceptors:
            interceptor.draw(self.screen, INTERCEPTOR_COLOR)
        for explosion in self.explosions:
            explosion.draw(self.screen)
        if self.two_player_mode:
            self.coop_cursor.draw(self.screen)
        self.draw_hud()
        if self.game_over:
            overlay = pygame.Surface((SCREEN_WIDTH, SCREEN_HEIGHT), pygame.SRCALPHA)
            overlay.fill((0, 0, 0, 120))
            self.screen.blit(overlay, (0, 0))
            text = self.large_font.render("Game Over", True, WARNING_COLOR)
            info = self.font.render("Press R to restart", True, TEXT_COLOR)
            self.screen.blit(
                text, (SCREEN_WIDTH / 2 - text.get_width() / 2, SCREEN_HEIGHT / 2 - 60)
            )
            self.screen.blit(
                info, (SCREEN_WIDTH / 2 - info.get_width() / 2, SCREEN_HEIGHT / 2)
            )
        pygame.display.flip()

    def toggle_two_player(self) -> None:
        self.two_player_mode = not self.two_player_mode

    def process_events(self) -> None:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                pygame.quit()
                sys.exit()
            if event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    pygame.quit()
                    sys.exit()
                if event.key == pygame.K_r and self.game_over:
                    self.restart_game()
                if event.key == pygame.K_F2:
                    self.toggle_two_player()
                if (
                    self.two_player_mode
                    and event.key == pygame.K_SPACE
                    and not self.game_over
                ):
                    self.fire_interceptor(
                        self.coop_cursor.x, self.coop_cursor.y, player_two=True
                    )
            if event.type == pygame.MOUSEBUTTONDOWN and not self.game_over:
                if event.button == 1:
                    mx, my = pygame.mouse.get_pos()
                    self.fire_interceptor(mx, my)

    def run(self) -> None:
        while True:
            dt = self.clock.tick(60) / 1000.0
            self.process_events()
            self.update(dt)
            self.draw()


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = argparse.ArgumentParser(description="Play Missile Command using pygame.")
    parser.add_argument(
        "--two-player",
        action="store_true",
        help="Start in cooperative mode with a keyboard-guided cursor.",
    )
    args = parser.parse_args(argv)

    pygame.init()
    game = MissileCommandGame(two_player=args.two_player)
    game.run()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
