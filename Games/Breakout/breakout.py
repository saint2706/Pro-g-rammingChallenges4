"""Breakout clone featuring multi-level progression and power-ups.

This build targets the `/g/` programming challenges list and focuses on:
- JSON-defined levels with distinct brick layouts and tuning knobs.
- Responsive paddle + ball physics that reflect launch angle control.
- Power-ups (enlarge paddle, extra life, multiball) with particle/auditory feedback.
- Scoring, lives, and difficulty escalation across the included stages.

Run the module directly to start the game:
    python breakout.py

Controls:
- Left / Right arrows (or A / D) move the paddle.
- Space launches a held ball after losing a life or starting a level.
- Escape closes the window.
"""

from __future__ import annotations

import json
import math
import random
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import pygame

# Window configuration -----------------------------------------------------
WIDTH = 800
HEIGHT = 720
BACKGROUND = (10, 10, 26)
TEXT_COLOUR = (236, 240, 241)
FPS = 120

LEVEL_PATH = Path(__file__).with_suffix("").parent / "levels"


@dataclass
class Particle:
    """Simple fading particle for brick impacts and power-ups."""

    position: pygame.Vector2
    velocity: pygame.Vector2
    colour: Tuple[int, int, int]
    radius: float
    lifetime: float

    age: float = 0.0

    def update(self, dt: float) -> bool:
        self.age += dt
        if self.age >= self.lifetime:
            return False
        self.position += self.velocity * dt
        self.radius = max(0.0, self.radius - dt * 40.0)
        return True

    def draw(self, surface: pygame.Surface) -> None:
        alpha = max(0, min(255, int(255 * (1 - self.age / self.lifetime))))
        if alpha <= 0 or self.radius <= 0:
            return
        size = max(2, int(self.radius * 2))
        radius_px = max(1, int(self.radius))
        gfx_surface = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.circle(
            gfx_surface,
            (*self.colour, alpha),
            (radius_px, radius_px),
            radius_px,
        )
        surface.blit(gfx_surface, (self.position.x - radius_px, self.position.y - radius_px))


class Paddle:
    """Player paddle managing movement and temporary size boosts."""

    def __init__(self, width: float, height: float, speed: float) -> None:
        self.base_width = width
        self.height = height
        self.speed = speed
        self.rect = pygame.Rect(0, 0, int(width), int(height))
        self.rect.midbottom = (WIDTH // 2, HEIGHT - 40)
        self.enlarge_timer = 0.0

    def update(self, dt: float, keys: Iterable[bool]) -> None:
        direction = 0
        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            direction -= 1
        if keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            direction += 1

        self.rect.x += int(direction * self.speed * dt)
        self.rect.x = max(20, min(WIDTH - self.rect.width - 20, self.rect.x))

        if self.enlarge_timer > 0:
            self.enlarge_timer = max(0.0, self.enlarge_timer - dt)
            if self.enlarge_timer == 0:
                self.set_width(self.base_width)

    def set_width(self, width: float, duration: float = 0.0) -> None:
        centre = self.rect.centerx
        self.rect.width = int(width)
        self.rect.centerx = centre
        self.enlarge_timer = duration

    def draw(self, surface: pygame.Surface) -> None:
        pygame.draw.rect(surface, (52, 152, 219), self.rect, border_radius=8)
        accent_rect = self.rect.inflate(-self.rect.width * 0.3, -self.rect.height * 0.4)
        pygame.draw.rect(surface, (236, 240, 241), accent_rect, border_radius=6)


class Ball:
    """Breakout ball supporting sticky launches and vector movement."""

    def __init__(self, speed: float, radius: int = 10) -> None:
        self.radius = radius
        self.speed = speed
        self.position = pygame.Vector2(WIDTH / 2, HEIGHT / 2)
        self.velocity = pygame.Vector2(0, -speed)
        self.rect = pygame.Rect(0, 0, radius * 2, radius * 2)
        self.sticky = True

    def attach_to(self, paddle: Paddle) -> None:
        self.position.update(paddle.rect.centerx, paddle.rect.top - self.radius - 2)
        self.rect.center = (int(self.position.x), int(self.position.y))
        self.velocity.update(0, -self.speed)
        self.sticky = True

    def launch(self) -> None:
        if not self.sticky:
            return
        angle = random.uniform(-0.35, 0.35)
        self.velocity = pygame.Vector2(0, -self.speed).rotate_rad(angle)
        self.sticky = False

    def update(self, dt: float) -> None:
        if self.sticky:
            return
        self.position += self.velocity * dt
        self.rect.center = (int(self.position.x), int(self.position.y))

    def draw(self, surface: pygame.Surface) -> None:
        pygame.draw.circle(surface, (241, 196, 15), (int(self.position.x), int(self.position.y)), self.radius)
        pygame.draw.circle(surface, (255, 255, 255), (int(self.position.x - 2), int(self.position.y - 2)), self.radius - 4)


class Brick(pygame.sprite.Sprite):
    """Single brick with hit points, score value, and optional power-up."""

    def __init__(
        self,
        rect: pygame.Rect,
        hits: int,
        score: int,
        powerup: Optional[str] = None,
    ) -> None:
        super().__init__()
        self.rect = rect
        self.hits = hits
        self.score = score
        self.powerup = powerup
        self.image = pygame.Surface((rect.width, rect.height))
        self._render()

    def _render(self) -> None:
        colour = self._colour_for_state()
        self.image.fill(colour)
        pygame.draw.rect(self.image, (236, 240, 241), self.image.get_rect(), 2, border_radius=4)

    def _colour_for_state(self) -> Tuple[int, int, int]:
        palette = {
            1: (46, 204, 113),
            2: (230, 126, 34),
            3: (231, 76, 60),
            4: (155, 89, 182),
        }
        base = palette.get(self.hits, (52, 73, 94))
        if self.powerup:
            return tuple(min(255, int(c * 1.2)) for c in base)
        return base

    def hit(self) -> Tuple[bool, int]:
        self.hits -= 1
        destroyed = self.hits <= 0
        if destroyed:
            return True, self.score
        self._render()
        return False, 0


@dataclass
class PowerUp:
    """Falling power-up pill that the paddle can collect."""

    rect: pygame.Rect
    power_type: str
    velocity: float = 180.0

    def update(self, dt: float) -> bool:
        self.rect.y += int(self.velocity * dt)
        return self.rect.top <= HEIGHT

    def draw(self, surface: pygame.Surface) -> None:
        colours = {
            "enlarge": (142, 68, 173),
            "life": (231, 76, 60),
            "multiball": (52, 152, 219),
        }
        colour = colours.get(self.power_type, (149, 165, 166))
        pygame.draw.ellipse(surface, colour, self.rect)
        pygame.draw.ellipse(surface, (236, 240, 241), self.rect.inflate(-8, -8))


class BreakoutGame:
    """Encapsulates the game loop, level loading, and audiovisual effects."""

    def __init__(self) -> None:
        pygame.init()
        self.screen = pygame.display.set_mode((WIDTH, HEIGHT))
        pygame.display.set_caption("Breakout Challenge")
        self.clock = pygame.time.Clock()
        self.font = pygame.font.SysFont("consolas", 24)
        self.big_font = pygame.font.SysFont("consolas", 48)

        self.sound_enabled = True
        try:
            pygame.mixer.init(frequency=44100, size=-16, channels=1)
        except pygame.error:
            self.sound_enabled = False
        self.sounds = self._build_sounds() if self.sound_enabled else {}

        self.levels = self._load_levels()
        if not self.levels:
            raise SystemExit("No level files found. Ensure JSON layouts exist in levels/.")

        self.current_level_index = 0
        self.lives = 3
        self.score = 0
        self.particles: List[Particle] = []
        self.powerups: List[PowerUp] = []
        self.bricks = pygame.sprite.Group()
        self.balls: List[Ball] = []
        self.paddle: Optional[Paddle] = None
        self._start_level()
        self.running = True
        self.level_complete_timer = 0.0

    def _build_sounds(self) -> Dict[str, pygame.mixer.Sound]:
        def tone(freq: float, duration: float = 0.2, volume: float = 0.3) -> pygame.mixer.Sound:
            sample_rate = 44100
            count = int(sample_rate * duration)
            amplitude = int(32767 * volume)
            buffer = bytearray()
            for i in range(count):
                sample = int(amplitude * math.sin(2 * math.pi * freq * (i / sample_rate)))
                buffer.extend(sample.to_bytes(2, byteorder="little", signed=True))
            return pygame.mixer.Sound(buffer=bytes(buffer))

        return {
            "bounce": tone(220, 0.1, 0.35),
            "brick": tone(523, 0.12, 0.4),
            "power": tone(880, 0.18, 0.4),
            "lose": tone(130, 0.25, 0.5),
        }

    def _load_levels(self) -> List[Dict[str, object]]:
        level_files = sorted(LEVEL_PATH.glob("*.json"))
        levels: List[Dict[str, object]] = []
        for path in level_files:
            with path.open("r", encoding="utf-8") as handle:
                data = json.load(handle)
                data["path"] = path
                levels.append(data)
        return levels

    # ------------------------------------------------------------------
    # Level + entity management
    # ------------------------------------------------------------------
    def _start_level(self) -> None:
        data = self.levels[self.current_level_index]
        brick_w, brick_h = data.get("brick_size", [64, 24])
        self.paddle = Paddle(brick_w * 2, 20, float(data.get("paddle_speed", 420)))
        self.bricks.empty()
        self.powerups.clear()
        self.particles.clear()
        self.balls = [Ball(float(data.get("ball_speed", 340)))]
        self.balls[0].attach_to(self.paddle)
        self._build_bricks(data["layout"], data["legend"], brick_w, brick_h)
        self.level_complete_timer = 1.0

    def _build_bricks(
        self,
        layout: Iterable[str],
        legend: Dict[str, Dict[str, object]],
        brick_w: int,
        brick_h: int,
    ) -> None:
        top_offset = 100
        left_offset = (WIDTH - brick_w * len(layout[0])) // 2
        for row_idx, row in enumerate(layout):
            for col_idx, symbol in enumerate(row):
                spec = legend.get(symbol)
                if not spec or spec.get("empty"):
                    continue
                hits = int(spec.get("hits", 1))
                score = int(spec.get("score", 50))
                powerup = spec.get("powerup")
                rect = pygame.Rect(
                    left_offset + col_idx * brick_w,
                    top_offset + row_idx * brick_h,
                    brick_w - 2,
                    brick_h - 2,
                )
                brick = Brick(rect, hits, score, str(powerup) if powerup else None)
                self.bricks.add(brick)

    # ------------------------------------------------------------------
    # Game loop
    # ------------------------------------------------------------------
    def run(self) -> None:
        while self.running:
            dt = self.clock.tick(FPS) / 1000.0
            self._handle_events()
            self._update(dt)
            self._draw()
        pygame.quit()

    def _handle_events(self) -> None:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                self.running = False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    self.running = False
                elif event.key == pygame.K_SPACE:
                    for ball in self.balls:
                        ball.launch()

    def _update(self, dt: float) -> None:
        if not self.paddle:
            return

        keys = pygame.key.get_pressed()
        self.paddle.update(dt, keys)

        # Attach sticky balls to paddle
        for ball in self.balls:
            if ball.sticky:
                ball.attach_to(self.paddle)

        for ball in list(self.balls):
            ball.update(dt)
            self._wall_collisions(ball)
            self._paddle_collision(ball)
            self._brick_collision(ball)

        self._update_powerups(dt)
        self._update_particles(dt)
        self._check_ball_losses()
        self._check_level_complete(dt)

    # ------------------------------------------------------------------
    # Collision + physics helpers
    # ------------------------------------------------------------------
    def _wall_collisions(self, ball: Ball) -> None:
        bounced = False
        if ball.position.x - ball.radius <= 0:
            ball.position.x = ball.radius + 1
            ball.velocity.x *= -1
            bounced = True
        elif ball.position.x + ball.radius >= WIDTH:
            ball.position.x = WIDTH - ball.radius - 1
            ball.velocity.x *= -1
            bounced = True

        if ball.position.y - ball.radius <= 0:
            ball.position.y = ball.radius + 1
            ball.velocity.y *= -1
            bounced = True

        if bounced:
            ball.rect.center = (int(ball.position.x), int(ball.position.y))
            self._play_sound("bounce")

    def _paddle_collision(self, ball: Ball) -> None:
        if ball.sticky or not self.paddle:
            return
        if ball.velocity.y > 0 and ball.rect.colliderect(self.paddle.rect):
            overlap = ball.rect.clip(self.paddle.rect)
            ball.position.y -= overlap.height
            rel = (ball.position.x - self.paddle.rect.centerx) / (self.paddle.rect.width / 2)
            rel = max(-1.0, min(1.0, rel))
            angle = rel * 60  # degrees
            speed = ball.velocity.length() * 1.02
            ball.velocity = pygame.Vector2(0, -speed).rotate(angle)
            ball.rect.center = (int(ball.position.x), int(ball.position.y))
            self._play_sound("bounce")

    def _brick_collision(self, ball: Ball) -> None:
        if ball.sticky:
            return
        hits = []
        for brick in self.bricks:
            if ball.rect.colliderect(brick.rect):
                hits.append(brick)
        if not hits:
            return

        for brick in hits:
            destroyed, gain = brick.hit()
            if destroyed:
                self.score += gain
                self._spawn_particles(brick.rect)
                self._play_sound("brick")
                if brick.powerup:
                    self._spawn_powerup(brick)
                self.bricks.remove(brick)
            else:
                self._play_sound("bounce")

            # Determine bounce axis based on overlap depth
            overlap = brick.rect.clip(ball.rect)
            if overlap.width < overlap.height:
                ball.velocity.x *= -1
                if ball.position.x < brick.rect.centerx:
                    ball.position.x -= overlap.width
                else:
                    ball.position.x += overlap.width
            else:
                ball.velocity.y *= -1
                if ball.position.y < brick.rect.centery:
                    ball.position.y -= overlap.height
                else:
                    ball.position.y += overlap.height

        ball.rect.center = (int(ball.position.x), int(ball.position.y))

    # ------------------------------------------------------------------
    # Power-ups, particles, and state transitions
    # ------------------------------------------------------------------
    def _spawn_particles(self, rect: pygame.Rect) -> None:
        centre = pygame.Vector2(rect.center)
        colour = (rect.width % 255, rect.height * 4 % 255, 200)
        for _ in range(10):
            velocity = pygame.Vector2(random.uniform(-120, 120), random.uniform(-200, 20))
            particle = Particle(centre.copy(), velocity, colour, random.uniform(4, 8), 0.5)
            self.particles.append(particle)

    def _update_particles(self, dt: float) -> None:
        self.particles = [p for p in self.particles if p.update(dt)]

    def _spawn_powerup(self, brick: Brick) -> None:
        rect = brick.rect.inflate(-brick.rect.width * 0.4, -brick.rect.height * 0.1)
        pill = PowerUp(rect, brick.powerup or "")
        self.powerups.append(pill)

    def _update_powerups(self, dt: float) -> None:
        if not self.paddle:
            return
        active: List[PowerUp] = []
        for power in self.powerups:
            if not power.update(dt):
                continue
            if power.rect.colliderect(self.paddle.rect):
                self._apply_powerup(power.power_type)
                self._play_sound("power")
            else:
                active.append(power)
        self.powerups = active

    def _apply_powerup(self, power_type: str) -> None:
        if not self.paddle:
            return
        if power_type == "enlarge":
            self.paddle.set_width(self.paddle.rect.width * 1.5, duration=12.0)
        elif power_type == "life":
            self.lives += 1
        elif power_type == "multiball":
            self._spawn_multiball()

    def _spawn_multiball(self) -> None:
        if not self.balls:
            return
        clones: List[Ball] = []
        for ball in self.balls:
            if ball.sticky:
                continue
            clone = Ball(ball.velocity.length())
            clone.position = ball.position.copy()
            clone.velocity = ball.velocity.rotate(random.choice([-25, 25]))
            clone.rect.center = (int(clone.position.x), int(clone.position.y))
            clone.sticky = False
            clones.append(clone)
        self.balls.extend(clones)

    def _check_ball_losses(self) -> None:
        remaining: List[Ball] = []
        lost = 0
        for ball in self.balls:
            if ball.position.y - ball.radius > HEIGHT + 40:
                lost += 1
            else:
                remaining.append(ball)
        if lost:
            self._play_sound("lose")
        self.balls = remaining

        if not self.balls:
            self.lives -= 1
            if self.lives < 0:
                self._game_over()
                return
            self._reset_for_life()

    def _reset_for_life(self) -> None:
        if not self.paddle:
            return
        level_data = self.levels[self.current_level_index]
        speed = float(level_data.get("ball_speed", 340))
        ball = Ball(speed)
        ball.attach_to(self.paddle)
        self.balls = [ball]

    def _game_over(self) -> None:
        self.running = False
        message = self.big_font.render("Game Over", True, TEXT_COLOUR)
        score_text = self.font.render(f"Score: {self.score}", True, TEXT_COLOUR)
        self.screen.blit(message, message.get_rect(center=(WIDTH // 2, HEIGHT // 2 - 40)))
        self.screen.blit(score_text, score_text.get_rect(center=(WIDTH // 2, HEIGHT // 2 + 10)))
        pygame.display.flip()
        pygame.time.wait(2000)

    def _check_level_complete(self, dt: float) -> None:
        if self.bricks:
            return
        self.level_complete_timer -= dt
        if self.level_complete_timer > 0:
            return
        self.current_level_index += 1
        if self.current_level_index >= len(self.levels):
            self._victory()
        else:
            self._start_level()

    def _victory(self) -> None:
        self.running = False
        message = self.big_font.render("You Win!", True, TEXT_COLOUR)
        score_text = self.font.render(f"Final Score: {self.score}", True, TEXT_COLOUR)
        self.screen.blit(message, message.get_rect(center=(WIDTH // 2, HEIGHT // 2 - 40)))
        self.screen.blit(score_text, score_text.get_rect(center=(WIDTH // 2, HEIGHT // 2 + 10)))
        pygame.display.flip()
        pygame.time.wait(2500)

    # ------------------------------------------------------------------
    # Rendering
    # ------------------------------------------------------------------
    def _draw(self) -> None:
        self.screen.fill(BACKGROUND)
        if not self.paddle:
            return

        self.bricks.draw(self.screen)
        for power in self.powerups:
            power.draw(self.screen)
        for particle in self.particles:
            particle.draw(self.screen)
        self.paddle.draw(self.screen)
        for ball in self.balls:
            ball.draw(self.screen)

        self._draw_hud()
        pygame.display.flip()

    def _draw_hud(self) -> None:
        level = self.levels[self.current_level_index]
        info = self.font.render(
            f"Level {self.current_level_index + 1}/{len(self.levels)}: {level.get('name', 'Unknown')}",
            True,
            TEXT_COLOUR,
        )
        score_text = self.font.render(f"Score: {self.score}", True, TEXT_COLOUR)
        lives_text = self.font.render(f"Lives: {self.lives}", True, TEXT_COLOUR)

        self.screen.blit(info, (20, 20))
        self.screen.blit(score_text, (20, 50))
        self.screen.blit(lives_text, (WIDTH - 160, 20))

        if any(ball.sticky for ball in self.balls):
            prompt = self.font.render("Press SPACE to launch", True, TEXT_COLOUR)
            self.screen.blit(prompt, prompt.get_rect(center=(WIDTH // 2, HEIGHT - 60)))

    def _play_sound(self, name: str) -> None:
        if not self.sound_enabled:
            return
        sound = self.sounds.get(name)
        if sound:
            sound.play()


def main() -> None:
    try:
        BreakoutGame().run()
    except pygame.error as exc:
        print(f"Pygame error: {exc}", file=sys.stderr)


if __name__ == "__main__":
    main()
