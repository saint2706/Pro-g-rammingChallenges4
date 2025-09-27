"""Scorched Earth clone implemented with pygame.

Features:
- Destructible terrain modelled as a one-dimensional height map drawn as polygons
- Wind affecting projectile physics
- Multiple weapon types with configurable presets saved to JSON
- Turn-based multiplayer with keyboard controls and AI opponents
- Score tracking and post-game scoreboard screen
- Configuration menus for match settings and weapon presets

The game is intentionally designed to be compact yet extendable; data-driven
weapon definitions and clearly separated menu logic make it easy to bolt on new
mechanics.
"""
from __future__ import annotations

import json
import math
import os
import random
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pygame

WIDTH, HEIGHT = 960, 600
GRAVITY = 80.0  # pixels per second squared
MAX_POWER = 120.0
TANK_WIDTH = 36
TANK_HEIGHT = 16
TERRAIN_RESOLUTION = 4  # pixels between height samples
WIND_RANGE = (-25.0, 25.0)

ASSET_DIR = Path(__file__).resolve().parent
WEAPON_PRESET_PATH = ASSET_DIR / "weapon_presets.json"

pygame.init()
pygame.font.init()
DEFAULT_FONT = pygame.font.SysFont("consolas", 20)
TITLE_FONT = pygame.font.SysFont("consolas", 36)


@dataclass
class Weapon:
    """Weapon definition controlling projectile explosion and damage."""

    name: str
    explosion_radius: float
    base_damage: float
    projectile_speed: float
    splash_falloff: float = 1.0

    def to_dict(self) -> Dict[str, float]:
        return {
            "name": self.name,
            "explosion_radius": self.explosion_radius,
            "base_damage": self.base_damage,
            "projectile_speed": self.projectile_speed,
            "splash_falloff": self.splash_falloff,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, float]) -> "Weapon":
        return cls(
            name=data["name"],
            explosion_radius=float(data["explosion_radius"]),
            base_damage=float(data["base_damage"]),
            projectile_speed=float(data["projectile_speed"]),
            splash_falloff=float(data.get("splash_falloff", 1.0)),
        )


DEFAULT_WEAPONS: List[Weapon] = [
    Weapon("Grenade", explosion_radius=45, base_damage=35, projectile_speed=85, splash_falloff=0.6),
    Weapon("Heavy Rocket", explosion_radius=60, base_damage=50, projectile_speed=95, splash_falloff=0.5),
    Weapon("Cluster", explosion_radius=35, base_damage=20, projectile_speed=90, splash_falloff=0.4),
    Weapon("Nuke", explosion_radius=85, base_damage=80, projectile_speed=80, splash_falloff=0.7),
]


@dataclass
class Tank:
    name: str
    color: Tuple[int, int, int]
    is_ai: bool
    x: float = 0
    y: float = 0
    angle: float = 45.0
    power: float = 70.0
    health: float = 100.0
    weapon_index: int = 0
    score: int = 0
    total_damage: float = 0.0

    def rect(self) -> pygame.Rect:
        return pygame.Rect(int(self.x - TANK_WIDTH / 2), int(self.y - TANK_HEIGHT), TANK_WIDTH, TANK_HEIGHT)


@dataclass
class Projectile:
    owner: Tank
    weapon: Weapon
    position: pygame.math.Vector2
    velocity: pygame.math.Vector2
    active: bool = True


@dataclass
class Terrain:
    width: int
    height: int
    resolution: int
    points: List[int] = field(default_factory=list)

    @classmethod
    def generate(cls, width: int, height: int, resolution: int) -> "Terrain":
        points = []
        base = height - 80
        variation = 50
        step = math.pi / (width / resolution)
        phase = random.random() * math.pi
        for i in range(0, width, resolution):
            noise = math.sin(i * step + phase) * variation
            jitter = random.uniform(-20, 20)
            points.append(int(min(height - 40, max(height - 200, base + noise + jitter))))
        return cls(width, height, resolution, points)

    def height_at(self, x: float) -> int:
        xi = int(max(0, min(len(self.points) - 1, x // self.resolution)))
        return self.points[xi]

    def carve_crater(self, center_x: float, radius: float) -> None:
        start = max(0, int((center_x - radius) // self.resolution))
        end = min(len(self.points) - 1, int((center_x + radius) // self.resolution))
        for i in range(start, end + 1):
            dx = abs(center_x - (i * self.resolution))
            if dx <= radius:
                depth = math.cos((dx / radius) * math.pi) * radius * 0.6
                self.points[i] = min(self.height, int(self.points[i] + depth))

    def settle_tank(self, tank: Tank) -> None:
        ground = self.height_at(tank.x)
        tank.y = ground

    def draw(self, surface: pygame.Surface) -> None:
        polygon_points = [(0, self.height)]
        for i, h in enumerate(self.points):
            polygon_points.append((i * self.resolution, h))
        polygon_points.append((self.width, self.height))
        pygame.draw.polygon(surface, (60, 42, 20), polygon_points)


class InputManager:
    """Encapsulates keyboard handling for both menus and in-game controls."""

    def __init__(self) -> None:
        self.just_pressed: set[int] = set()

    def update(self) -> None:
        self.just_pressed.clear()
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                raise SystemExit
            if event.type == pygame.KEYDOWN:
                self.just_pressed.add(event.key)

    def was_pressed(self, key: int) -> bool:
        return key in self.just_pressed


class MenuOption:
    def __init__(self, label: str, action) -> None:
        self.label = label
        self.action = action


class Menu:
    """Simple vertical menu widget."""

    def __init__(self, title: str, options: List[MenuOption]) -> None:
        self.title = title
        self.options = options
        self.selected = 0

    def handle_input(self, input_mgr: InputManager) -> None:
        keys = pygame.key.get_pressed()
        if input_mgr.was_pressed(pygame.K_UP):
            self.selected = (self.selected - 1) % len(self.options)
        elif input_mgr.was_pressed(pygame.K_DOWN):
            self.selected = (self.selected + 1) % len(self.options)
        elif input_mgr.was_pressed(pygame.K_RETURN) or input_mgr.was_pressed(pygame.K_SPACE):
            self.options[self.selected].action()
        if keys[pygame.K_ESCAPE]:
            raise MenuExit

    def draw(self, surface: pygame.Surface) -> None:
        surface.fill((18, 24, 32))
        title_surface = TITLE_FONT.render(self.title, True, (240, 240, 240))
        surface.blit(title_surface, (WIDTH // 2 - title_surface.get_width() // 2, 80))
        for idx, option in enumerate(self.options):
            color = (255, 200, 90) if idx == self.selected else (200, 200, 200)
            text_surface = DEFAULT_FONT.render(option.label, True, color)
            surface.blit(text_surface, (WIDTH // 2 - text_surface.get_width() // 2, 180 + idx * 40))


class MenuExit(Exception):
    pass


class WeaponPresetStore:
    """Load/save custom weapon presets from JSON."""

    def __init__(self, path: Path) -> None:
        self.path = path

    def load(self) -> List[Weapon]:
        if not self.path.exists():
            return DEFAULT_WEAPONS.copy()
        with self.path.open("r", encoding="utf8") as fh:
            data = json.load(fh)
        return [Weapon.from_dict(entry) for entry in data]

    def save(self, weapons: List[Weapon]) -> None:
        payload = [weapon.to_dict() for weapon in weapons]
        with self.path.open("w", encoding="utf8") as fh:
            json.dump(payload, fh, indent=2)


class GameApp:
    def __init__(self) -> None:
        self.screen = pygame.display.set_mode((WIDTH, HEIGHT))
        pygame.display.set_caption("Scorched Earth Clone")
        self.clock = pygame.time.Clock()
        self.input = InputManager()
        self.weapon_store = WeaponPresetStore(WEAPON_PRESET_PATH)
        self.weapons: List[Weapon] = self.weapon_store.load()
        self.match_settings = {
            "players": 2,
            "ai_players": 1,
            "rounds": 1,
            "wind_min": WIND_RANGE[0],
            "wind_max": WIND_RANGE[1],
        }
        self.state = "main_menu"
        self.humans: List[Tank] = []
        self.ai: List[Tank] = []
        self.tanks: List[Tank] = []
        self.terrain = Terrain.generate(WIDTH, HEIGHT, TERRAIN_RESOLUTION)
        self.wind = 0.0
        self.projectile: Optional[Projectile] = None
        self.current_turn = 0
        self.round = 1

    # ----------------------
    # Menu state management
    # ----------------------
    def run(self) -> None:
        while True:
            if self.state == "main_menu":
                self.run_main_menu()
            elif self.state == "config":
                self.run_config_menu()
            elif self.state == "weapon_config":
                self.run_weapon_menu()
            elif self.state == "game":
                self.run_game_loop()
            elif self.state == "scoreboard":
                self.run_scoreboard()

    def run_main_menu(self) -> None:
        def start_game():
            self.round = 1
            self.setup_game()
            self.state = "game"

        def configure():
            self.state = "config"

        def weapon_menu():
            self.state = "weapon_config"

        def quit_game():
            pygame.quit()
            raise SystemExit

        menu = Menu(
            "Scorched Earth",
            [
                MenuOption("Start Match", start_game),
                MenuOption("Match Configuration", configure),
                MenuOption("Weapon Presets", weapon_menu),
                MenuOption("Quit", quit_game),
            ],
        )
        while self.state == "main_menu":
            self.input.update()
            try:
                menu.handle_input(self.input)
            except MenuExit:
                self.state = "main_menu"
            self.screen.fill((18, 24, 32))
            menu.draw(self.screen)
            self.draw_footer("Arrow keys to navigate, Enter to select, Esc to stay")
            pygame.display.flip()
            self.clock.tick(60)

    def run_config_menu(self) -> None:
        options = [
            ("Human players", "players"),
            ("AI players", "ai_players"),
            ("Rounds", "rounds"),
            ("Wind minimum", "wind_min"),
            ("Wind maximum", "wind_max"),
        ]
        selected = 0
        while self.state == "config":
            self.input.update()
            keys = pygame.key.get_pressed()
            if self.input.was_pressed(pygame.K_ESCAPE):
                self.state = "main_menu"
                break
            if self.input.was_pressed(pygame.K_DOWN):
                selected = (selected + 1) % len(options)
            elif self.input.was_pressed(pygame.K_UP):
                selected = (selected - 1) % len(options)
            if self.input.was_pressed(pygame.K_LEFT):
                self.adjust_setting(options[selected][1], -1)
            elif self.input.was_pressed(pygame.K_RIGHT):
                self.adjust_setting(options[selected][1], 1)
            elif self.input.was_pressed(pygame.K_RETURN):
                self.state = "main_menu"
                break

            self.screen.fill((12, 18, 30))
            title = TITLE_FONT.render("Match Configuration", True, (220, 220, 240))
            self.screen.blit(title, (WIDTH // 2 - title.get_width() // 2, 60))
            for idx, (label, key) in enumerate(options):
                color = (255, 200, 90) if idx == selected else (200, 200, 200)
                value = self.match_settings[key]
                text_surface = DEFAULT_FONT.render(f"{label}: {value}", True, color)
                self.screen.blit(text_surface, (200, 160 + idx * 40))
            instructions = "Left/Right to modify, Enter to accept, Esc to cancel"
            self.draw_footer(instructions)
            pygame.display.flip()
            self.clock.tick(60)

    def adjust_setting(self, key: str, direction: int) -> None:
        if key in {"players", "ai_players", "rounds"}:
            new_value = max(1, self.match_settings[key] + direction)
            self.match_settings[key] = new_value
        elif key in {"wind_min", "wind_max"}:
            delta = 2.5 * direction
            self.match_settings[key] = round(self.match_settings[key] + delta, 1)
            if self.match_settings["wind_min"] > self.match_settings["wind_max"]:
                self.match_settings["wind_min"], self.match_settings["wind_max"] = (
                    self.match_settings["wind_max"],
                    self.match_settings["wind_min"],
                )

    def run_weapon_menu(self) -> None:
        selected = 0
        attribute_index = 0
        attributes = ["explosion_radius", "base_damage", "projectile_speed", "splash_falloff"]
        while self.state == "weapon_config":
            self.input.update()
            if self.input.was_pressed(pygame.K_ESCAPE):
                self.state = "main_menu"
                break
            if self.input.was_pressed(pygame.K_DOWN):
                selected = (selected + 1) % len(self.weapons)
            elif self.input.was_pressed(pygame.K_UP):
                selected = (selected - 1) % len(self.weapons)
            if self.input.was_pressed(pygame.K_RIGHT):
                attribute_index = (attribute_index + 1) % len(attributes)
            elif self.input.was_pressed(pygame.K_LEFT):
                attribute_index = (attribute_index - 1) % len(attributes)
            elif self.input.was_pressed(pygame.K_MINUS) or self.input.was_pressed(pygame.K_KP_MINUS):
                self.modify_weapon(self.weapons[selected], attributes[attribute_index], -1)
            elif self.input.was_pressed(pygame.K_EQUALS) or self.input.was_pressed(pygame.K_KP_PLUS):
                self.modify_weapon(self.weapons[selected], attributes[attribute_index], 1)
            elif self.input.was_pressed(pygame.K_s):
                self.weapon_store.save(self.weapons)
            elif self.input.was_pressed(pygame.K_r):
                self.weapons = DEFAULT_WEAPONS.copy()

            self.screen.fill((30, 24, 20))
            title = TITLE_FONT.render("Weapon Presets", True, (240, 230, 210))
            self.screen.blit(title, (WIDTH // 2 - title.get_width() // 2, 50))
            for idx, weapon in enumerate(self.weapons):
                color = (255, 210, 90) if idx == selected else (210, 210, 210)
                line = f"{weapon.name}"
                weapon_surface = DEFAULT_FONT.render(line, True, color)
                self.screen.blit(weapon_surface, (120, 140 + idx * 30))
            weapon = self.weapons[selected]
            attr = attributes[attribute_index]
            attr_title = DEFAULT_FONT.render(f"Adjusting: {attr}", True, (255, 200, 90))
            self.screen.blit(attr_title, (480, 140))
            stats = [
                f"Explosion radius: {weapon.explosion_radius:.1f}",
                f"Base damage: {weapon.base_damage:.1f}",
                f"Projectile speed: {weapon.projectile_speed:.1f}",
                f"Splash falloff: {weapon.splash_falloff:.2f}",
            ]
            for i, text in enumerate(stats):
                stat_surface = DEFAULT_FONT.render(text, True, (220, 220, 220))
                self.screen.blit(stat_surface, (480, 180 + i * 30))
            self.draw_footer("Up/Down select weapon, Left/Right attribute, +/- adjust, S save, R reset, Esc back")
            pygame.display.flip()
            self.clock.tick(60)

    def modify_weapon(self, weapon: Weapon, attribute: str, direction: int) -> None:
        delta = 5 if attribute != "splash_falloff" else 0.1
        current = getattr(weapon, attribute)
        new_value = max(1.0, current + delta * direction)
        if attribute == "splash_falloff":
            new_value = max(0.1, min(1.5, current + 0.1 * direction))
        setattr(weapon, attribute, new_value)

    # ----------------------
    # Game setup and loop
    # ----------------------
    def setup_game(self) -> None:
        total_players = self.match_settings["players"] + self.match_settings["ai_players"]
        previous_stats: Dict[str, Tuple[int, float]] = {
            tank.name: (tank.score, tank.total_damage) for tank in self.tanks
        }
        color_palette = [
            (235, 90, 70),
            (80, 180, 235),
            (120, 220, 140),
            (250, 200, 80),
            (200, 120, 235),
            (230, 150, 90),
        ]
        random.shuffle(color_palette)
        self.tanks = []
        for i in range(total_players):
            is_ai = i >= self.match_settings["players"]
            tank = Tank(
                name=f"AI {i - self.match_settings['players'] + 1}" if is_ai else f"Player {i + 1}",
                color=color_palette[i % len(color_palette)],
                is_ai=is_ai,
            )
            self.tanks.append(tank)
        spacing = WIDTH / (len(self.tanks) + 1)
        self.terrain = Terrain.generate(WIDTH, HEIGHT, TERRAIN_RESOLUTION)
        for idx, tank in enumerate(self.tanks):
            tank.x = (idx + 1) * spacing
            self.terrain.settle_tank(tank)
            tank.health = 100.0
            tank.angle = 45.0
            tank.power = 70.0
            tank.weapon_index = 0
            if tank.name in previous_stats:
                tank.score, tank.total_damage = previous_stats[tank.name]
            else:
                tank.score = 0
                tank.total_damage = 0.0
        self.wind = random.uniform(self.match_settings["wind_min"], self.match_settings["wind_max"])
        self.projectile = None
        self.current_turn = 0

    def next_turn(self) -> None:
        alive_tanks = [tank for tank in self.tanks if tank.health > 0]
        if len(alive_tanks) <= 1:
            winner = alive_tanks[0] if alive_tanks else None
            if winner:
                winner.score += 1
            if self.round >= self.match_settings["rounds"]:
                self.state = "scoreboard"
            else:
                self.round += 1
                self.setup_game()
            return
        self.current_turn = (self.current_turn + 1) % len(self.tanks)
        if self.tanks[self.current_turn].health <= 0:
            self.next_turn()
        else:
            self.wind = random.uniform(self.match_settings["wind_min"], self.match_settings["wind_max"])
            self.projectile = None

    def run_game_loop(self) -> None:
        while self.state == "game":
            dt = self.clock.tick(60) / 1000.0
            self.input.update()
            active_tank = self.tanks[self.current_turn]
            if active_tank.health <= 0:
                self.next_turn()
                continue
            if not self.projectile:
                if active_tank.is_ai:
                    self.handle_ai_turn(active_tank)
                else:
                    self.handle_player_turn(active_tank)
            else:
                self.update_projectile(dt)
            self.draw_game(active_tank)
            pygame.display.flip()

    def handle_player_turn(self, tank: Tank) -> None:
        keys = pygame.key.get_pressed()
        if self.input.was_pressed(pygame.K_ESCAPE):
            self.state = "main_menu"
            return
        if keys[pygame.K_LEFT]:
            tank.angle = max(5, tank.angle - 80 * self.clock.get_time() / 1000.0)
        if keys[pygame.K_RIGHT]:
            tank.angle = min(175, tank.angle + 80 * self.clock.get_time() / 1000.0)
        if keys[pygame.K_DOWN]:
            tank.power = max(10, tank.power - 80 * self.clock.get_time() / 1000.0)
        if keys[pygame.K_UP]:
            tank.power = min(MAX_POWER, tank.power + 80 * self.clock.get_time() / 1000.0)
        if self.input.was_pressed(pygame.K_q):
            tank.weapon_index = (tank.weapon_index - 1) % len(self.weapons)
        elif self.input.was_pressed(pygame.K_e):
            tank.weapon_index = (tank.weapon_index + 1) % len(self.weapons)
        if self.input.was_pressed(pygame.K_SPACE):
            self.launch_projectile(tank)

    def handle_ai_turn(self, tank: Tank) -> None:
        target = self.choose_ai_target(tank)
        angle, power = self.estimate_ai_shot(tank, target)
        tank.angle = angle
        tank.power = power
        tank.weapon_index = random.randrange(len(self.weapons))
        self.launch_projectile(tank)

    def choose_ai_target(self, tank: Tank) -> Tank:
        opponents = [t for t in self.tanks if t is not tank and t.health > 0]
        return min(opponents, key=lambda t: abs(t.x - tank.x))

    def estimate_ai_shot(self, tank: Tank, target: Tank) -> Tuple[float, float]:
        best_angle = 45.0
        best_power = 70.0
        best_error = float("inf")
        for angle in range(20, 160, 5):
            for power in range(40, int(MAX_POWER), 10):
                landing_x = self.simulate_shot_landing(tank, math.radians(angle), power)
                error = abs(landing_x - target.x)
                if error < best_error:
                    best_error = error
                    best_angle = float(angle)
                    best_power = float(power)
        return best_angle, best_power

    def simulate_shot_landing(self, tank: Tank, angle_rad: float, power: float) -> float:
        vx = math.cos(angle_rad) * power
        vy = -math.sin(angle_rad) * power
        pos = pygame.math.Vector2(tank.x, tank.y - TANK_HEIGHT)
        wind_acc = self.wind
        time_step = 0.1
        for _ in range(120):
            vx += wind_acc * time_step
            vy += GRAVITY * time_step
            pos.x += vx * time_step
            pos.y += vy * time_step
            if pos.x < 0 or pos.x > WIDTH or pos.y > HEIGHT:
                break
            terrain_height = self.terrain.height_at(pos.x)
            if pos.y >= terrain_height:
                break
        return pos.x

    def launch_projectile(self, tank: Tank) -> None:
        weapon = self.weapons[tank.weapon_index]
        angle_rad = math.radians(tank.angle)
        speed = weapon.projectile_speed * (tank.power / MAX_POWER)
        velocity = pygame.math.Vector2(math.cos(angle_rad) * speed, -math.sin(angle_rad) * speed)
        position = pygame.math.Vector2(tank.x, tank.y - TANK_HEIGHT)
        self.projectile = Projectile(tank, weapon, position, velocity)

    def update_projectile(self, dt: float) -> None:
        assert self.projectile is not None
        projectile = self.projectile
        projectile.velocity.x += self.wind * dt
        projectile.velocity.y += GRAVITY * dt
        projectile.position += projectile.velocity * dt
        if (
            projectile.position.x < 0
            or projectile.position.x > WIDTH
            or projectile.position.y > HEIGHT
            or projectile.position.y >= self.terrain.height_at(projectile.position.x)
        ):
            self.resolve_explosion(projectile)
            self.projectile = None
            self.next_turn()

    def resolve_explosion(self, projectile: Projectile) -> None:
        center = projectile.position
        weapon = projectile.weapon
        self.terrain.carve_crater(center.x, weapon.explosion_radius)
        for tank in self.tanks:
            if tank.health <= 0:
                continue
            dx = tank.x - center.x
            dy = (tank.y - TANK_HEIGHT / 2) - center.y
            distance = math.hypot(dx, dy)
            if distance <= weapon.explosion_radius:
                damage_ratio = 1 - (distance / weapon.explosion_radius) ** weapon.splash_falloff
                damage = max(0, weapon.base_damage * damage_ratio)
                tank.health -= damage
                projectile.owner.total_damage += damage
                if tank.health <= 0:
                    projectile.owner.score += 1
        for tank in self.tanks:
            if tank.health > 0:
                self.terrain.settle_tank(tank)

    def draw_game(self, active_tank: Tank) -> None:
        self.screen.fill((10, 12, 20))
        self.terrain.draw(self.screen)
        for tank in self.tanks:
            if tank.health <= 0:
                continue
            pygame.draw.rect(self.screen, tank.color, tank.rect())
            health_ratio = max(0, tank.health / 100)
            pygame.draw.rect(
                self.screen,
                (60, 12, 12),
                pygame.Rect(tank.rect().left, tank.rect().top - 8, TANK_WIDTH, 6),
            )
            pygame.draw.rect(
                self.screen,
                (200, 60, 60),
                pygame.Rect(tank.rect().left, tank.rect().top - 8, int(TANK_WIDTH * health_ratio), 6),
            )
        if self.projectile:
            pygame.draw.circle(self.screen, (250, 250, 250), self.projectile.position, 4)
        hud_lines = [
            f"Round {self.round}/{self.match_settings['rounds']} - Wind {self.wind:+.1f}",
            f"Turn: {active_tank.name} | Angle: {active_tank.angle:.1f} | Power: {active_tank.power:.1f}",
            f"Weapon: {self.weapons[active_tank.weapon_index].name}",
        ]
        for i, line in enumerate(hud_lines):
            text = DEFAULT_FONT.render(line, True, (240, 240, 240))
            self.screen.blit(text, (20, 20 + i * 24))
        control_hint = (
            "Arrow keys adjust angle/power, Q/E weapon select, Space to fire. Esc for menu."
            if not active_tank.is_ai
            else "AI aiming..."
        )
        hint_surface = DEFAULT_FONT.render(control_hint, True, (200, 200, 200))
        self.screen.blit(hint_surface, (20, HEIGHT - 40))

    def run_scoreboard(self) -> None:
        while self.state == "scoreboard":
            self.input.update()
            if self.input.was_pressed(pygame.K_RETURN) or self.input.was_pressed(pygame.K_ESCAPE):
                self.state = "main_menu"
                break
            self.screen.fill((14, 16, 30))
            title = TITLE_FONT.render("Match Results", True, (240, 240, 240))
            self.screen.blit(title, (WIDTH // 2 - title.get_width() // 2, 60))
            sorted_tanks = sorted(self.tanks, key=lambda t: (t.health > 0, t.score, t.total_damage), reverse=True)
            for idx, tank in enumerate(sorted_tanks):
                line = f"{tank.name:>10} | Wins: {tank.score} | Damage: {tank.total_damage:.1f} | Health: {max(0, tank.health):.1f}"
                color = tank.color if tank.health > 0 else (120, 120, 120)
                text = DEFAULT_FONT.render(line, True, color)
                self.screen.blit(text, (180, 160 + idx * 32))
            self.draw_footer("Enter/Esc to return to main menu")
            pygame.display.flip()
            self.clock.tick(60)

    # ----------------------
    # Utility helpers
    # ----------------------
    def draw_footer(self, text: str) -> None:
        surface = DEFAULT_FONT.render(text, True, (210, 210, 210))
        self.screen.blit(surface, (WIDTH // 2 - surface.get_width() // 2, HEIGHT - 50))


def main() -> None:
    os.environ.setdefault("SDL_VIDEO_CENTERED", "1")
    app = GameApp()
    app.run()


if __name__ == "__main__":
    main()
