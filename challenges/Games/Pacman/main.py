"""Pac-Man clone with behavioural ghosts and JSON-defined level data."""

from __future__ import annotations

import json
import logging
import math
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Set, Tuple

import pygame

logger = logging.getLogger(__name__)

Vec2 = pygame.math.Vector2


class Direction(Vec2):
    """Convenience wrapper for axis-aligned unit vectors."""

    @property
    def tuple(self) -> Tuple[int, int]:
        return int(self.x), int(self.y)


UP = Direction(0, -1)
DOWN = Direction(0, 1)
LEFT = Direction(-1, 0)
RIGHT = Direction(1, 0)
DIRECTIONS: Sequence[Direction] = (UP, DOWN, LEFT, RIGHT)


def load_json(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as fh:
        return json.load(fh)


class TileType(Enum):
    WALL = "#"
    PELLET = "."
    POWER = "o"
    EMPTY = " "
    DOOR = "-"
    GHOST = "G"
    PLAYER_SPAWN = "P"


@dataclass
class LevelData:
    name: str
    tile_size: int
    layout: List[str]
    pacman_start: Tuple[int, int]
    ghost_home: Tuple[int, int]
    ghost_starts: Dict[str, Tuple[int, int]]
    scatter_targets: Dict[str, Tuple[int, int]]

    @classmethod
    def from_file(cls, path: Path) -> "LevelData":
        payload = load_json(path)
        return cls(
            name=payload["name"],
            tile_size=payload["tile_size"],
            layout=payload["layout"],
            pacman_start=tuple(payload["pacman_start"]),
            ghost_home=tuple(payload["ghost_home"]),
            ghost_starts={
                name: tuple(pos) for name, pos in payload["ghost_starts"].items()
            },
            scatter_targets={
                name: tuple(pos) for name, pos in payload["scatter_targets"].items()
            },
        )

    @property
    def width(self) -> int:
        return len(self.layout[0])

    @property
    def height(self) -> int:
        return len(self.layout)

    def tile_at(self, tile: Tuple[int, int]) -> str:
        x, y = tile
        if y < 0 or y >= self.height:
            return TileType.WALL.value
        row = self.layout[y]
        if x < 0 or x >= len(row):
            return TileType.WALL.value
        return row[x]

    def is_wall(self, tile: Tuple[int, int]) -> bool:
        return self.tile_at(tile) == TileType.WALL.value

    def walkable(self, tile: Tuple[int, int], *, through_door: bool = True) -> bool:
        value = self.tile_at(tile)
        if value == TileType.WALL.value:
            return False
        if value == TileType.DOOR.value and not through_door:
            return False
        return True

    def pellets(self) -> Tuple[Set[Tuple[int, int]], Set[Tuple[int, int]]]:
        pellets: Set[Tuple[int, int]] = set()
        powers: Set[Tuple[int, int]] = set()
        for y, row in enumerate(self.layout):
            for x, cell in enumerate(row):
                if cell == TileType.PELLET.value:
                    pellets.add((x, y))
                elif cell == TileType.POWER.value:
                    powers.add((x, y))
        return pellets, powers


class PathCache:
    """Caches BFS distance fields for faster ghost path queries."""

    def __init__(self, level: LevelData):
        self.level = level
        self._cache: Dict[Tuple[Tuple[int, int], bool], Dict[Tuple[int, int], int]] = {}

    def _neighbors(
        self, tile: Tuple[int, int], *, through_door: bool
    ) -> Iterable[Tuple[int, int]]:
        for direction in DIRECTIONS:
            next_tile = tile[0] + int(direction.x), tile[1] + int(direction.y)
            if self.level.walkable(next_tile, through_door=through_door):
                yield next_tile

    def _distances_from(
        self, start: Tuple[int, int], *, through_door: bool
    ) -> Dict[Tuple[int, int], int]:
        key = (start, through_door)
        if key in self._cache:
            return self._cache[key]
        queue: List[Tuple[int, int]] = [start]
        distances: Dict[Tuple[int, int], int] = {start: 0}
        for tile in queue:
            base_distance = distances[tile]
            for neighbour in self._neighbors(tile, through_door=through_door):
                if neighbour not in distances:
                    distances[neighbour] = base_distance + 1
                    queue.append(neighbour)
        self._cache[key] = distances
        return distances

    def distance(
        self,
        start: Tuple[int, int],
        goal: Tuple[int, int],
        *,
        through_door: bool,
    ) -> float:
        distances = self._distances_from(start, through_door=through_door)
        return distances.get(goal, math.inf)

    def clear(self) -> None:
        self._cache.clear()


class AnimatedSprite:
    """Simple sprite wrapper handling orientation flipping."""

    def __init__(self, image: pygame.Surface):
        self.base = image.convert_alpha()
        self.current = self.base
        self.flipped_x = False

    def oriented(self, direction: Direction) -> pygame.Surface:
        if direction == LEFT and not self.flipped_x:
            self.current = pygame.transform.flip(self.base, True, False)
            self.flipped_x = True
        elif direction in (RIGHT, UP, DOWN) and self.flipped_x:
            self.current = pygame.transform.flip(self.base, True, False)
            self.flipped_x = False
        return self.current


@dataclass
class Entity:
    position: Vec2
    direction: Direction
    speed: float
    sprite: AnimatedSprite

    def rect(self, tile_size: int) -> pygame.Rect:
        return self.sprite.current.get_rect(center=self.pixel_position(tile_size))

    def pixel_position(self, tile_size: int) -> Tuple[int, int]:
        return int(self.position.x * tile_size + tile_size / 2), int(
            self.position.y * tile_size + tile_size / 2
        )

    def move(self, delta: Vec2):
        self.position += delta


class Pacman(Entity):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.pending_direction: Optional[Direction] = None

    def update(self, dt: float, level: LevelData, speed: float):
        tile_size = level.tile_size
        center = Vec2(self.position)
        if self.pending_direction is not None and self.can_move(
            self.pending_direction, level
        ):
            if self.is_aligned(center):
                self.direction = self.pending_direction
                self.pending_direction = None
        if not self.can_move(self.direction, level):
            return
        velocity = Vec2(self.direction) * (speed * dt / tile_size)
        self.position += velocity
        self.wrap(level)

    def is_aligned(self, pos: Vec2) -> bool:
        return abs(pos.x - round(pos.x)) < 0.05 and abs(pos.y - round(pos.y)) < 0.05

    def can_move(self, direction: Direction, level: LevelData) -> bool:
        next_tile = Vec2(self.position) + direction
        return level.walkable(
            (int(round(next_tile.x)), int(round(next_tile.y))), through_door=False
        )

    def queue_direction(self, direction: Direction):
        self.pending_direction = direction

    def wrap(self, level: LevelData):
        width = level.width
        if self.position.x < 0:
            self.position.x = width - 1
        elif self.position.x >= width:
            self.position.x = 0


class GhostState(Enum):
    SCATTER = "scatter"
    CHASE = "chase"
    FRIGHTENED = "frightened"
    EATEN = "eaten"


class Ghost(Entity):
    name: str
    scatter_target: Vec2
    home: Vec2
    state: GhostState
    frightened_timer: float

    def __init__(
        self,
        name: str,
        position: Vec2,
        direction: Direction,
        speed: float,
        sprite: AnimatedSprite,
        scatter_target: Tuple[int, int],
        home: Tuple[int, int],
    ):
        super().__init__(position, direction, speed, sprite)
        self.name = name
        self.scatter_target = Vec2(scatter_target)
        self.home = Vec2(home)
        self.state = GhostState.SCATTER
        self.frightened_timer = 0.0
        self.previous_direction: Direction = direction

    def available_directions(self, level: LevelData) -> List[Direction]:
        options: List[Direction] = []
        for direction in DIRECTIONS:
            next_tile = self.position + direction
            if direction == -self.direction:
                continue
            tile = int(round(next_tile.x)), int(round(next_tile.y))
            can_pass_door = self.state in (
                GhostState.EATEN,
                GhostState.CHASE,
                GhostState.SCATTER,
            )
            if level.walkable(tile, through_door=can_pass_door):
                options.append(direction)
        if not options:
            # allow reversing when forced
            options.append(-self.direction)
        return options

    def target_tile(
        self,
        pacman: Pacman,
        blinky: "Ghost",
        level: LevelData,
    ) -> Vec2:
        if self.state == GhostState.SCATTER:
            return self.scatter_target
        if self.state == GhostState.EATEN:
            return self.home
        if self.state == GhostState.FRIGHTENED:
            away = Vec2(self.position) - Vec2(pacman.position)
            if away.length_squared() == 0:
                return self.scatter_target
            return Vec2(self.position) + away.normalize() * 4
        # Chase behaviour
        pac_tile = Vec2(round(pacman.position.x), round(pacman.position.y))
        if self.name == "blinky":
            return pac_tile
        if self.name == "pinky":
            return pac_tile + pacman.direction * 4
        if self.name == "inky":
            ahead = pac_tile + pacman.direction * 2
            vector = ahead - Vec2(round(blinky.position.x), round(blinky.position.y))
            return Vec2(round(blinky.position.x), round(blinky.position.y)) + 2 * vector
        if self.name == "clyde":
            distance = (pac_tile - Vec2(self.position)).length()
            if distance > 8:
                return pac_tile
            return self.scatter_target
        return pac_tile

    def update(
        self,
        dt: float,
        level: LevelData,
        pacman: Pacman,
        blinky: "Ghost",
        speed: float,
        navigator: PathCache,
    ):
        if self.state == GhostState.FRIGHTENED:
            self.frightened_timer -= dt
            if self.frightened_timer <= 0:
                self.state = GhostState.CHASE
        if self.state == GhostState.EATEN:
            if Vec2(self.position).distance_to(self.home) < 0.1:
                self.state = GhostState.CHASE
        target = self.target_tile(pacman, blinky, level)
        direction = self.choose_direction(target, level, navigator)
        self.direction = direction
        velocity = Vec2(direction) * (speed * dt / level.tile_size)
        self.position += velocity
        self.wrap(level)

    def choose_direction(
        self, target: Vec2, level: LevelData, navigator: PathCache
    ) -> Direction:
        options = self.available_directions(level)
        best = options[0]
        best_distance = math.inf
        start_tile = (int(round(self.position.x)), int(round(self.position.y)))
        target_tile = (int(round(target.x)), int(round(target.y)))
        can_pass_door = self.state in (
            GhostState.EATEN,
            GhostState.CHASE,
            GhostState.SCATTER,
        )
        for direction in options:
            next_pos = self.position + direction
            next_tile = (int(round(next_pos.x)), int(round(next_pos.y)))
            distance = navigator.distance(
                next_tile, target_tile, through_door=can_pass_door
            )
            if distance < best_distance:
                best_distance = distance
                best = direction
        return best

    def enter_frightened(self, duration: float):
        if self.state == GhostState.EATEN:
            return
        self.state = GhostState.FRIGHTENED
        self.frightened_timer = duration

    def eaten(self):
        self.state = GhostState.EATEN

    def wrap(self, level: LevelData):
        width = level.width
        if self.position.x < 0:
            self.position.x = width - 1
        elif self.position.x >= width:
            self.position.x = 0


@dataclass
class Config:
    starting_lives: int
    base_pacman_speed: float
    base_ghost_speed: float
    frightened_speed: float
    frightened_duration: float
    elroy_threshold: int
    scatter_chase_cycle: List[float]
    level_speed_modifiers: List[Dict[str, float]]

    @classmethod
    def from_file(cls, path: Path) -> "Config":
        payload = load_json(path)
        return cls(
            starting_lives=payload["starting_lives"],
            base_pacman_speed=payload["base_pacman_speed"],
            base_ghost_speed=payload["base_ghost_speed"],
            frightened_speed=payload["frightened_speed"],
            frightened_duration=payload["frightened_duration"],
            elroy_threshold=payload["elroy_threshold"],
            scatter_chase_cycle=payload["scatter_chase_cycle"],
            level_speed_modifiers=payload["level_speed_modifiers"],
        )


class ModeController:
    """Cycles scatter/chase timings as per the classic arcade behaviour."""

    def __init__(self, cycle: Sequence[float]):
        self.cycle = list(cycle)
        self.timer = 0.0
        self.index = 0
        self.state = GhostState.SCATTER

    def update(self, dt: float) -> GhostState:
        if self.index >= len(self.cycle):
            self.state = GhostState.CHASE
            return self.state
        self.timer += dt
        if self.timer >= self.cycle[self.index]:
            self.timer = 0.0
            self.index += 1
            self.state = (
                GhostState.CHASE
                if self.state == GhostState.SCATTER
                else GhostState.SCATTER
            )
        return self.state

    def reset(self):
        self.timer = 0.0
        self.index = 0
        self.state = GhostState.SCATTER


class Game:
    def __init__(self, root: Path, *, enable_sound: bool = True):
        self.root = root
        self.config = Config.from_file(root / "config.json")
        self.level_paths = sorted((root / "maps").glob("level*.json"))
        if not self.level_paths:
            raise FileNotFoundError("No level JSON files found in maps/ directory")
        self.level_index = 0
        self.sound_enabled = enable_sound
        self.pellet_sound: Optional[pygame.mixer.Sound] = None
        self.power_sound: Optional[pygame.mixer.Sound] = None
        self.eat_ghost_sound: Optional[pygame.mixer.Sound] = None
        self.target_resolution: Tuple[int, int] = (0, 0)
        self.needs_resize = False
        self._ensure_display_surface()
        self.load_assets()
        self.clock = pygame.time.Clock()
        self.font = pygame.font.Font(None, 24)
        self.big_font = pygame.font.Font(None, 48)
        self.reset_game()

    def _ensure_display_surface(self):
        surface = pygame.display.get_surface()
        if surface is None:
            flags = getattr(pygame, "HIDDEN", 0)
            pygame.display.set_mode((1, 1), flags)

    def load_assets(self):
        image_dir = self.root / "Assets" / "Images"
        audio_dir = self.root / "Assets" / "Audio"
        self.pacman_sprite = AnimatedSprite(pygame.image.load(image_dir / "pacman.png"))
        self.ghost_sprites = {
            "blinky": AnimatedSprite(pygame.image.load(image_dir / "ghost_red.png")),
            "pinky": AnimatedSprite(pygame.image.load(image_dir / "ghost_pink.png")),
            "inky": AnimatedSprite(pygame.image.load(image_dir / "ghost_blue.png")),
            "clyde": AnimatedSprite(pygame.image.load(image_dir / "ghost_orange.png")),
        }
        self.frightened_sprite = pygame.image.load(
            image_dir / "ghost_frightened.png"
        ).convert_alpha()
        self.eaten_sprite = pygame.image.load(
            image_dir / "ghost_eaten.png"
        ).convert_alpha()
        if self.sound_enabled and pygame.mixer.get_init():
            try:
                self.pellet_sound = pygame.mixer.Sound(str(audio_dir / "pellet.wav"))
                self.power_sound = pygame.mixer.Sound(str(audio_dir / "power.wav"))
                self.eat_ghost_sound = pygame.mixer.Sound(str(audio_dir / "ghost.wav"))
            except (pygame.error, FileNotFoundError) as exc:
                logger.warning("Disabling audio playback: %s", exc)
                self.sound_enabled = False
                self.pellet_sound = None
                self.power_sound = None
                self.eat_ghost_sound = None
        else:
            if self.sound_enabled:
                logger.warning("Audio disabled: pygame mixer not initialised")
            self.sound_enabled = False

    def reset_game(self):
        self.lives = self.config.starting_lives
        self.score = 0
        self.level_index = 0
        self.load_level()

    def load_level(self):
        path = self.level_paths[self.level_index % len(self.level_paths)]
        self.level = LevelData.from_file(path)
        self.navigator = PathCache(self.level)
        pellets, powers = self.level.pellets()
        self.pellets = pellets
        self.power_pellets = powers
        self.pacman = Pacman(
            position=Vec2(self.level.pacman_start),
            direction=LEFT,
            speed=self.config.base_pacman_speed,
            sprite=self.pacman_sprite,
        )
        self.mode_controller = ModeController(self.config.scatter_chase_cycle)
        self.ghosts: Dict[str, Ghost] = {}
        for name, start in self.level.ghost_starts.items():
            sprite = self.ghost_sprites[name]
            ghost = Ghost(
                name=name,
                position=Vec2(start),
                direction=UP,
                speed=self.config.base_ghost_speed,
                sprite=sprite,
                scatter_target=self.level.scatter_targets[name],
                home=self.level.ghost_home,
            )
            ghost.state = GhostState.SCATTER
            self.ghosts[name] = ghost
        self.ghost_combo = 1
        self.frightened_remaining = 0.0
        modifiers = self.config.level_speed_modifiers[
            min(self.level_index, len(self.config.level_speed_modifiers) - 1)
        ]
        self.pacman_speed = self.config.base_pacman_speed * modifiers["pacman"]
        self.ghost_speed = self.config.base_ghost_speed * modifiers["ghost"]
        self.frightened_speed = self.config.frightened_speed * modifiers["frightened"]
        self.target_resolution = (
            self.level.width * self.level.tile_size,
            self.level.height * self.level.tile_size,
        )
        self.needs_resize = True

    def restart_positions(self):
        self.pacman.position = Vec2(self.level.pacman_start)
        self.pacman.direction = LEFT
        self.navigator.clear()
        for name, start in self.level.ghost_starts.items():
            ghost = self.ghosts[name]
            ghost.position = Vec2(start)
            ghost.direction = UP
            ghost.state = GhostState.SCATTER
            ghost.frightened_timer = 0.0
        self.mode_controller.reset()
        self.ghost_combo = 1
        self.frightened_remaining = 0.0

    def update(self, dt: float):
        state = self.mode_controller.update(dt)
        for ghost in self.ghosts.values():
            if ghost.state not in (GhostState.FRIGHTENED, GhostState.EATEN):
                ghost.state = state
        self.pacman.update(dt, self.level, self.pacman_speed)
        blinky = self.ghosts["blinky"]
        for ghost in self.ghosts.values():
            speed = (
                self.frightened_speed
                if ghost.state == GhostState.FRIGHTENED
                else self.ghost_speed
            )
            if ghost.state == GhostState.EATEN:
                speed = self.ghost_speed * 1.5
            ghost.update(dt, self.level, self.pacman, blinky, speed, self.navigator)
        self.check_pellet_collision()
        self.check_ghost_collision()
        if not self.pellets and not self.power_pellets:
            self.level_complete()

    def level_complete(self):
        self.level_index += 1
        self.load_level()

    def check_pellet_collision(self):
        tile = (int(round(self.pacman.position.x)), int(round(self.pacman.position.y)))
        if tile in self.pellets:
            self.pellets.remove(tile)
            self.score += 10
            self.play_sound(self.pellet_sound)
        if tile in self.power_pellets:
            self.power_pellets.remove(tile)
            self.score += 50
            self.play_sound(self.power_sound)
            self.enter_frightened()

    def enter_frightened(self):
        self.ghost_combo = 1
        duration = self.config.frightened_duration
        for ghost in self.ghosts.values():
            ghost.enter_frightened(duration)
        self.frightened_remaining = duration

    def check_ghost_collision(self):
        pac_pos = Vec2(self.pacman.position)
        for ghost in self.ghosts.values():
            if pac_pos.distance_to(ghost.position) < 0.45:
                if ghost.state == GhostState.FRIGHTENED:
                    ghost.eaten()
                    self.score += 200 * self.ghost_combo
                    self.ghost_combo *= 2
                    self.play_sound(self.eat_ghost_sound)
                elif ghost.state != GhostState.EATEN:
                    self.handle_pacman_caught()
                    break

    def play_sound(self, sound: Optional[pygame.mixer.Sound]):
        if self.sound_enabled and sound is not None:
            try:
                sound.play()
            except pygame.error as exc:
                logger.warning("Failed to play sound: %s", exc)
                self.sound_enabled = False

    def handle_pacman_caught(self):
        self.lives -= 1
        if self.lives <= 0:
            self.game_over()
        else:
            self.restart_positions()

    def game_over(self):
        self.restart_positions()
        self.lives = self.config.starting_lives
        self.score = 0
        self.level_index = 0
        self.load_level()

    def draw(self, screen: pygame.Surface):
        screen.fill((0, 0, 0))
        tile_size = self.level.tile_size
        for y, row in enumerate(self.level.layout):
            for x, cell in enumerate(row):
                if cell == TileType.WALL.value:
                    rect = pygame.Rect(
                        x * tile_size, y * tile_size, tile_size, tile_size
                    )
                    pygame.draw.rect(screen, (0, 51, 255), rect)
        for tile in self.pellets:
            center = (
                int(tile[0] * tile_size + tile_size / 2),
                int(tile[1] * tile_size + tile_size / 2),
            )
            pygame.draw.circle(screen, (255, 182, 85), center, 3)
        for tile in self.power_pellets:
            center = (
                int(tile[0] * tile_size + tile_size / 2),
                int(tile[1] * tile_size + tile_size / 2),
            )
            pygame.draw.circle(screen, (255, 182, 85), center, 6, width=2)
        # Draw Pac-Man
        sprite = self.pacman.sprite.oriented(self.pacman.direction)
        screen.blit(
            sprite, sprite.get_rect(center=self.pacman.pixel_position(tile_size))
        )
        # Draw ghosts
        for ghost in self.ghosts.values():
            if ghost.state == GhostState.FRIGHTENED:
                sprite = self.frightened_sprite
            elif ghost.state == GhostState.EATEN:
                sprite = self.eaten_sprite
            else:
                sprite = ghost.sprite.oriented(ghost.direction)
            screen.blit(sprite, sprite.get_rect(center=ghost.pixel_position(tile_size)))
        # HUD
        hud = self.font.render(f"Score: {self.score}", True, (255, 255, 255))
        screen.blit(hud, (10, 5))
        hud_level = self.font.render(
            f"Level: {self.level_index + 1}", True, (255, 255, 255)
        )
        screen.blit(hud_level, (200, 5))
        lives_text = self.font.render(f"Lives: {self.lives}", True, (255, 255, 255))
        screen.blit(lives_text, (350, 5))

    def run(self):
        screen = pygame.display.get_surface()
        running = True
        while running:
            if self.needs_resize:
                screen = pygame.display.set_mode(self.target_resolution)
                pygame.display.set_caption("Pac-Man")
                self.needs_resize = False
            dt = self.clock.tick(60) / 1000.0
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    running = False
                elif event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_ESCAPE:
                        running = False
                    elif event.key in (pygame.K_UP, pygame.K_w):
                        self.pacman.queue_direction(UP)
                    elif event.key in (pygame.K_DOWN, pygame.K_s):
                        self.pacman.queue_direction(DOWN)
                    elif event.key in (pygame.K_LEFT, pygame.K_a):
                        self.pacman.queue_direction(LEFT)
                    elif event.key in (pygame.K_RIGHT, pygame.K_d):
                        self.pacman.queue_direction(RIGHT)
            self.update(dt)
            self.draw(screen)
            pygame.display.flip()
        pygame.quit()


def main():
    logging.basicConfig(level=logging.INFO)
    pygame.init()
    mixer_ready = True
    try:
        pygame.mixer.init()
    except pygame.error as exc:
        logger.warning("Unable to initialise audio mixer: %s", exc)
        mixer_ready = False
    root = Path(__file__).resolve().parent
    Game(root, enable_sound=mixer_ready).run()


if __name__ == "__main__":
    main()
