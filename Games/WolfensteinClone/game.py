"""2.5D Wolfenstein-like raycasting engine implemented with pygame."""

from __future__ import annotations

import argparse
import json
import math
import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Tuple

import pygame

ASSET_DIR = Path(__file__).resolve().parent / "Assets"
MAP_DIR = Path(__file__).resolve().parent / "maps"


@dataclass
class ControlScheme:
    """Mapping of high level actions to pygame key constants."""

    move_forward: int = pygame.K_w
    move_back: int = pygame.K_s
    strafe_left: int = pygame.K_a
    strafe_right: int = pygame.K_d
    turn_left: int = pygame.K_LEFT
    turn_right: int = pygame.K_RIGHT
    sprint: int = pygame.K_LSHIFT


@dataclass
class GameConfig:
    """Configuration for the engine runtime."""

    map_path: Path
    controls: ControlScheme = field(default_factory=ControlScheme)
    screen_size: Tuple[int, int] = (960, 600)
    fov: float = math.pi / 3  # 60 degrees
    wall_textures: Optional[Dict[int, str]] = None
    sprite_texture: str = "enemy_guard.png"


class RaycastingMap:
    """Tile-based map that stores collision data."""

    def __init__(self, name: str, layout: List[List[int]]) -> None:
        self.name = name
        self.layout = layout
        self.height = len(layout)
        self.width = len(layout[0]) if self.height else 0

    def tile_at(self, x: float, y: float) -> int:
        grid_x = int(x)
        grid_y = int(y)
        if grid_x < 0 or grid_y < 0 or grid_x >= self.width or grid_y >= self.height:
            return 1  # treat outside map as wall
        return self.layout[grid_y][grid_x]

    def is_walkable(self, x: float, y: float) -> bool:
        return self.tile_at(x, y) == 0


@dataclass
class Sprite:
    x: float
    y: float
    texture: pygame.Surface

    def distance_to(self, px: float, py: float) -> float:
        return math.hypot(self.x - px, self.y - py)


class WolfensteinGame:
    """Main runtime for the Wolfenstein 3D inspired raycaster."""

    def __init__(
        self,
        config: Optional[GameConfig] = None,
        *,
        headless: bool = False,
    ) -> None:
        pygame.init()
        self.headless = headless
        self.config = config or self._default_config()
        self.screen_width, self.screen_height = self.config.screen_size
        flags = pygame.HWSURFACE | pygame.DOUBLEBUF
        if headless:
            flags |= pygame.HIDDEN
        self.screen = pygame.display.set_mode(self.config.screen_size, flags)
        pygame.display.set_caption("Wolfenstein Clone")
        self.clock = pygame.time.Clock()

        self.map, self.player_pos, self.player_angle, enemy_positions = self._load_map(self.config.map_path)
        self.controls = self.config.controls
        self.move_speed = 3.0
        self.turn_speed = 2.0
        self.sprint_multiplier = 1.8

        self.wall_textures = self._load_wall_textures(self.config.wall_textures)
        self.sprite_texture = self._load_texture(self.config.sprite_texture).convert_alpha()
        self.sprites = [Sprite(enemy[0], enemy[1], self.sprite_texture) for enemy in enemy_positions]

        self.depth_buffer: List[float] = [0.0] * self.screen_width
        self.background = self._create_background()
        self.minimap_scale = 8

    # ------------------------------------------------------------------
    # Setup helpers
    def _default_config(self) -> GameConfig:
        return GameConfig(
            map_path=MAP_DIR / "default_map.json",
            controls=ControlScheme(),
            wall_textures={1: "wall_stone.png", 2: "wall_brick.png"},
        )

    def _create_background(self) -> pygame.Surface:
        bg = pygame.Surface(self.config.screen_size)
        horizon = self.screen_height // 2
        bg.fill((40, 40, 40), rect=pygame.Rect(0, 0, self.screen_width, horizon))
        bg.fill((15, 15, 15), rect=pygame.Rect(0, horizon, self.screen_width, horizon))
        return bg.convert()

    def _load_map(
        self, path: Path
    ) -> Tuple[RaycastingMap, Tuple[float, float], float, List[Tuple[float, float]]]:
        with Path(path).open("r", encoding="utf-8") as handle:
            data = json.load(handle)
        layout = data["layout"]
        map_name = data.get("name", "Custom Map")
        player_start = data.get("player_start", {"x": 1.5, "y": 1.5, "angle": 0.0})
        enemies = [(enemy["x"], enemy["y"]) for enemy in data.get("enemies", [])]
        return (
            RaycastingMap(map_name, layout),
            (float(player_start["x"]), float(player_start["y"])),
            float(player_start.get("angle", 0.0)),
            enemies,
        )

    def _load_texture(self, filename: str) -> pygame.Surface:
        texture_path = ASSET_DIR / filename
        surface = pygame.image.load(texture_path.as_posix())
        return surface.convert()

    def _load_wall_textures(self, mapping: Optional[Dict[int, str]]) -> Dict[int, pygame.Surface]:
        mapping = mapping or {1: "wall_stone.png"}
        textures = {}
        for tile_id, filename in mapping.items():
            surface = self._load_texture(filename)
            textures[tile_id] = surface
        return textures

    # ------------------------------------------------------------------
    # Game Loop
    def run(self, *, max_frames: Optional[int] = None) -> None:
        frame = 0
        running = True
        while running:
            dt = self.clock.tick(60) / 1000.0
            running = self._process_events()
            self._update(dt)
            self._render()
            pygame.display.flip()

            frame += 1
            if max_frames is not None and frame >= max_frames:
                break

        if self.headless:
            pygame.display.quit()
        pygame.quit()

    # ------------------------------------------------------------------
    def _process_events(self) -> bool:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                return False
            if event.type == pygame.KEYDOWN and event.key == pygame.K_ESCAPE:
                return False
        return True

    def _update(self, dt: float) -> None:
        keys = pygame.key.get_pressed()

        speed = self.move_speed * dt
        if keys[self.controls.sprint]:
            speed *= self.sprint_multiplier

        dir_x = math.cos(self.player_angle)
        dir_y = math.sin(self.player_angle)
        strafe_x = -dir_y
        strafe_y = dir_x

        move_x = 0.0
        move_y = 0.0

        if keys[self.controls.move_forward]:
            move_x += dir_x * speed
            move_y += dir_y * speed
        if keys[self.controls.move_back]:
            move_x -= dir_x * speed
            move_y -= dir_y * speed
        if keys[self.controls.strafe_left]:
            move_x += strafe_x * speed
            move_y += strafe_y * speed
        if keys[self.controls.strafe_right]:
            move_x -= strafe_x * speed
            move_y -= strafe_y * speed

        self._move_player(move_x, move_y)

        if keys[self.controls.turn_left]:
            self.player_angle -= self.turn_speed * dt
        if keys[self.controls.turn_right]:
            self.player_angle += self.turn_speed * dt

        self.player_angle %= math.tau

    def _move_player(self, dx: float, dy: float) -> None:
        new_x = self.player_pos[0] + dx
        new_y = self.player_pos[1] + dy

        if self.map.is_walkable(new_x, self.player_pos[1]):
            self.player_pos = (new_x, self.player_pos[1])
        if self.map.is_walkable(self.player_pos[0], new_y):
            self.player_pos = (self.player_pos[0], new_y)

    # ------------------------------------------------------------------
    def _render(self) -> None:
        self.screen.blit(self.background, (0, 0))
        self._cast_walls()
        self._render_sprites()
        self._draw_minimap()

    def _cast_walls(self) -> None:
        dir_x = math.cos(self.player_angle)
        dir_y = math.sin(self.player_angle)
        plane_x = -dir_y * math.tan(self.config.fov / 2)
        plane_y = dir_x * math.tan(self.config.fov / 2)

        for column in range(self.screen_width):
            camera_x = 2 * column / self.screen_width - 1
            ray_dir_x = dir_x + plane_x * camera_x
            ray_dir_y = dir_y + plane_y * camera_x

            map_x = int(self.player_pos[0])
            map_y = int(self.player_pos[1])

            delta_dist_x = abs(1 / ray_dir_x) if ray_dir_x != 0 else float("inf")
            delta_dist_y = abs(1 / ray_dir_y) if ray_dir_y != 0 else float("inf")

            if ray_dir_x < 0:
                step_x = -1
                side_dist_x = (self.player_pos[0] - map_x) * delta_dist_x
            else:
                step_x = 1
                side_dist_x = (map_x + 1.0 - self.player_pos[0]) * delta_dist_x

            if ray_dir_y < 0:
                step_y = -1
                side_dist_y = (self.player_pos[1] - map_y) * delta_dist_y
            else:
                step_y = 1
                side_dist_y = (map_y + 1.0 - self.player_pos[1]) * delta_dist_y

            hit = False
            side = 0
            tile_id = 0

            while not hit:
                if side_dist_x < side_dist_y:
                    side_dist_x += delta_dist_x
                    map_x += step_x
                    side = 0
                else:
                    side_dist_y += delta_dist_y
                    map_y += step_y
                    side = 1
                tile_id = self.map.tile_at(map_x, map_y)
                if tile_id > 0:
                    hit = True

            if side == 0:
                perp_wall_dist = (map_x - self.player_pos[0] + (1 - step_x) / 2) / (ray_dir_x or 1e-6)
            else:
                perp_wall_dist = (map_y - self.player_pos[1] + (1 - step_y) / 2) / (ray_dir_y or 1e-6)
            perp_wall_dist = max(perp_wall_dist, 1e-4)
            self.depth_buffer[column] = perp_wall_dist

            line_height = int(self.screen_height / perp_wall_dist)
            line_height = max(1, line_height)
            draw_start = -line_height // 2 + self.screen_height // 2
            draw_end = line_height // 2 + self.screen_height // 2

            texture = self.wall_textures.get(tile_id)
            if texture is None:
                continue

            if side == 0:
                wall_x = self.player_pos[1] + perp_wall_dist * ray_dir_y
            else:
                wall_x = self.player_pos[0] + perp_wall_dist * ray_dir_x
            wall_x -= math.floor(wall_x)

            tex_width = texture.get_width()
            tex_height = texture.get_height()
            tex_x = int(wall_x * tex_width)
            if (side == 0 and ray_dir_x > 0) or (side == 1 and ray_dir_y < 0):
                tex_x = tex_width - tex_x - 1

            column_surface = pygame.Surface((1, line_height), pygame.SRCALPHA)
            for y in range(line_height):
                tex_y = int(((y / line_height) * tex_height))
                tex_y = min(tex_y, tex_height - 1)
                color = pygame.Color(*texture.get_at((tex_x, tex_y)))
                if side == 1:
                    color.r = max(0, int(color.r * 0.75))
                    color.g = max(0, int(color.g * 0.75))
                    color.b = max(0, int(color.b * 0.75))
                column_surface.set_at((0, y), color)

            draw_pos = (column, draw_start)
            self.screen.blit(column_surface, draw_pos)

    def _render_sprites(self) -> None:
        dir_x = math.cos(self.player_angle)
        dir_y = math.sin(self.player_angle)
        plane_x = -dir_y * math.tan(self.config.fov / 2)
        plane_y = dir_x * math.tan(self.config.fov / 2)

        inv_det = 1.0 / (plane_x * dir_y - dir_x * plane_y + 1e-6)

        for sprite in sorted(self.sprites, key=lambda s: s.distance_to(*self.player_pos), reverse=True):
            sprite_x = sprite.x - self.player_pos[0]
            sprite_y = sprite.y - self.player_pos[1]

            transform_x = inv_det * (dir_y * sprite_x - dir_x * sprite_y)
            transform_y = inv_det * (-plane_y * sprite_x + plane_x * sprite_y)

            if transform_y <= 0:
                continue

            sprite_screen_x = int((self.screen_width / 2) * (1 + transform_x / transform_y))

            sprite_height = max(1, abs(int(self.screen_height / transform_y)))
            sprite_width = sprite_height

            draw_start_y = -sprite_height // 2 + self.screen_height // 2
            draw_end_y = sprite_height // 2 + self.screen_height // 2
            draw_start_x = -sprite_width // 2 + sprite_screen_x
            draw_end_x = sprite_width // 2 + sprite_screen_x

            scaled_texture = pygame.transform.smoothscale(sprite.texture, (sprite_width, sprite_height))

            for stripe in range(draw_start_x, draw_end_x):
                if 0 <= stripe < self.screen_width and transform_y < self.depth_buffer[stripe]:
                    column = stripe - draw_start_x
                    if 0 <= column < scaled_texture.get_width():
                        column_surface = scaled_texture.subsurface((column, 0, 1, sprite_height))
                        self.screen.blit(column_surface, (stripe, draw_start_y))

    def _draw_minimap(self) -> None:
        map_surface = pygame.Surface((self.map.width * self.minimap_scale, self.map.height * self.minimap_scale), pygame.SRCALPHA)
        wall_color = (120, 120, 120, 220)
        floor_color = (30, 30, 30, 180)
        for y in range(self.map.height):
            for x in range(self.map.width):
                rect = pygame.Rect(
                    x * self.minimap_scale,
                    y * self.minimap_scale,
                    self.minimap_scale,
                    self.minimap_scale,
                )
                tile = self.map.layout[y][x]
                map_surface.fill(wall_color if tile else floor_color, rect)

        px = int(self.player_pos[0] * self.minimap_scale)
        py = int(self.player_pos[1] * self.minimap_scale)
        pygame.draw.circle(map_surface, (255, 255, 0), (px, py), 4)
        ray_length = 10
        end_x = px + int(math.cos(self.player_angle) * ray_length)
        end_y = py + int(math.sin(self.player_angle) * ray_length)
        pygame.draw.line(map_surface, (255, 0, 0), (px, py), (end_x, end_y), 2)

        for sprite in self.sprites:
            sx = int(sprite.x * self.minimap_scale)
            sy = int(sprite.y * self.minimap_scale)
            pygame.draw.circle(map_surface, (0, 200, 200), (sx, sy), 3)

        border = pygame.Surface((map_surface.get_width() + 4, map_surface.get_height() + 4), pygame.SRCALPHA)
        border.fill((0, 0, 0, 150))
        border.blit(map_surface, (2, 2))
        self.screen.blit(border, (10, 10))


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Wolfenstein 3D inspired raycasting engine")
    parser.add_argument("--map", type=str, default=str(MAP_DIR / "default_map.json"), help="Path to a JSON map file")
    parser.add_argument("--width", type=int, default=960, help="Screen width")
    parser.add_argument("--height", type=int, default=600, help="Screen height")
    parser.add_argument("--fov", type=float, default=math.pi / 3, help="Field of view in radians")
    return parser


def main(argv: Optional[Iterable[str]] = None) -> None:
    parser = build_argument_parser()
    args = parser.parse_args(argv)
    config = GameConfig(
        map_path=Path(args.map),
        screen_size=(args.width, args.height),
        fov=args.fov,
        wall_textures={1: "wall_stone.png", 2: "wall_brick.png"},
    )
    game = WolfensteinGame(config)
    game.run()


if __name__ == "__main__":
    # Allow running headless in CI when a dummy video driver is used.
    if os.environ.get("SDL_VIDEODRIVER") == "dummy":
        pygame.display.init()
    main()
