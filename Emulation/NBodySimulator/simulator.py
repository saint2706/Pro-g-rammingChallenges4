"""Interactive pygame front-end for the N-body simulator.

Controls
========
- **Left click & drag**: place a new body. Drag direction sets initial velocity.
- **Mouse wheel / +/-**: adjust spawn mass.
- **[`[`] / [`]`]**: decrease / increase simulation timestep.
- **T**: toggle particle trails.
- **Space**: pause / resume integration.
- **C**: clear all bodies.
- **Esc / Close button**: exit.

The physics core lives in :mod:`pro_g_rammingchallenges4.nbody` and is
covered by automated tests.
"""
from __future__ import annotations

import math
from pathlib import Path
from typing import Tuple

import pygame

from pro_g_rammingchallenges4.nbody import Body, NBodySimulation

WIDTH, HEIGHT = 1024, 720
BACKGROUND = (10, 12, 25)
INFO_COLOR = (200, 200, 210)
DEFAULT_SPAWN_MASS = 20.0
MASS_STEP = 1.2
VELOCITY_SCALE = 0.1
FONT_PATH = str(Path(pygame.font.get_default_font()))


def _draw_body(surface: pygame.Surface, body: Body) -> None:
    pygame.draw.circle(surface, body.color, (int(body.position[0]), int(body.position[1])), int(body.radius))
    pygame.draw.circle(surface, (30, 30, 50), (int(body.position[0]), int(body.position[1])), int(body.radius), 1)
    if body.trail:
        pygame.draw.lines(surface, body.color, False, [(int(x), int(y)) for x, y in body.trail], 1)


def _draw_velocity_vector(surface: pygame.Surface, start: Tuple[int, int], current: Tuple[int, int]) -> None:
    dx = current[0] - start[0]
    dy = current[1] - start[1]
    pygame.draw.line(surface, (120, 180, 255), start, current, 2)
    length = math.hypot(dx, dy)
    if length > 0:
        direction = (dx / length, dy / length)
        tip = (current[0] + int(direction[0] * 12), current[1] + int(direction[1] * 12))
        pygame.draw.circle(surface, (120, 180, 255), tip, 3)


def _draw_overlay(surface: pygame.Surface, font: pygame.font.Font, sim: NBodySimulation, paused: bool, spawn_mass: float) -> None:
    lines = [
        f"Bodies: {len(sim.bodies)} | dt: {sim.timestep:.4f} | Trails: {'on' if sim.trails_enabled else 'off'}",
        f"Paused: {'yes' if paused else 'no'} | Spawn mass: {spawn_mass:.2f}",
        "Drag to set velocity. Wheel or +/- to adjust mass. [ and ] tweak dt.",
    ]
    y = 12
    for line in lines:
        text = font.render(line, True, INFO_COLOR)
        surface.blit(text, (12, y))
        y += text.get_height() + 4


def main() -> None:
    pygame.init()
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    pygame.display.set_caption("N-Body Simulator")
    clock = pygame.time.Clock()
    font = pygame.font.Font(FONT_PATH, 18)

    sim = NBodySimulation(gravitational_constant=1000.0, softening=4.0, max_trail_length=600)
    sim.timestep = 0.02

    paused = False
    spawn_mass = DEFAULT_SPAWN_MASS
    drag_start: Tuple[int, int] | None = None

    running = True
    while running:
        real_dt = clock.tick(60) / 1000.0
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            elif event.type == pygame.KEYDOWN:
                if event.key == pygame.K_ESCAPE:
                    running = False
                elif event.key == pygame.K_SPACE:
                    paused = not paused
                elif event.key == pygame.K_LEFTBRACKET:
                    sim.timestep = max(sim.timestep / 1.3, 1e-4)
                elif event.key == pygame.K_RIGHTBRACKET:
                    sim.timestep = min(sim.timestep * 1.3, 1.0)
                elif event.key in (pygame.K_t,):
                    sim.toggle_trails()
                elif event.key in (pygame.K_c,):
                    sim.reset()
                elif event.key in (pygame.K_PLUS, pygame.K_EQUALS):
                    spawn_mass *= MASS_STEP
                elif event.key == pygame.K_MINUS:
                    spawn_mass = max(spawn_mass / MASS_STEP, 0.1)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:
                    drag_start = event.pos
                elif event.button == 4:
                    spawn_mass *= MASS_STEP
                elif event.button == 5:
                    spawn_mass = max(spawn_mass / MASS_STEP, 0.1)
            elif event.type == pygame.MOUSEBUTTONUP:
                if event.button == 1 and drag_start is not None:
                    dx = event.pos[0] - drag_start[0]
                    dy = event.pos[1] - drag_start[1]
                    sim.add_body(
                        mass=spawn_mass,
                        position=drag_start,
                        velocity=(dx * VELOCITY_SCALE, dy * VELOCITY_SCALE),
                        color=(255, 210, 140),
                    )
                    drag_start = None

        if not paused:
            # Integrate multiple small steps for stability.
            accumulate = 0.0
            while accumulate + sim.timestep <= real_dt:
                sim.step(sim.timestep)
                accumulate += sim.timestep
            if real_dt > accumulate:
                sim.step(real_dt - accumulate)

        screen.fill(BACKGROUND)
        for body in sim.bodies:
            _draw_body(screen, body)
        if drag_start is not None:
            _draw_velocity_vector(screen, drag_start, pygame.mouse.get_pos())
        _draw_overlay(screen, font, sim, paused, spawn_mass)
        pygame.display.flip()

    pygame.quit()


if __name__ == "__main__":  # pragma: no cover - manual usage only
    main()
