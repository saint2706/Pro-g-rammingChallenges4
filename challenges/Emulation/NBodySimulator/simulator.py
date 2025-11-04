"""Interactive pygame front-end for the N-body simulator.

This script provides an interactive 2D gravitational sandbox where users can
create and observe the interactions of celestial bodies.

Controls:
- Left-click and drag: Place a new body with an initial velocity.
- Mouse wheel or +/- keys: Adjust the mass of the next body to be spawned.
- `[` / `]`: Decrease or increase the simulation timestep.
- `T`: Toggle particle trails.
- `Space`: Pause or resume the simulation.
- `C`: Clear all bodies from the simulation.
- `Esc` or close button: Exit the simulator.
"""

from __future__ import annotations

import math
from pathlib import Path
from typing import Tuple

try:
    import pygame
except ImportError as exc:
    pygame = None
    _PYGAME_IMPORT_ERROR = exc

# Assuming the core nbody simulation is in a separate module.
from pro_g_rammingchallenges4.nbody import Body, NBodySimulation

# --- Constants ---
WIDTH, HEIGHT = 1024, 720
BACKGROUND = (10, 12, 25)
INFO_COLOR = (200, 200, 210)
DEFAULT_SPAWN_MASS = 20.0
MASS_STEP = 1.2
VELOCITY_SCALE = 0.1


def _require_pygame() -> None:
    """Checks if the pygame library is installed."""
    if pygame is not None:
        return
    raise RuntimeError(
        "Pygame is required for the N-Body simulator. "
        "Install it with 'pip install pygame'."
    ) from _PYGAME_IMPORT_ERROR


def _draw_body(surface: pygame.Surface, body: Body) -> None:
    """Draws a single celestial body on the screen.

    Args:
        surface: The pygame surface to draw on.
        body: The Body object to draw.
    """
    # Draw the main body.
    pygame.draw.circle(
        surface, body.color, (int(body.position[0]), int(body.position[1])), int(body.radius)
    )
    # Draw a border.
    pygame.draw.circle(
        surface, (30, 30, 50), (int(body.position[0]), int(body.position[1])), int(body.radius), 1
    )
    # Draw the trail if it exists.
    if body.trail:
        pygame.draw.lines(
            surface, body.color, False, [(int(x), int(y)) for x, y in body.trail], 1
        )


def _draw_velocity_vector(
    surface: pygame.Surface, start: Tuple[int, int], current: Tuple[int, int]
) -> None:
    """Draws the initial velocity vector when placing a new body.

    Args:
        surface: The pygame surface to draw on.
        start: The starting position of the drag.
        current: The current position of the mouse.
    """
    dx, dy = current[0] - start[0], current[1] - start[1]
    pygame.draw.line(surface, (120, 180, 255), start, current, 2)
    length = math.hypot(dx, dy)
    if length > 0:
        direction = (dx / length, dy / length)
        tip = (current[0] + direction[0] * 12, current[1] + direction[1] * 12)
        pygame.draw.circle(surface, (120, 180, 255), tip, 3)


def _draw_overlay(
    surface: pygame.Surface,
    font: pygame.font.Font,
    sim: NBodySimulation,
    paused: bool,
    spawn_mass: float,
) -> None:
    """Draws the information overlay on the screen.

    Args:
        surface: The pygame surface to draw on.
        font: The font to use for the text.
        sim: The NBodySimulation instance.
        paused: A boolean indicating if the simulation is paused.
        spawn_mass: The mass of the next body to be spawned.
    """
    lines = [
        f"Bodies: {len(sim.bodies)} | dt: {sim.timestep:.4f} | Trails: {'On' if sim.trails_enabled else 'Off'}",
        f"Paused: {'Yes' if paused else 'No'} | Spawn mass: {spawn_mass:.2f}",
        "Controls: Drag to set velocity, Mouse wheel or +/- to adjust mass, [ and ] to change timestep.",
    ]
    y = 12
    for line in lines:
        text = font.render(line, True, INFO_COLOR)
        surface.blit(text, (12, y))
        y += text.get_height() + 4


def main() -> None:
    """The main entry point for the N-Body simulator."""
    _require_pygame()
    assert pygame is not None

    pygame.init()
    screen = pygame.display.set_mode((WIDTH, HEIGHT))
    pygame.display.set_caption("N-Body Simulator")
    clock = pygame.time.Clock()
    font = pygame.font.Font(None, 24)

    sim = NBodySimulation(
        gravitational_constant=1000.0, softening=4.0, max_trail_length=600
    )
    sim.timestep = 0.02

    paused = False
    spawn_mass = DEFAULT_SPAWN_MASS
    drag_start: Tuple[int, int] | None = None

    running = True
    while running:
        real_dt = clock.tick(60) / 1000.0
        # --- Event Handling ---
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
                elif event.key == pygame.K_t:
                    sim.toggle_trails()
                elif event.key == pygame.K_c:
                    sim.reset()
                elif event.key in (pygame.K_PLUS, pygame.K_EQUALS):
                    spawn_mass *= MASS_STEP
                elif event.key == pygame.K_MINUS:
                    spawn_mass = max(spawn_mass / MASS_STEP, 0.1)
            elif event.type == pygame.MOUSEBUTTONDOWN:
                if event.button == 1:
                    drag_start = event.pos
                elif event.button == 4:  # Scroll up
                    spawn_mass *= MASS_STEP
                elif event.button == 5:  # Scroll down
                    spawn_mass = max(spawn_mass / MASS_STEP, 0.1)
            elif event.type == pygame.MOUSEBUTTONUP:
                if event.button == 1 and drag_start:
                    dx, dy = event.pos[0] - drag_start[0], event.pos[1] - drag_start[1]
                    sim.add_body(
                        mass=spawn_mass,
                        position=drag_start,
                        velocity=(dx * VELOCITY_SCALE, dy * VELOCITY_SCALE),
                    )
                    drag_start = None

        # --- Simulation Step ---
        if not paused:
            # Use a fixed-step integration for stability.
            accumulate = 0.0
            while accumulate + sim.timestep <= real_dt:
                sim.step(sim.timestep)
                accumulate += sim.timestep
            if real_dt > accumulate:
                sim.step(real_dt - accumulate)

        # --- Drawing ---
        screen.fill(BACKGROUND)
        for body in sim.bodies:
            _draw_body(screen, body)
        if drag_start:
            _draw_velocity_vector(screen, drag_start, pygame.mouse.get_pos())
        _draw_overlay(screen, font, sim, paused, spawn_mass)
        pygame.display.flip()

    pygame.quit()


if __name__ == "__main__":
    main()
