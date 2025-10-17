"""Core cloth simulation based on Verlet integration.

The module provides a `Cloth` class that manages a grid of particles connected by
simple distance constraints.  Each simulation step performs a Verlet update,
optionally applies gravity and wind forces, and iteratively satisfies
constraints to keep the fabric cohesive.

Example
-------
>>> cloth = Cloth(width=2.0, height=1.5, num_x=15, num_y=10)
>>> cloth.step(0.016)  # Advance ~1/60th of a second
>>> positions = cloth.positions
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, Iterable, List, Sequence, Tuple

import numpy as np

ArrayLike = Sequence[float]


@dataclass
class Particle:
    """A single node of the cloth mesh."""

    position: np.ndarray
    prev_position: np.ndarray
    acceleration: np.ndarray
    rest_position: np.ndarray
    pinned: bool = False
    pin_position: np.ndarray | None = None

    def apply_force(self, force: np.ndarray) -> None:
        """Accumulate a force (treated as acceleration for unit mass)."""

        if self.pinned:
            return
        self.acceleration += force

    def verlet(self, dt: float, damping: float) -> None:
        """Advance the particle using Verlet integration."""

        if self.pinned:
            target = (
                self.pin_position
                if self.pin_position is not None
                else self.rest_position
            )
            self.position[...] = target
            self.prev_position[...] = target
            self.acceleration[...] = 0.0
            return

        current = self.position.copy()
        velocity = (self.position - self.prev_position) * damping
        self.position += velocity + self.acceleration * (dt * dt)
        self.prev_position = current
        self.acceleration[...] = 0.0


@dataclass
class DistanceConstraint:
    """Maintains a fixed distance between two particles."""

    particle_a: Particle
    particle_b: Particle
    rest_length: float
    stiffness: float = 1.0

    def satisfy(self) -> None:
        pos_a = self.particle_a.position
        pos_b = self.particle_b.position
        delta = pos_b - pos_a
        distance = np.linalg.norm(delta)
        if distance == 0.0:
            return
        difference = (distance - self.rest_length) / distance
        correction = delta * 0.5 * self.stiffness * difference

        if not self.particle_a.pinned:
            pos_a += correction
        if not self.particle_b.pinned:
            pos_b -= correction


class Cloth:
    """Two-dimensional cloth simulation driven by Verlet integration."""

    def __init__(
        self,
        width: float = 2.0,
        height: float = 1.5,
        num_x: int = 30,
        num_y: int = 20,
        pinned_rows: int = 1,
        gravity: ArrayLike = (0.0, 9.81),
        wind: ArrayLike = (3.0, 0.0),
        damping: float = 0.995,
        constraint_iterations: int = 5,
    ) -> None:
        if num_x < 2 or num_y < 2:
            raise ValueError("Cloth requires at least a 2x2 grid of particles")
        if not 0 <= pinned_rows <= num_y:
            raise ValueError("pinned_rows must be between 0 and num_y inclusive")

        self.width = float(width)
        self.height = float(height)
        self.num_x = int(num_x)
        self.num_y = int(num_y)
        self.damping = float(damping)
        self.constraint_iterations = int(constraint_iterations)

        self.gravity_vector = np.array(gravity, dtype=np.float64)
        self.wind_base = np.array(wind, dtype=np.float64)
        self.wind_variation = np.array(wind, dtype=np.float64) * 0.35

        self.gravity_enabled = True
        self.wind_enabled = False

        self._time = 0.0

        self._grid: List[List[Particle]] = []
        self._constraints: List[DistanceConstraint] = []

        self._init_particles(pinned_rows)
        self._init_constraints()

    # ------------------------------------------------------------------
    # Initialization helpers
    def _init_particles(self, pinned_rows: int) -> None:
        spacing_x = self.width / (self.num_x - 1)
        spacing_y = self.height / (self.num_y - 1)

        for y in range(self.num_y):
            row: List[Particle] = []
            for x in range(self.num_x):
                position = np.array([x * spacing_x, y * spacing_y], dtype=np.float64)
                particle = Particle(
                    position=position.copy(),
                    prev_position=position.copy(),
                    acceleration=np.zeros(2, dtype=np.float64),
                    rest_position=position.copy(),
                )
                if y < pinned_rows:
                    particle.pinned = True
                    particle.pin_position = position.copy()
                row.append(particle)
            self._grid.append(row)

    def _init_constraints(self) -> None:
        rest_x = self.width / (self.num_x - 1)
        rest_y = self.height / (self.num_y - 1)

        for y in range(self.num_y):
            for x in range(self.num_x):
                if x < self.num_x - 1:
                    self._constraints.append(
                        DistanceConstraint(
                            self._grid[y][x],
                            self._grid[y][x + 1],
                            rest_x,
                        )
                    )
                if y < self.num_y - 1:
                    self._constraints.append(
                        DistanceConstraint(
                            self._grid[y][x],
                            self._grid[y + 1][x],
                            rest_y,
                        )
                    )

    # ------------------------------------------------------------------
    # Public API
    @property
    def particles(self) -> Iterable[Particle]:
        for row in self._grid:
            for particle in row:
                yield particle

    @property
    def constraints(self) -> Sequence[DistanceConstraint]:
        return self._constraints

    @property
    def positions(self) -> np.ndarray:
        return np.array([particle.position for particle in self.particles])

    @property
    def segments(self) -> np.ndarray:
        segments = []
        for constraint in self._constraints:
            segments.append(
                np.array(
                    [
                        constraint.particle_a.position.copy(),
                        constraint.particle_b.position.copy(),
                    ]
                )
            )
        return np.array(segments)

    def reset(self) -> None:
        """Return the cloth to its initial rest configuration."""

        for row in self._grid:
            for particle in row:
                particle.position[...] = particle.rest_position
                particle.prev_position[...] = particle.rest_position
                particle.acceleration[...] = 0.0
        self._time = 0.0

    def toggle_gravity(self) -> None:
        self.gravity_enabled = not self.gravity_enabled

    def toggle_wind(self) -> None:
        self.wind_enabled = not self.wind_enabled

    def set_gravity(self, enabled: bool) -> None:
        self.gravity_enabled = bool(enabled)

    def set_wind(self, enabled: bool) -> None:
        self.wind_enabled = bool(enabled)

    def step(self, dt: float) -> None:
        """Advance the simulation by `dt` seconds."""

        if dt <= 0:
            raise ValueError("dt must be positive")

        self._time += dt
        gust = (
            np.sin(self._time * 1.3) * self.wind_variation
            if self.wind_enabled
            else None
        )

        for particle in self.particles:
            if self.gravity_enabled:
                particle.apply_force(self.gravity_vector)
            if self.wind_enabled:
                force = self.wind_base + gust * (
                    1.0 + 0.25 * np.sin(particle.position[1] * 2.5)
                )
                particle.apply_force(force)
            particle.verlet(dt, self.damping)

        for _ in range(self.constraint_iterations):
            for constraint in self._constraints:
                constraint.satisfy()

    # ------------------------------------------------------------------
    # Utility helpers for demos / testing
    def displacements(self) -> np.ndarray:
        """Return the distance each constraint deviates from rest."""

        deviations = []
        for constraint in self._constraints:
            pa = constraint.particle_a.position
            pb = constraint.particle_b.position
            distance = np.linalg.norm(pb - pa)
            deviations.append(distance - constraint.rest_length)
        return np.array(deviations)

    def pin_indices(self) -> List[Tuple[int, int]]:
        """Return the grid indices of pinned particles."""

        indices: List[Tuple[int, int]] = []
        for y, row in enumerate(self._grid):
            for x, particle in enumerate(row):
                if particle.pinned:
                    indices.append((x, y))
        return indices

    def pinned_positions(self) -> np.ndarray:
        """Return an array of the world positions of pinned particles."""

        positions = []
        for y, row in enumerate(self._grid):
            for x, particle in enumerate(row):
                if particle.pinned:
                    positions.append(particle.position.copy())
        return np.array(positions)


def _capture_frame(
    cloth: "Cloth",
    step_index: int,
    time_elapsed: float,
    record_positions: bool,
    record_segments: bool,
) -> Dict[str, np.ndarray | int | float]:
    frame: Dict[str, np.ndarray | int | float] = {
        "step": step_index,
        "time": time_elapsed,
    }
    if record_positions:
        frame["positions"] = cloth.positions.copy()
    if record_segments:
        frame["segments"] = cloth.segments.copy()
    return frame


def simulate_cloth(
    steps: int,
    *,
    dt: float = 0.016,
    include_initial_state: bool = True,
    record_positions: bool = True,
    record_segments: bool = True,
    cloth: "Cloth" | None = None,
    **cloth_kwargs,
) -> List[Dict[str, np.ndarray | int | float]]:
    """Run a cloth simulation and capture frame data.

    Parameters
    ----------
    steps:
        Number of simulation steps to advance.  A value of ``0`` captures only
        the initial state (if requested).
    dt:
        Duration of each step.  Must be positive.
    include_initial_state:
        When ``True`` the first frame corresponds to the state before any
        integration step has been applied.
    record_positions / record_segments:
        Control which arrays are stored for each frame.  Disabling unused data
        avoids unnecessary allocations when only a subset of information is
        required.
    cloth:
        An optional pre-configured :class:`Cloth` instance.  When provided no
        additional keyword arguments may be supplied.
    **cloth_kwargs:
        Parameters forwarded to :class:`Cloth` when ``cloth`` is not supplied.

    Returns
    -------
    list of dict
        A list of frames; each frame contains the ``step`` index, ``time`` in
        seconds, and any requested arrays.
    """

    if steps < 0:
        raise ValueError("steps must be non-negative")
    if dt <= 0:
        raise ValueError("dt must be positive")

    if cloth is not None and cloth_kwargs:
        raise ValueError("Cannot supply cloth_kwargs when cloth instance is provided")

    if cloth is None:
        cloth = Cloth(**cloth_kwargs)
    else:
        cloth.reset()

    frames: List[Dict[str, np.ndarray | int | float]] = []

    if include_initial_state:
        frames.append(_capture_frame(cloth, 0, 0.0, record_positions, record_segments))

    for step in range(1, steps + 1):
        cloth.step(dt)
        time_elapsed = step * dt
        frames.append(
            _capture_frame(cloth, step, time_elapsed, record_positions, record_segments)
        )

    return frames


def simulate_positions(
    steps: int,
    *,
    dt: float = 0.016,
    include_initial_state: bool = True,
    cloth: "Cloth" | None = None,
    **cloth_kwargs,
) -> List[np.ndarray]:
    """Convenience wrapper returning only the node positions for each frame."""

    frames = simulate_cloth(
        steps,
        dt=dt,
        include_initial_state=include_initial_state,
        record_positions=True,
        record_segments=False,
        cloth=cloth,
        **cloth_kwargs,
    )
    return [frame["positions"] for frame in frames]


def simulate_segments(
    steps: int,
    *,
    dt: float = 0.016,
    include_initial_state: bool = True,
    cloth: "Cloth" | None = None,
    **cloth_kwargs,
) -> List[np.ndarray]:
    """Convenience wrapper returning only the constraint segments for each frame."""

    frames = simulate_cloth(
        steps,
        dt=dt,
        include_initial_state=include_initial_state,
        record_positions=False,
        record_segments=True,
        cloth=cloth,
        **cloth_kwargs,
    )
    return [frame["segments"] for frame in frames]
