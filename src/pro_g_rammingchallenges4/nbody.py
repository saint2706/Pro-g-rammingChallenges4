"""Core physics model for the N-body simulator challenge.

The module keeps the numerical integration code separate from the
pygame front-end so unit tests can exercise the physics without a GUI
context.  Bodies interact gravitationally, merge elastically when they
collide, and scale their radius with mass assuming constant density.
"""

from __future__ import annotations

from dataclasses import dataclass, field
import math
from typing import List, Sequence, Tuple

Vector = Tuple[float, float]


def radius_from_mass(
    mass: float,
    *,
    reference_mass: float = 1.0,
    reference_radius: float = 6.0,
) -> float:
    """Return a radius that scales with the cube root of the mass.

    The scaling assumes constant density, meaning doubling the mass
    increases the volume by a factor of two and therefore the radius by
    :math:`2^{1/3}`.
    """
    if mass <= 0:
        raise ValueError("Bodies must have a positive mass")
    radius = reference_radius * (mass / reference_mass) ** (1.0 / 3.0)
    return max(radius, reference_radius * 0.25)


@dataclass
class Body:
    """A particle in the simulation."""

    mass: float
    position: List[float]
    velocity: List[float]
    color: Tuple[int, int, int] = (255, 200, 120)
    trail: List[Vector] = field(default_factory=list)
    radius: float = field(init=False)

    def __post_init__(self) -> None:
        self.update_radius()

    def update_radius(self) -> None:
        self.radius = radius_from_mass(self.mass)

    def copy(self) -> "Body":
        return Body(
            mass=self.mass,
            position=list(self.position),
            velocity=list(self.velocity),
            color=self.color,
            trail=list(self.trail),
        )


class NBodySimulation:
    """Simple 2D gravitational N-body simulation with merging collisions."""

    def __init__(
        self,
        *,
        gravitational_constant: float = 1.0,
        softening: float = 1e-2,
        max_trail_length: int = 200,
    ) -> None:
        self.gravitational_constant = gravitational_constant
        self.softening = softening
        self.max_trail_length = max_trail_length
        self.bodies: List[Body] = []
        self.timestep = 0.02
        self.trails_enabled = True

    def add_body(
        self,
        mass: float,
        position: Sequence[float],
        velocity: Sequence[float] = (0.0, 0.0),
        color: Tuple[int, int, int] | None = None,
    ) -> Body:
        body = Body(
            mass=mass,
            position=list(position),
            velocity=list(velocity),
            color=color or (255, 200, 120),
        )
        self.bodies.append(body)
        return body

    # Physics helpers -------------------------------------------------
    def gravitational_force(self, subject: Body, attractor: Body) -> Vector:
        """Return the gravitational force vector exerted on *subject* by *attractor*."""
        dx = attractor.position[0] - subject.position[0]
        dy = attractor.position[1] - subject.position[1]
        distance_sq = dx * dx + dy * dy + self.softening * self.softening
        distance = math.sqrt(distance_sq)
        if distance == 0.0:
            return (0.0, 0.0)
        strength = (
            self.gravitational_constant
            * subject.mass
            * attractor.mass
            / (distance_sq * distance)
        )
        return (strength * dx, strength * dy)

    def _compute_forces(self) -> List[Vector]:
        forces = [(0.0, 0.0) for _ in self.bodies]
        for i, body in enumerate(self.bodies):
            fx = fy = 0.0
            for j, other in enumerate(self.bodies):
                if i == j:
                    continue
                f = self.gravitational_force(body, other)
                fx += f[0]
                fy += f[1]
            forces[i] = (fx, fy)
        return forces

    def step(self, dt: float | None = None) -> None:
        """Advance the simulation by one timestep."""
        if dt is None:
            dt = self.timestep
        if not self.bodies:
            return
        forces = self._compute_forces()
        # Symplectic Euler integration: update velocity then position.
        for body, (fx, fy) in zip(self.bodies, forces):
            ax = fx / body.mass
            ay = fy / body.mass
            body.velocity[0] += ax * dt
            body.velocity[1] += ay * dt
        for body in self.bodies:
            body.position[0] += body.velocity[0] * dt
            body.position[1] += body.velocity[1] * dt
        self._handle_collisions()
        if self.trails_enabled:
            for body in self.bodies:
                body.trail.append((body.position[0], body.position[1]))
                if len(body.trail) > self.max_trail_length:
                    del body.trail[0 : len(body.trail) - self.max_trail_length]
        else:
            for body in self.bodies:
                body.trail.clear()

    # Collision handling ---------------------------------------------
    def _handle_collisions(self) -> None:
        i = 0
        while i < len(self.bodies):
            body = self.bodies[i]
            j = i + 1
            while j < len(self.bodies):
                other = self.bodies[j]
                dx = other.position[0] - body.position[0]
                dy = other.position[1] - body.position[1]
                distance = math.hypot(dx, dy)
                if distance <= body.radius + other.radius:
                    body = self._merge_bodies(body, other)
                    self.bodies[i] = body
                    self.bodies.pop(j)
                else:
                    j += 1
            i += 1

    def _merge_bodies(self, a: Body, b: Body) -> Body:
        total_mass = a.mass + b.mass
        if total_mass <= 0:
            raise ValueError("Cannot merge bodies with non-positive total mass")
        position = [
            (a.position[0] * a.mass + b.position[0] * b.mass) / total_mass,
            (a.position[1] * a.mass + b.position[1] * b.mass) / total_mass,
        ]
        velocity = [
            (a.velocity[0] * a.mass + b.velocity[0] * b.mass) / total_mass,
            (a.velocity[1] * a.mass + b.velocity[1] * b.mass) / total_mass,
        ]
        color = tuple(
            int((a.color[idx] * a.mass + b.color[idx] * b.mass) / total_mass)
            for idx in range(3)
        )
        merged = Body(
            mass=total_mass, position=position, velocity=velocity, color=color
        )
        merged.trail = (a.trail + b.trail)[-self.max_trail_length :]
        return merged

    # Convenience operations -----------------------------------------
    def reset(self) -> None:
        self.bodies.clear()

    def toggle_trails(self) -> None:
        self.trails_enabled = not self.trails_enabled
        if not self.trails_enabled:
            for body in self.bodies:
                body.trail.clear()


__all__ = ["Body", "NBodySimulation", "radius_from_mass"]
