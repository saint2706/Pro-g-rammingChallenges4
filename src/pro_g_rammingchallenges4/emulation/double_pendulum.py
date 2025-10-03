"""Double pendulum simulation utilities.

This module provides a Runge--Kutta 4 integrator for the classical
non-linear double pendulum along with helpers for visualisation and
exporting simulation data.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import cos, sin
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple


@dataclass(frozen=True)
class DoublePendulumConfig:
    """Physical configuration values for the double pendulum."""

    m1: float = 1.0
    m2: float = 1.0
    l1: float = 1.0
    l2: float = 1.0
    g: float = 9.81
    damping: float = 0.0


@dataclass
class SimulationResult:
    """Container for simulation traces."""

    time: List[float]
    theta1: List[float]
    theta2: List[float]
    omega1: List[float]
    omega2: List[float]

    def as_rows(self) -> Iterable[Tuple[float, float, float, float, float]]:
        """Iterate over the recorded samples as CSV-friendly rows."""

        for idx in range(len(self.time)):
            yield (
                self.time[idx],
                self.theta1[idx],
                self.theta2[idx],
                self.omega1[idx],
                self.omega2[idx],
            )


class DoublePendulumSimulator:
    """Integrate the equations of motion for a double pendulum."""

    def __init__(self, config: DoublePendulumConfig | None = None) -> None:
        self.config = config or DoublePendulumConfig()

    def _derivatives(self, state: Sequence[float]) -> Tuple[float, float, float, float]:
        theta1, omega1, theta2, omega2 = state
        cfg = self.config
        delta = theta2 - theta1

        m1 = cfg.m1
        m2 = cfg.m2
        l1 = cfg.l1
        l2 = cfg.l2
        g = cfg.g
        damping = cfg.damping

        denom1 = (m1 + m2) * l1 - m2 * l1 * cos(delta) * cos(delta)
        denom2 = (l2 / l1) * denom1

        domega1 = (
            m2 * l1 * omega1 * omega1 * sin(delta) * cos(delta)
            + m2 * g * sin(theta2) * cos(delta)
            + m2 * l2 * omega2 * omega2 * sin(delta)
            - (m1 + m2) * g * sin(theta1)
        ) / denom1

        domega2 = (
            -m2 * l2 * omega2 * omega2 * sin(delta) * cos(delta)
            + (m1 + m2)
            * (
                g * sin(theta1) * cos(delta)
                - l1 * omega1 * omega1 * sin(delta)
                - g * sin(theta2)
            )
        ) / denom2

        if damping:
            domega1 -= damping * omega1
            domega2 -= damping * omega2

        return (omega1, domega1, omega2, domega2)

    def _rk4_step(self, state: Sequence[float], dt: float) -> Tuple[float, float, float, float]:
        def combine(s: Sequence[float], scale: float, deriv: Sequence[float]) -> Tuple[float, float, float, float]:
            return (
                s[0] + scale * deriv[0],
                s[1] + scale * deriv[1],
                s[2] + scale * deriv[2],
                s[3] + scale * deriv[3],
            )

        k1 = self._derivatives(state)
        k2 = self._derivatives(combine(state, dt / 2.0, k1))
        k3 = self._derivatives(combine(state, dt / 2.0, k2))
        k4 = self._derivatives(combine(state, dt, k3))

        return (
            state[0] + dt * (k1[0] + 2 * k2[0] + 2 * k3[0] + k4[0]) / 6.0,
            state[1] + dt * (k1[1] + 2 * k2[1] + 2 * k3[1] + k4[1]) / 6.0,
            state[2] + dt * (k1[2] + 2 * k2[2] + 2 * k3[2] + k4[2]) / 6.0,
            state[3] + dt * (k1[3] + 2 * k2[3] + 2 * k3[3] + k4[3]) / 6.0,
        )

    def simulate(
        self,
        duration: float,
        time_step: float,
        initial_state: Sequence[float],
    ) -> SimulationResult:
        """Run the integrator for ``duration`` seconds."""

        steps = int(round(duration / time_step))
        time: List[float] = [0.0]
        theta1: List[float] = [initial_state[0]]
        theta2: List[float] = [initial_state[2]]
        omega1: List[float] = [initial_state[1]]
        omega2: List[float] = [initial_state[3]]

        state = tuple(initial_state)
        for step in range(1, steps + 1):
            state = self._rk4_step(state, time_step)
            time.append(step * time_step)
            theta1.append(state[0])
            omega1.append(state[1])
            theta2.append(state[2])
            omega2.append(state[3])

        return SimulationResult(time, theta1, theta2, omega1, omega2)

    def positions(self, theta1: float, theta2: float) -> Tuple[float, float, float, float]:
        """Compute cartesian coordinates of the two bobs."""

        cfg = self.config
        x1 = cfg.l1 * sin(theta1)
        y1 = -cfg.l1 * cos(theta1)
        x2 = x1 + cfg.l2 * sin(theta2)
        y2 = y1 - cfg.l2 * cos(theta2)
        return x1, y1, x2, y2

    def trajectory(self, result: SimulationResult) -> Tuple[List[float], List[float], List[float], List[float]]:
        """Return x/y coordinates for both links across the trace."""

        x1: List[float] = []
        y1: List[float] = []
        x2: List[float] = []
        y2: List[float] = []
        for th1, th2 in zip(result.theta1, result.theta2):
            px1, py1, px2, py2 = self.positions(th1, th2)
            x1.append(px1)
            y1.append(py1)
            x2.append(px2)
            y2.append(py2)
        return x1, y1, x2, y2


def compute_total_energy(
    cfg: DoublePendulumConfig,
    theta1: float,
    omega1: float,
    theta2: float,
    omega2: float,
) -> float:
    """Return the mechanical energy for the given state."""

    # Kinetic energy of the first mass.
    ke1 = 0.5 * cfg.m1 * (cfg.l1 * omega1) ** 2

    # Velocities of the second mass derived from the joint velocities.
    v1x = cfg.l1 * omega1 * cos(theta1)
    v1y = cfg.l1 * omega1 * sin(theta1)
    v2x = v1x + cfg.l2 * omega2 * cos(theta2)
    v2y = v1y + cfg.l2 * omega2 * sin(theta2)
    ke2 = 0.5 * cfg.m2 * (v2x * v2x + v2y * v2y)

    # Potential energy taking the pivot as zero height.
    y1 = -cfg.l1 * cos(theta1)
    y2 = y1 - cfg.l2 * cos(theta2)
    pe = cfg.m1 * cfg.g * y1 + cfg.m2 * cfg.g * y2

    return ke1 + ke2 + pe


def export_csv(
    result: SimulationResult,
    path: str | Path,
    include_energy: bool = True,
    cfg: DoublePendulumConfig | None = None,
) -> None:
    """Write the trace to ``path`` in CSV format."""

    import csv

    cfg = cfg or DoublePendulumConfig()
    with Path(path).open("w", newline="", encoding="utf-8") as handle:
        fieldnames = ["time", "theta1", "theta2", "omega1", "omega2"]
        if include_energy:
            fieldnames.append("energy")
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for t, th1, th2, om1, om2 in result.as_rows():
            row = {
                "time": t,
                "theta1": th1,
                "theta2": th2,
                "omega1": om1,
                "omega2": om2,
            }
            if include_energy:
                row["energy"] = compute_total_energy(cfg, th1, om1, th2, om2)
            writer.writerow(row)


def render_matplotlib(
    simulator: DoublePendulumSimulator,
    result: SimulationResult,
    *,
    show: bool = True,
    save_path: str | Path | None = None,
    trail_length: int = 120,
) -> None:
    """Render the trajectory with matplotlib."""

    import matplotlib.pyplot as plt

    if not show and save_path is None:
        return

    x1, y1, x2, y2 = simulator.trajectory(result)

    fig, ax = plt.subplots(figsize=(6, 6))
    ax.set_aspect("equal")

    history_x: List[float] = []
    history_y: List[float] = []

    if show:
        for idx in range(len(result.time)):
            ax.clear()
            ax.set_xlim(-simulator.config.l1 - simulator.config.l2 - 0.2, simulator.config.l1 + simulator.config.l2 + 0.2)
            ax.set_ylim(-simulator.config.l1 - simulator.config.l2 - 0.2, simulator.config.l1 + simulator.config.l2 + 0.2)
            ax.set_title(f"Double Pendulum t={result.time[idx]:.2f}s")
            ax.grid(True, linestyle="--", alpha=0.3)

            history_x.append(x2[idx])
            history_y.append(y2[idx])
            if len(history_x) > trail_length:
                history_x = history_x[-trail_length:]
                history_y = history_y[-trail_length:]

            ax.plot([0.0, x1[idx]], [0.0, y1[idx]], color="tab:blue")
            ax.plot([x1[idx], x2[idx]], [y1[idx], y2[idx]], color="tab:orange")
            ax.scatter([x1[idx], x2[idx]], [y1[idx], y2[idx]], color=["tab:blue", "tab:orange"], s=50)
            ax.plot(history_x, history_y, color="tab:red", linewidth=1.0, alpha=0.7)
            plt.pause(0.001)

        if save_path is not None:
            fig.savefig(save_path)
        plt.show()
        return

    # Static render for headless mode with optional save.
    idx = -1
    ax.set_xlim(-simulator.config.l1 - simulator.config.l2 - 0.2, simulator.config.l1 + simulator.config.l2 + 0.2)
    ax.set_ylim(-simulator.config.l1 - simulator.config.l2 - 0.2, simulator.config.l1 + simulator.config.l2 + 0.2)
    ax.grid(True, linestyle="--", alpha=0.3)
    ax.set_title("Double Pendulum (final state)")

    history_x.extend(x2[max(0, len(x2) - trail_length):])
    history_y.extend(y2[max(0, len(y2) - trail_length):])

    ax.plot([0.0, x1[idx]], [0.0, y1[idx]], color="tab:blue")
    ax.plot([x1[idx], x2[idx]], [y1[idx], y2[idx]], color="tab:orange")
    ax.scatter([x1[idx], x2[idx]], [y1[idx], y2[idx]], color=["tab:blue", "tab:orange"], s=50)
    ax.plot(history_x, history_y, color="tab:red", linewidth=1.0, alpha=0.7)

    if save_path is not None:
        fig.savefig(save_path)

    plt.close(fig)


def export_gif(
    simulator: DoublePendulumSimulator,
    result: SimulationResult,
    path: str | Path,
    *,
    fps: int = 30,
    trail_length: int = 120,
) -> None:
    """Export the motion as an animated GIF."""

    import imageio.v2 as imageio
    import matplotlib.pyplot as plt
    import numpy as np

    path = Path(path)
    x1, y1, x2, y2 = simulator.trajectory(result)

    fig, ax = plt.subplots(figsize=(4, 4))
    frames: List = []
    history_x: List[float] = []
    history_y: List[float] = []

    for idx in range(len(result.time)):
        ax.clear()
        ax.set_xlim(-simulator.config.l1 - simulator.config.l2 - 0.2, simulator.config.l1 + simulator.config.l2 + 0.2)
        ax.set_ylim(-simulator.config.l1 - simulator.config.l2 - 0.2, simulator.config.l1 + simulator.config.l2 + 0.2)
        ax.set_axis_off()

        history_x.append(x2[idx])
        history_y.append(y2[idx])
        if len(history_x) > trail_length:
            history_x = history_x[-trail_length:]
            history_y = history_y[-trail_length:]

        ax.plot([0.0, x1[idx]], [0.0, y1[idx]], color="tab:blue", linewidth=2)
        ax.plot([x1[idx], x2[idx]], [y1[idx], y2[idx]], color="tab:orange", linewidth=2)
        ax.scatter([x1[idx], x2[idx]], [y1[idx], y2[idx]], color=["tab:blue", "tab:orange"], s=40)
        ax.plot(history_x, history_y, color="tab:red", linewidth=1.0, alpha=0.7)

        fig.canvas.draw()
        width, height = fig.canvas.get_width_height()
        buffer = np.frombuffer(fig.canvas.tostring_rgb(), dtype=np.uint8)
        frame = buffer.reshape((height, width, 3))
        frames.append(frame)

    plt.close(fig)
    imageio.mimsave(path, frames, duration=1 / fps)


def _cli() -> None:
    """Entry-point used by ``python -m`` and the repository CLI."""

    import argparse

    parser = argparse.ArgumentParser(description="Simulate a chaotic double pendulum")
    parser.add_argument("--theta1", type=float, default=120.0, help="Initial angle of the first link in degrees")
    parser.add_argument("--theta2", type=float, default=-10.0, help="Initial angle of the second link in degrees")
    parser.add_argument("--omega1", type=float, default=0.0, help="Initial angular velocity of the first link (rad/s)")
    parser.add_argument("--omega2", type=float, default=0.0, help="Initial angular velocity of the second link (rad/s)")
    parser.add_argument("--duration", type=float, default=10.0, help="Simulation duration in seconds")
    parser.add_argument("--dt", type=float, default=0.01, help="Integration time step")
    parser.add_argument("--damping", type=float, default=0.0, help="Damping factor applied to both joints")
    parser.add_argument("--csv", type=Path, help="Optional CSV output path")
    parser.add_argument("--gif", type=Path, help="Optional GIF output path")
    parser.add_argument("--no-show", action="store_true", help="Skip the matplotlib preview window")
    parser.add_argument("--trail", type=int, default=180, help="Number of points to keep for the path trail")

    args = parser.parse_args()

    from math import radians

    cfg = DoublePendulumConfig(damping=args.damping)
    simulator = DoublePendulumSimulator(cfg)
    initial_state = (
        radians(args.theta1),
        args.omega1,
        radians(args.theta2),
        args.omega2,
    )

    result = simulator.simulate(args.duration, args.dt, initial_state)

    if args.csv:
        export_csv(result, args.csv, cfg=cfg)

    if args.gif:
        export_gif(simulator, result, args.gif, trail_length=args.trail)

    render_matplotlib(
        simulator,
        result,
        show=not args.no_show,
        save_path=None,
        trail_length=args.trail,
    )


if __name__ == "__main__":  # pragma: no cover
    _cli()
