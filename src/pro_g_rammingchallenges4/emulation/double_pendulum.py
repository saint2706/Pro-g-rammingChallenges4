"""Double pendulum simulation utilities.

This module provides a Runge-Kutta 4 integrator for the classical
non-linear double pendulum, along with helpers for visualization and
exporting simulation data.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import cos, sin, radians
from pathlib import Path
from typing import Iterable, List, Sequence, Tuple
import argparse
import csv

@dataclass(frozen=True)
class DoublePendulumConfig:
    """Physical configuration values for the double pendulum.

    Attributes:
        m1: Mass of the first pendulum bob.
        m2: Mass of the second pendulum bob.
        l1: Length of the first pendulum rod.
        l2: Length of the second pendulum rod.
        g: Acceleration due to gravity.
        damping: Damping factor applied to both joints.
    """

    m1: float = 1.0
    m2: float = 1.0
    l1: float = 1.0
    l2: float = 1.0
    g: float = 9.81
    damping: float = 0.0


@dataclass
class SimulationResult:
    """Container for simulation traces.

    Attributes:
        time: List of time points.
        theta1: List of angles for the first pendulum.
        theta2: List of angles for the second pendulum.
        omega1: List of angular velocities for the first pendulum.
        omega2: List of angular velocities for the second pendulum.
    """

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
        """Initializes the simulator with a given configuration.

        Args:
            config: The physical configuration for the pendulum.
        """
        self.config = config or DoublePendulumConfig()

    def _derivatives(self, state: Sequence[float]) -> Tuple[float, float, float, float]:
        """Calculates the derivatives of the state variables.

        Args:
            state: A sequence of [theta1, omega1, theta2, omega2].

        Returns:
            A tuple of the derivatives (d(theta1)/dt, d(omega1)/dt, etc.).
        """
        theta1, omega1, theta2, omega2 = state
        cfg = self.config
        delta = theta2 - theta1

        m1, m2, l1, l2, g, damping = cfg.m1, cfg.m2, cfg.l1, cfg.l2, cfg.g, cfg.damping

        # Equations of motion for the double pendulum.
        denom1 = (m1 + m2) * l1 - m2 * l1 * cos(delta) ** 2
        denom2 = (l2 / l1) * denom1

        domega1 = (
            m2 * l1 * omega1**2 * sin(delta) * cos(delta)
            + m2 * g * sin(theta2) * cos(delta)
            + m2 * l2 * omega2**2 * sin(delta)
            - (m1 + m2) * g * sin(theta1)
        ) / denom1

        domega2 = (
            -m2 * l2 * omega2**2 * sin(delta) * cos(delta)
            + (m1 + m2)
            * (
                g * sin(theta1) * cos(delta)
                - l1 * omega1**2 * sin(delta)
                - g * sin(theta2)
            )
        ) / denom2

        if damping:
            domega1 -= damping * omega1
            domega2 -= damping * omega2

        return (omega1, domega1, omega2, domega2)

    def _rk4_step(
        self, state: Sequence[float], dt: float
    ) -> Tuple[float, float, float, float]:
        """Performs a single step of the Runge-Kutta 4th order integration.

        Args:
            state: The current state of the system.
            dt: The time step.

        Returns:
            The new state after the time step.
        """

        def combine(
            s: Sequence[float], scale: float, deriv: Sequence[float]
        ) -> Tuple[float, float, float, float]:
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
        """Runs the simulation for a given duration.

        Args:
            duration: The total time to simulate.
            time_step: The time step for the integration.
            initial_state: The initial state of the pendulum.

        Returns:
            A SimulationResult object containing the simulation data.
        """
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

    def positions(
        self, theta1: float, theta2: float
    ) -> Tuple[float, float, float, float]:
        """Computes the Cartesian coordinates of the two pendulum bobs.

        Args:
            theta1: The angle of the first pendulum.
            theta2: The angle of the second pendulum.

        Returns:
            A tuple of (x1, y1, x2, y2) coordinates.
        """
        cfg = self.config
        x1 = cfg.l1 * sin(theta1)
        y1 = -cfg.l1 * cos(theta1)
        x2 = x1 + cfg.l2 * sin(theta2)
        y2 = y1 - cfg.l2 * cos(theta2)
        return x1, y1, x2, y2

    def trajectory(
        self, result: SimulationResult
    ) -> Tuple[List[float], List[float], List[float], List[float]]:
        """Computes the trajectory of the pendulum bobs.

        Args:
            result: The simulation result.

        Returns:
            A tuple of lists (x1, y1, x2, y2) for the trajectory.
        """
        x1, y1, x2, y2 = [], [], [], []
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
    """Computes the total mechanical energy of the system.

    Args:
        cfg: The pendulum configuration.
        theta1, omega1, theta2, omega2: The state variables.

    Returns:
        The total energy of the system.
    """
    ke1 = 0.5 * cfg.m1 * (cfg.l1 * omega1) ** 2
    v1x = cfg.l1 * omega1 * cos(theta1)
    v1y = cfg.l1 * omega1 * sin(theta1)
    v2x = v1x + cfg.l2 * omega2 * cos(theta2)
    v2y = v1y + cfg.l2 * omega2 * sin(theta2)
    ke2 = 0.5 * cfg.m2 * (v2x**2 + v2y**2)
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
    """Exports the simulation results to a CSV file.

    Args:
        result: The simulation data.
        path: The path to save the CSV file to.
        include_energy: Whether to include an energy column.
        cfg: The pendulum configuration.
    """
    cfg = cfg or DoublePendulumConfig()
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as handle:
        fieldnames = ["time", "theta1", "theta2", "omega1", "omega2"]
        if include_energy:
            fieldnames.append("energy")
        writer = csv.DictWriter(handle, fieldnames=fieldnames)
        writer.writeheader()
        for t, th1, th2, om1, om2 in result.as_rows():
            row = {"time": t, "theta1": th1, "theta2": th2, "omega1": om1, "omega2": om2}
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
    """Renders the pendulum's trajectory using Matplotlib.

    Args:
        simulator: The simulator instance.
        result: The simulation data.
        show: Whether to display the plot interactively.
        save_path: Optional path to save the plot as an image.
        trail_length: The number of points to show in the trajectory trail.
    """
    # Import necessary libraries for plotting.
    try:
        import matplotlib.pyplot as plt
    except ImportError:
        print("Matplotlib is required for rendering.")
        return

    save_path_obj: Path | None = Path(save_path) if save_path else None
    if save_path_obj:
        save_path_obj.parent.mkdir(parents=True, exist_ok=True)

    if not show and not save_path_obj:
        return

    x1, y1, x2, y2 = simulator.trajectory(result)

    fig, ax = plt.subplots(figsize=(6, 6))
    ax.set_aspect("equal")

    limit = simulator.config.l1 + simulator.config.l2 + 0.2
    ax.set_xlim(-limit, limit)
    ax.set_ylim(-limit, limit)

    if show:
        # Animate the pendulum if in interactive mode.
        history_x, history_y = [], []
        for idx in range(len(result.time)):
            ax.clear()
            ax.set_xlim(-limit, limit)
            ax.set_ylim(-limit, limit)
            ax.set_title(f"Double Pendulum t={result.time[idx]:.2f}s")
            ax.grid(True, linestyle="--", alpha=0.3)

            history_x.append(x2[idx])
            history_y.append(y2[idx])
            if len(history_x) > trail_length:
                history_x = history_x[-trail_length:]
                history_y = history_y[-trail_length:]

            ax.plot([0, x1[idx]], [0, y1[idx]], "o-", color="tab:blue")
            ax.plot([x1[idx], x2[idx]], [y1[idx], y2[idx]], "o-", color="tab:orange")
            ax.plot(history_x, history_y, color="tab:red", linewidth=1.0, alpha=0.7)
            plt.pause(0.001)
        if save_path_obj:
            fig.savefig(save_path_obj)
        plt.show()
    else:
        # Static render for headless mode.
        ax.set_title("Double Pendulum (final state)")
        ax.plot([0, x1[-1]], [0, y1[-1]], "o-", color="tab:blue")
        ax.plot([x1[-1], x2[-1]], [y1[-1], y2[-1]], "o-", color="tab:orange")
        if save_path_obj:
            fig.savefig(save_path_obj)
        plt.close(fig)


def export_gif(
    simulator: DoublePendulumSimulator,
    result: SimulationResult,
    path: str | Path,
    *,
    fps: int = 30,
    trail_length: int = 120,
) -> None:
    """Exports the simulation as an animated GIF.

    Args:
        simulator: The simulator instance.
        result: The simulation data.
        path: The path to save the GIF to.
        fps: The frames per second of the animation.
        trail_length: The length of the trajectory trail.
    """
    try:
        import imageio.v2 as imageio
        import matplotlib.pyplot as plt
        import numpy as np
    except ImportError:
        print("imageio and Matplotlib are required for GIF export.")
        return

    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    x1, y1, x2, y2 = simulator.trajectory(result)

    fig, ax = plt.subplots(figsize=(4, 4))
    frames = []
    history_x, history_y = [], []

    for idx in range(len(result.time)):
        ax.clear()
        limit = simulator.config.l1 + simulator.config.l2 + 0.2
        ax.set_xlim(-limit, limit)
        ax.set_ylim(-limit, limit)
        ax.set_axis_off()

        history_x.append(x2[idx])
        history_y.append(y2[idx])
        if len(history_x) > trail_length:
            history_x = history_x[-trail_length:]
            history_y = history_y[-trail_length:]

        ax.plot([0, x1[idx]], [0, y1[idx]], "o-", color="tab:blue", linewidth=2)
        ax.plot([x1[idx], x2[idx]], [y1[idx], y2[idx]], "o-", color="tab:orange", linewidth=2)
        ax.plot(history_x, history_y, color="tab:red", linewidth=1.0, alpha=0.7)

        fig.canvas.draw()
        width, height = fig.canvas.get_width_height()
        buffer = np.frombuffer(fig.canvas.tostring_rgb(), dtype=np.uint8)
        frame = buffer.reshape((height, width, 3))
        frames.append(frame)

    plt.close(fig)
    imageio.mimsave(path, frames, fps=fps)


def _cli() -> None:
    """Command-line interface for the double pendulum simulator."""
    parser = argparse.ArgumentParser(description="Simulate a chaotic double pendulum")
    parser.add_argument(
        "--theta1", type=float, default=120.0, help="Initial angle of the first link (degrees)."
    )
    parser.add_argument(
        "--theta2", type=float, default=-10.0, help="Initial angle of the second link (degrees)."
    )
    parser.add_argument(
        "--omega1", type=float, default=0.0, help="Initial angular velocity of the first link (rad/s)."
    )
    parser.add_argument(
        "--omega2", type=float, default=0.0, help="Initial angular velocity of the second link (rad/s)."
    )
    parser.add_argument(
        "--duration", type=float, default=10.0, help="Simulation duration (seconds)."
    )
    parser.add_argument("--dt", type=float, default=0.01, help="Integration time step.")
    parser.add_argument(
        "--damping", type=float, default=0.0, help="Damping factor for both joints."
    )
    parser.add_argument("--csv", type=Path, help="Path to save simulation data as a CSV file.")
    parser.add_argument("--gif", type=Path, help="Path to save the animation as a GIF.")
    parser.add_argument(
        "--no-show", action="store_true", help="Do not show the Matplotlib preview window."
    )
    parser.add_argument(
        "--trail", type=int, default=180, help="Number of points in the trajectory trail."
    )
    args = parser.parse_args()

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
        print(f"Simulation CSV written to {args.csv}")

    if args.gif:
        export_gif(simulator, result, args.gif, trail_length=args.trail)
        print(f"Animation written to {args.gif}")

    if not args.no_show:
        render_matplotlib(simulator, result, trail_length=args.trail)
