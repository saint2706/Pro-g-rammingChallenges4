"""Interactive matplotlib visualization for the Verlet cloth simulation."""

from __future__ import annotations

import argparse
from typing import Tuple

import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
from matplotlib.backend_bases import KeyEvent
from matplotlib.collections import LineCollection

from cloth import Cloth


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Animate a Verlet cloth simulation")
    parser.add_argument("--width", type=float, default=2.0, help="Width of the cloth in world units")
    parser.add_argument("--height", type=float, default=1.5, help="Height of the cloth in world units")
    parser.add_argument("--cols", type=int, default=30, help="Number of particles along the x-axis")
    parser.add_argument("--rows", type=int, default=20, help="Number of particles along the y-axis")
    parser.add_argument("--iterations", type=int, default=5, help="Constraint solver iterations per frame")
    parser.add_argument("--dt", type=float, default=0.016, help="Simulation timestep per frame in seconds")
    return parser.parse_args()


def configure_axes(ax: plt.Axes, width: float, height: float) -> None:
    ax.set_xlim(0, width)
    ax.set_ylim(height, 0)  # invert y-axis (origin at top-left)
    ax.set_aspect("equal")
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_title("Verlet Cloth Simulation")


def init_plot(ax: plt.Axes, cloth: Cloth) -> Tuple[LineCollection, plt.Line2D, plt.Text]:
    segments = cloth.segments
    collection = LineCollection(segments, colors="tab:blue", linewidths=1.0)
    ax.add_collection(collection)

    scatter = ax.scatter(cloth.positions[:, 0], cloth.positions[:, 1], s=10, c="tab:red")

    info = ax.text(
        0.02,
        0.98,
        "",
        transform=ax.transAxes,
        verticalalignment="top",
        fontsize=9,
        family="monospace",
        bbox=dict(facecolor="white", alpha=0.7, edgecolor="none"),
    )
    return collection, scatter, info


def update_info(info: plt.Text, cloth: Cloth) -> None:
    status_lines = [
        f"Gravity: {'ON ' if cloth.gravity_enabled else 'OFF'}",
        f"Wind:    {'ON ' if cloth.wind_enabled else 'OFF'}",
        "Keys: [g] gravity  [w] wind  [r] reset  [q] quit",
    ]
    info.set_text("\n".join(status_lines))


def main() -> None:
    args = parse_args()
    cloth = Cloth(
        width=args.width,
        height=args.height,
        num_x=args.cols,
        num_y=args.rows,
        constraint_iterations=args.iterations,
    )

    fig, ax = plt.subplots(figsize=(8, 6))
    configure_axes(ax, args.width, args.height)
    collection, scatter, info = init_plot(ax, cloth)
    update_info(info, cloth)

    paused = {"value": False}

    def on_key(event: KeyEvent) -> None:
        if event.key == "g":
            cloth.toggle_gravity()
            update_info(info, cloth)
        elif event.key == "w":
            cloth.toggle_wind()
            update_info(info, cloth)
        elif event.key == "r":
            cloth.reset()
            update_info(info, cloth)
        elif event.key == "p":
            paused["value"] = not paused["value"]
        elif event.key == "q":
            plt.close(fig)

    fig.canvas.mpl_connect("key_press_event", on_key)

    def animate(_frame: int) -> Tuple[LineCollection, plt.Line2D]:
        if not paused["value"]:
            cloth.step(args.dt)
        segments = cloth.segments
        collection.set_segments(segments)
        scatter.set_offsets(cloth.positions)
        return collection, scatter

    FuncAnimation(fig, animate, interval=int(args.dt * 1000), blit=False)
    plt.show()


if __name__ == "__main__":
    main()
