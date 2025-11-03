"""Emulation utilities and numerical demos."""

from .double_pendulum import (
    DoublePendulumConfig,
    DoublePendulumSimulator,
    SimulationResult,
    compute_total_energy,
    export_csv,
    export_gif,
    render_matplotlib,
)

__all__ = [
    "DoublePendulumConfig",
    "DoublePendulumSimulator",
    "SimulationResult",
    "compute_total_energy",
    "export_csv",
    "export_gif",
    "render_matplotlib",
]
