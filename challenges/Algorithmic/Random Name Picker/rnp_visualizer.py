"""Random Name Picker visualizer.

Generates normalized probability weights from ``names.txt`` style inputs and
simulates repeated draws to compare theoretical weights to empirical
frequencies. Optionally renders matplotlib pie/bar charts for quick sanity
checks and can emit JSON for automated tests.
"""

from __future__ import annotations

import argparse
import importlib
import json
import math
import os
import random
import sys
from dataclasses import dataclass
from typing import Dict, Iterable, List, Optional, Sequence

import rnp


@dataclass(slots=True)
class SimulationConfig:
    path: str
    count: int
    trials: int
    with_replacement: bool
    seed: Optional[int]
    json_output: bool
    json_indent: int
    no_show: bool
    save: Optional[str]

    def validate(self, total_names: int) -> None:
        if self.count <= 0:
            raise ValueError("Count must be positive")
        if self.trials <= 0:
            raise ValueError("Trials must be positive")
        if not self.with_replacement and self.count > total_names:
            raise ValueError(
                f"Cannot pick {self.count} unique names from {total_names}"
            )


@dataclass(slots=True)
class SimulationResult:
    names: List[str]
    weights: Optional[List[float]]
    normalized_weights: List[float]
    with_replacement: bool
    count_per_trial: int
    trials: int
    seed: Optional[int]
    sample_counts: Dict[str, int]
    sample_probabilities: List[float]

    def to_json(self, indent: int = 2) -> str:
        payload = {
            "names": self.names,
            "weights": self.weights,
            "normalized_weights": self.normalized_weights,
            "with_replacement": self.with_replacement,
            "count_per_trial": self.count_per_trial,
            "trials": self.trials,
            "seed": self.seed,
            "sample_counts": self.sample_counts,
            "sample_probabilities": self.sample_probabilities,
        }
        return json.dumps(payload, indent=indent)


def compute_normalized_weights(
    names: Sequence[str], weights: Optional[Sequence[float]]
) -> List[float]:
    if weights is None:
        weights = [1.0] * len(names)
    total = float(sum(weights))
    if not math.isfinite(total) or total <= 0:
        raise ValueError("Total weight must be a positive finite number")
    return [float(w) / total for w in weights]


def simulate_draws(
    names: Sequence[str],
    *,
    weights: Optional[Sequence[float]],
    count: int,
    trials: int,
    with_replacement: bool,
    rng: random.Random,
) -> Dict[str, int]:
    counts: Dict[str, int] = {name: 0 for name in names}
    for _ in range(trials):
        picks = rnp.pick_names(
            names,
            count,
            with_replacement=with_replacement,
            weights=weights,
            rng=rng,
        )
        for pick in picks:
            counts[pick] += 1
    return counts


def summarize(
    *,
    names: List[str],
    weights: Optional[List[float]],
    normalized: List[float],
    counts: Dict[str, int],
    config: SimulationConfig,
) -> SimulationResult:
    total_draws = config.count * config.trials
    sample_probabilities = [counts[name] / total_draws for name in names]
    return SimulationResult(
        names=names,
        weights=weights,
        normalized_weights=normalized,
        with_replacement=config.with_replacement,
        count_per_trial=config.count,
        trials=config.trials,
        seed=config.seed,
        sample_counts=counts,
        sample_probabilities=sample_probabilities,
    )


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Visualize random name picker probabilities and simulations",
    )
    parser.add_argument("-f", "--file", default="names.txt", help="Path to names file")
    parser.add_argument(
        "-c",
        "--count",
        type=int,
        default=1,
        help="Number of names drawn per trial",
    )
    parser.add_argument(
        "-t",
        "--trials",
        type=int,
        default=500,
        help="Number of simulation trials to run",
    )
    parser.add_argument(
        "--with-replacement",
        action="store_true",
        help="Sample with replacement (allows repeats)",
    )
    parser.add_argument("--seed", type=int, help="Seed RNG for deterministic runs")
    parser.add_argument(
        "--json",
        action="store_true",
        help="Emit JSON summary (useful for automated checks)",
    )
    parser.add_argument(
        "--json-indent",
        type=int,
        default=2,
        help="Indentation level for JSON output",
    )
    parser.add_argument(
        "--no-show",
        action="store_true",
        help="Skip displaying the matplotlib window (still saves if --save)",
    )
    parser.add_argument(
        "--save",
        help="Optional path to save the generated visualization (PNG, SVG, ...)",
    )
    return parser


def load_pyplot(headless: bool):
    try:
        matplotlib = importlib.import_module("matplotlib")
        if headless and "matplotlib.pyplot" not in sys.modules:
            matplotlib.use("Agg")  # type: ignore[attr-defined]
        return importlib.import_module("matplotlib.pyplot")
    except ModuleNotFoundError as exc:  # pragma: no cover - optional dependency
        raise RuntimeError(
            "matplotlib is required for plotting. Install it or pass --json/--no-show."
        ) from exc


def render_visualization(
    summary: SimulationResult,
    *,
    save_path: Optional[str],
    show: bool,
) -> None:
    plt = load_pyplot(headless=not show and not save_path)
    fig, axes = plt.subplots(1, 2, figsize=(12, 6))

    # Pie chart for theoretical probabilities
    axes[0].pie(
        summary.normalized_weights,
        labels=summary.names,
        autopct="%1.1f%%",
        startangle=90,
    )
    axes[0].set_title("Normalized Selection Probabilities")

    # Bar chart for empirical results
    sample_probs = summary.sample_probabilities
    indices = range(len(summary.names))
    axes[1].bar(indices, sample_probs, label="Simulated", alpha=0.7)
    axes[1].plot(
        indices,
        summary.normalized_weights,
        color="black",
        linestyle="--",
        marker="o",
        label="Normalized Weight",
    )
    axes[1].set_xticks(list(indices))
    axes[1].set_xticklabels(summary.names, rotation=45, ha="right")
    axes[1].set_ylim(0, max(max(sample_probs), max(summary.normalized_weights)) * 1.15)
    axes[1].set_ylabel("Probability")
    axes[1].set_title("Empirical vs. Theoretical")
    axes[1].legend()
    fig.tight_layout()

    if save_path:
        fig.savefig(save_path, bbox_inches="tight")
    if show:
        plt.show()
    else:
        plt.close(fig)


def run_simulation(config: SimulationConfig) -> SimulationResult:
    names, weights = rnp.parse_names_file(config.path)
    config.validate(len(names))
    normalized = compute_normalized_weights(names, weights)
    rng = random.Random(config.seed)
    counts = simulate_draws(
        names,
        weights=weights,
        count=config.count,
        trials=config.trials,
        with_replacement=config.with_replacement,
        rng=rng,
    )
    return summarize(
        names=names,
        weights=list(weights) if weights is not None else None,
        normalized=normalized,
        counts=counts,
        config=config,
    )


def main(argv: Optional[Iterable[str]] = None) -> int:
    parser = build_arg_parser()
    args = parser.parse_args(list(argv) if argv is not None else None)
    cfg = SimulationConfig(
        path=args.file,
        count=args.count,
        trials=args.trials,
        with_replacement=args.with_replacement,
        seed=args.seed,
        json_output=args.json,
        json_indent=args.json_indent,
        no_show=args.no_show,
        save=args.save,
    )

    if not os.path.exists(cfg.path):
        print(f"Error: name file '{cfg.path}' not found", file=sys.stderr)
        return 1

    try:
        summary = run_simulation(cfg)
    except Exception as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1

    if cfg.json_output:
        print(summary.to_json(indent=cfg.json_indent))

    if not cfg.no_show or cfg.save:
        try:
            render_visualization(summary, save_path=cfg.save, show=not cfg.no_show)
        except RuntimeError as exc:
            print(f"Warning: {exc}", file=sys.stderr)
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    raise SystemExit(main())
