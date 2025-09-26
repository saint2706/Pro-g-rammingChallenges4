"""Interactive Bellman–Ford algorithm visualizer and CLI walkthrough.

Features
--------
* Loads weighted directed graphs from JSON or CSV (>=5 vertices).
* Precomputes every relaxation step and the final negative-cycle check.
* Matplotlib GUI displays node/edge updates with interactive controls.
* Headless mode mirrors the step log in the terminal for remote use.
* Optional export of results (JSON/CSV) for grading or reporting.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import sys
import textwrap
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, List, Optional, Sequence, Tuple

import matplotlib.pyplot as plt
from matplotlib.patches import Circle, FancyArrowPatch
from matplotlib.widgets import Button


@dataclass(frozen=True)
class Edge:
    """Directed edge in the graph."""

    source: str
    target: str
    weight: float


@dataclass
class Step:
    """Represents a single state transition in the Bellman–Ford process."""

    index: int
    phase: str  # "init", "relax", "check", "summary"
    iteration: int
    edge: Optional[Edge]
    updated: bool
    negative_cycle: bool
    distances: Dict[str, float]
    predecessors: Dict[str, Optional[str]]
    note: str


class BellmanFordSimulation:
    """Compute Bellman–Ford steps for visualization or CLI playback."""

    def __init__(self, edges: Sequence[Edge], start: str) -> None:
        if not edges:
            raise ValueError("Graph must contain at least one edge.")

        nodes = sorted({edge.source for edge in edges} | {edge.target for edge in edges})
        if len(nodes) < 5:
            raise ValueError("Bellman–Ford challenge requires at least five vertices.")
        if start not in nodes:
            raise ValueError(f"Start vertex '{start}' is not present in the graph.")

        self.edges: List[Edge] = list(edges)
        self.nodes: List[str] = nodes
        self.start: str = start
        self.steps: List[Step] = []
        self.negative_cycle_edges: List[Edge] = []
        self.final_distances: Dict[str, float] = {}
        self.predecessors: Dict[str, Optional[str]] = {}
        self.negative_cycle_detected: bool = False
        self._prepare_steps()

    # ------------------------------------------------------------------
    # Bellman–Ford core
    # ------------------------------------------------------------------
    def _prepare_steps(self) -> None:
        distances: Dict[str, float] = {node: math.inf for node in self.nodes}
        predecessors: Dict[str, Optional[str]] = {node: None for node in self.nodes}
        distances[self.start] = 0.0

        self.steps.append(
            Step(
                index=len(self.steps),
                phase="init",
                iteration=0,
                edge=None,
                updated=False,
                negative_cycle=False,
                distances=dict(distances),
                predecessors=dict(predecessors),
                note=f"Initialization complete. Start vertex '{self.start}' distance set to 0.",
            )
        )

        vertex_count = len(self.nodes)
        early_stop = False
        for iteration in range(1, vertex_count):
            any_update = False
            for edge in self.edges:
                relaxed, note = self._relax_edge(distances, predecessors, edge)
                if relaxed:
                    any_update = True
                self.steps.append(
                    Step(
                        index=len(self.steps),
                        phase="relax",
                        iteration=iteration,
                        edge=edge,
                        updated=relaxed,
                        negative_cycle=False,
                        distances=dict(distances),
                        predecessors=dict(predecessors),
                        note=note,
                    )
                )
            if not any_update:
                self.steps.append(
                    Step(
                        index=len(self.steps),
                        phase="relax",
                        iteration=iteration,
                        edge=None,
                        updated=False,
                        negative_cycle=False,
                        distances=dict(distances),
                        predecessors=dict(predecessors),
                        note=(
                            "No updates during this iteration; algorithm has converged "
                            "and can terminate early."
                        ),
                    )
                )
                early_stop = True
                break

        check_iteration = vertex_count if not early_stop else iteration
        for edge in self.edges:
            indicates_cycle = self._detect_negative_cycle(distances, edge)
            note = (
                "Negative cycle detected via edge "
                f"{edge.source}->{edge.target} (weight {edge.weight})."
                if indicates_cycle
                else (
                    "No further improvement possible from edge "
                    f"{edge.source}->{edge.target}; distances are stable."
                )
            )
            if indicates_cycle:
                self.negative_cycle_edges.append(edge)
            self.steps.append(
                Step(
                    index=len(self.steps),
                    phase="check",
                    iteration=check_iteration,
                    edge=edge,
                    updated=indicates_cycle,
                    negative_cycle=indicates_cycle,
                    distances=dict(distances),
                    predecessors=dict(predecessors),
                    note=note,
                )
            )

        self.negative_cycle_detected = bool(self.negative_cycle_edges)
        self.final_distances = dict(distances)
        self.predecessors = dict(predecessors)

        summary_note = (
            "Negative cycle reachable from the source. Distances that keep decreasing "
            "are highlighted in the log."
            if self.negative_cycle_detected
            else "All distances finalized. No negative cycles reachable from the source."
        )
        self.steps.append(
            Step(
                index=len(self.steps),
                phase="summary",
                iteration=check_iteration,
                edge=None,
                updated=False,
                negative_cycle=self.negative_cycle_detected,
                distances=dict(distances),
                predecessors=dict(predecessors),
                note=summary_note,
            )
        )

    def _relax_edge(
        self,
        distances: Dict[str, float],
        predecessors: Dict[str, Optional[str]],
        edge: Edge,
    ) -> Tuple[bool, str]:
        src_distance = distances[edge.source]
        dest_distance = distances[edge.target]
        if math.isinf(src_distance):
            note = (
                f"Skipping edge {edge.source}->{edge.target}: source distance is ∞, "
                "so relaxation is not possible yet."
            )
            return False, note

        candidate = src_distance + edge.weight
        if candidate < dest_distance:
            distances[edge.target] = candidate
            predecessors[edge.target] = edge.source
            note = (
                f"Updated distance of {edge.target}: {dest_distance if not math.isinf(dest_distance) else '∞'} → "
                f"{candidate:.2f} via {edge.source}."
            )
            return True, note
        else:
            note = (
                f"No update for {edge.target}; current distance {self._fmt_distance(dest_distance)} "
                f"is better than candidate {candidate:.2f}."
            )
            return False, note

    @staticmethod
    def _detect_negative_cycle(distances: Dict[str, float], edge: Edge) -> bool:
        src_distance = distances[edge.source]
        dest_distance = distances[edge.target]
        if math.isinf(src_distance):
            return False
        return src_distance + edge.weight < dest_distance

    # ------------------------------------------------------------------
    # Export helpers
    # ------------------------------------------------------------------
    def export_results(self, output_path: Path) -> None:
        output_path.parent.mkdir(parents=True, exist_ok=True)
        suffix = output_path.suffix.lower()
        if suffix == ".json":
            self._export_json(output_path)
        elif suffix == ".csv":
            self._export_csv(output_path)
        else:
            raise ValueError("Export path must end with .json or .csv")

    def _export_json(self, output_path: Path) -> None:
        distances = {
            node: (None if math.isinf(distance) else round(distance, 6))
            for node, distance in self.final_distances.items()
        }
        data = {
            "start": self.start,
            "distances": distances,
            "predecessors": self.predecessors,
            "negative_cycle": self.negative_cycle_detected,
            "negative_cycle_edges": [edge.__dict__ for edge in self.negative_cycle_edges],
        }
        output_path.write_text(json.dumps(data, indent=2), encoding="utf-8")

    def _export_csv(self, output_path: Path) -> None:
        with output_path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.writer(handle)
            writer.writerow(["start", self.start])
            writer.writerow(["node", "distance", "predecessor"])
            for node in self.nodes:
                distance = "" if math.isinf(self.final_distances[node]) else f"{self.final_distances[node]:.6f}"
                predecessor = self.predecessors[node] or ""
                writer.writerow([node, distance, predecessor])
            writer.writerow([])
            writer.writerow(["negative_cycle", str(self.negative_cycle_detected)])
            if self.negative_cycle_edges:
                writer.writerow(["cycle_source", "cycle_target", "weight"])
                for edge in self.negative_cycle_edges:
                    writer.writerow([edge.source, edge.target, edge.weight])

    # ------------------------------------------------------------------
    # Utility formatting
    # ------------------------------------------------------------------
    @staticmethod
    def _fmt_distance(distance: float) -> str:
        if math.isinf(distance):
            return "∞"
        return f"{distance:.2f}"


class BellmanFordVisualizer:
    """Matplotlib figure that animates Bellman–Ford steps with controls."""

    def __init__(self, simulation: BellmanFordSimulation, auto_interval: float = 1.0) -> None:
        self.simulation = simulation
        self.auto_interval = auto_interval
        self.current_index = 0
        self.playing = False

        self.fig, self.ax = plt.subplots(figsize=(10, 7))
        plt.subplots_adjust(bottom=0.22, right=0.68)
        self.ax.set_title("Bellman–Ford Simulation")
        self.ax.axis("off")

        self.positions = generate_layout(self.simulation.nodes)
        self.node_artists: Dict[str, Circle] = {}
        self.node_labels: Dict[str, plt.Text] = {}
        self.edge_patches: Dict[Tuple[str, str], FancyArrowPatch] = {}
        self.edge_labels: Dict[Tuple[str, str], plt.Text] = {}

        self._draw_graph()
        self._init_ui()
        self.update_display()

    # UI setup ---------------------------------------------------------
    def _draw_graph(self) -> None:
        for edge in self.simulation.edges:
            self._add_edge(edge)
        for node in self.simulation.nodes:
            self._add_node(node)

    def _add_node(self, node: str) -> None:
        x, y = self.positions[node]
        circle = Circle((x, y), 0.35, facecolor="#bbbbbb", edgecolor="black", linewidth=1.5, zorder=3)
        self.ax.add_patch(circle)
        label = self.ax.text(
            x,
            y,
            f"{node}\n∞",
            ha="center",
            va="center",
            color="black",
            fontsize=11,
            fontweight="bold",
            zorder=4,
        )
        self.node_artists[node] = circle
        self.node_labels[node] = label

    def _add_edge(self, edge: Edge) -> None:
        start = self.positions[edge.source]
        end = self.positions[edge.target]
        arrow = FancyArrowPatch(
            posA=start,
            posB=end,
            arrowstyle="-|>",
            mutation_scale=15,
            linewidth=1.8,
            color="#888888",
            zorder=1,
        )
        self.ax.add_patch(arrow)
        mid_x = start[0] + 0.6 * (end[0] - start[0])
        mid_y = start[1] + 0.6 * (end[1] - start[1])
        label = self.ax.text(
            mid_x,
            mid_y,
            f"{edge.weight}",
            fontsize=9,
            ha="center",
            va="center",
            backgroundcolor="white",
            color="#444444",
            zorder=5,
        )
        self.edge_patches[(edge.source, edge.target)] = arrow
        self.edge_labels[(edge.source, edge.target)] = label

    def _init_ui(self) -> None:
        ax_prev = self.fig.add_axes([0.1, 0.05, 0.12, 0.07])
        ax_next = self.fig.add_axes([0.24, 0.05, 0.12, 0.07])
        ax_play = self.fig.add_axes([0.38, 0.05, 0.12, 0.07])
        ax_reset = self.fig.add_axes([0.52, 0.05, 0.12, 0.07])

        self.btn_prev = Button(ax_prev, "Prev")
        self.btn_next = Button(ax_next, "Next")
        self.btn_play = Button(ax_play, "Play")
        self.btn_reset = Button(ax_reset, "Reset")

        self.btn_prev.on_clicked(lambda _: self.prev_step())
        self.btn_next.on_clicked(lambda _: self.next_step())
        self.btn_play.on_clicked(lambda _: self.toggle_play())
        self.btn_reset.on_clicked(lambda _: self.reset())

        self.fig.canvas.mpl_connect("key_press_event", self._on_key)

        self.distance_text = self.fig.text(
            0.72,
            0.75,
            "",
            va="top",
            ha="left",
            fontfamily="monospace",
            fontsize=11,
        )
        self.status_text = self.fig.text(
            0.72,
            0.35,
            "",
            va="top",
            ha="left",
            wrap=True,
            fontsize=10,
        )

        self.timer = self.fig.canvas.new_timer(interval=int(self.auto_interval * 1000))
        self.timer.add_callback(self.next_step)

    # Interaction ------------------------------------------------------
    def _on_key(self, event) -> None:  # type: ignore[override]
        if event.key == "left":
            self.prev_step()
        elif event.key == "right":
            self.next_step()
        elif event.key == " ":
            self.toggle_play()

    def prev_step(self) -> None:
        self.pause()
        if self.current_index > 0:
            self.current_index -= 1
            self.update_display()

    def next_step(self) -> None:
        if self.current_index < len(self.simulation.steps) - 1:
            self.current_index += 1
            self.update_display()
        else:
            self.pause()

    def toggle_play(self) -> None:
        if self.playing:
            self.pause()
        else:
            self.play()

    def play(self) -> None:
        if self.current_index >= len(self.simulation.steps) - 1:
            self.current_index = 0
        self.playing = True
        self.btn_play.label.set_text("Pause")
        self.timer.start()

    def pause(self) -> None:
        if self.playing:
            self.timer.stop()
            self.playing = False
            self.btn_play.label.set_text("Play")

    def reset(self) -> None:
        self.pause()
        self.current_index = 0
        self.update_display()

    # Rendering --------------------------------------------------------
    def update_display(self) -> None:
        step = self.simulation.steps[self.current_index]

        for key, arrow in self.edge_patches.items():
            arrow.set_color("#888888")
            arrow.set_linewidth(1.8)
            arrow.set_zorder(1)
            label = self.edge_labels[key]
            label.set_color("#444444")

        if step.edge:
            key = (step.edge.source, step.edge.target)
            if key in self.edge_patches:
                color = "#d62728" if step.updated else "#ff7f0e"
                if step.negative_cycle:
                    color = "#9467bd"
                arrow = self.edge_patches[key]
                arrow.set_color(color)
                arrow.set_linewidth(3.0)
                arrow.set_zorder(4)
                self.edge_labels[key].set_color(color)

        for node, circle in self.node_artists.items():
            distance = step.distances[node]
            if math.isinf(distance):
                circle.set_facecolor("#dddddd")
                text_color = "black"
            else:
                circle.set_facecolor("#1f77b4")
                text_color = "white"
            if step.edge and node == step.edge.target and step.updated:
                circle.set_facecolor("#2ca02c")
                text_color = "white"
            label = self.node_labels[node]
            label.set_color(text_color)
            label.set_text(f"{node}\n{BellmanFordSimulation._fmt_distance(distance)}")

        iteration_label = (
            f"Iteration {step.iteration}" if step.phase in {"relax", "check"} else "Initialization"
        )
        phase_label = step.phase.capitalize()
        header = f"Step {self.current_index + 1}/{len(self.simulation.steps)} — {phase_label} ({iteration_label})"
        self.status_text.set_text(f"{header}\n\n" + textwrap.fill(step.note, 40))

        distance_lines = ["Node  Dist   Pred"]
        for node in self.simulation.nodes:
            dist = BellmanFordSimulation._fmt_distance(step.distances[node])
            pred = step.predecessors[node] or "-"
            distance_lines.append(f"{node:>4}  {dist:>5}  {pred:>4}")
        self.distance_text.set_text("\n".join(distance_lines))

        self.fig.canvas.draw_idle()

    def show(self) -> None:
        plt.show()


# ----------------------------------------------------------------------
# CLI helpers
# ----------------------------------------------------------------------

def load_graph(path: Path, fmt: Optional[str] = None) -> List[Edge]:
    if not path.exists():
        raise FileNotFoundError(f"Graph file '{path}' does not exist.")
    format_hint = fmt or path.suffix.lower().lstrip(".")
    if format_hint == "json":
        return load_json_graph(path)
    if format_hint == "csv":
        return load_csv_graph(path)
    raise ValueError("Graph format could not be inferred. Use --format json|csv.")


def load_json_graph(path: Path) -> List[Edge]:
    data = json.loads(path.read_text(encoding="utf-8"))
    edges_raw = data.get("edges")
    if not edges_raw:
        raise ValueError("JSON graph must include an 'edges' array.")
    edges: List[Edge] = []
    for idx, edge in enumerate(edges_raw, start=1):
        try:
            source = str(edge["source"])
            target = str(edge["target"])
            weight = float(edge["weight"])
        except (KeyError, TypeError, ValueError) as exc:
            raise ValueError(f"Invalid edge entry at index {idx}: {edge}") from exc
        if source == target:
            continue
        edges.append(Edge(source=source, target=target, weight=weight))
    return edges


def load_csv_graph(path: Path) -> List[Edge]:
    edges: List[Edge] = []
    with path.open("r", newline="", encoding="utf-8") as handle:
        reader = csv.DictReader(handle)
        required = {"source", "target", "weight"}
        if not required.issubset(reader.fieldnames or {}):
            raise ValueError("CSV graph must contain source,target,weight columns.")
        for idx, row in enumerate(reader, start=1):
            source = row["source"].strip()
            target = row["target"].strip()
            if not source or not target or source == target:
                continue
            try:
                weight = float(row["weight"])
            except (TypeError, ValueError) as exc:
                raise ValueError(f"Invalid weight on row {idx}: {row['weight']}") from exc
            edges.append(Edge(source=source, target=target, weight=weight))
    if not edges:
        raise ValueError("CSV graph did not contain any usable edges.")
    return edges


def run_cli_stepper(sim: BellmanFordSimulation) -> None:
    print("Bellman–Ford simulation (headless mode).")
    print("Commands: Enter = next, b = back, q = quit.\n")
    index = 0
    total = len(sim.steps)
    while True:
        step = sim.steps[index]
        print("=" * 60)
        print(
            f"Step {index + 1}/{total} | Phase: {step.phase} | Iteration: {step.iteration}"
        )
        if step.edge:
            print(
                f"Edge: {step.edge.source} -> {step.edge.target} (w={step.edge.weight})"
            )
        print(textwrap.fill(step.note, 60))
        print("Distances:")
        for node in sim.nodes:
            dist = BellmanFordSimulation._fmt_distance(step.distances[node])
            pred = step.predecessors[node] or "-"
            print(f"  {node:>3}: {dist:>6} (pred: {pred})")

        command = input("[Enter] next | b back | q quit > ").strip().lower()
        if command == "q":
            break
        if command == "b":
            if index > 0:
                index -= 1
            continue
        if index < total - 1:
            index += 1
        else:
            print("Reached the final step.")


def generate_layout(nodes: Sequence[str]) -> Dict[str, Tuple[float, float]]:
    radius = 4.0
    positions: Dict[str, Tuple[float, float]] = {}
    for i, node in enumerate(nodes):
        angle = 2 * math.pi * i / len(nodes)
        x = radius * math.cos(angle)
        y = radius * math.sin(angle)
        positions[node] = (x, y)
    return positions


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Bellman–Ford algorithm simulator with visualization and export options.",
    )
    parser.add_argument(
        "--input",
        type=Path,
        help="Path to graph file (JSON or CSV). Defaults to the bundled sample graph.",
    )
    parser.add_argument(
        "--format",
        choices=["json", "csv"],
        help="Explicitly set the graph format when the extension is ambiguous.",
    )
    parser.add_argument(
        "--start",
        help="Start vertex for Bellman–Ford. Defaults to the first vertex alphabetically.",
    )
    parser.add_argument(
        "--export",
        type=Path,
        help="Optional path to export final distances/predecessors (JSON or CSV).",
    )
    parser.add_argument(
        "--no-gui",
        action="store_true",
        help="Run in headless CLI mode (useful for remote shells).",
    )
    parser.add_argument(
        "--auto-interval",
        type=float,
        default=1.0,
        help="Seconds between automatic steps when playing the animation (default: 1.0).",
    )
    return parser.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = parse_args(argv)
    default_graph = Path(__file__).with_name("sample_graph.json")
    graph_path = args.input or default_graph

    try:
        edges = load_graph(graph_path, args.format)
    except Exception as exc:  # noqa: BLE001 - surface clean message to CLI
        print(f"Error loading graph: {exc}", file=sys.stderr)
        return 1

    nodes = sorted({edge.source for edge in edges} | {edge.target for edge in edges})
    start = args.start or (nodes[0] if nodes else None)
    if start is None:
        print("Graph does not contain any vertices.", file=sys.stderr)
        return 1

    try:
        simulation = BellmanFordSimulation(edges, start)
    except Exception as exc:  # noqa: BLE001 - user facing error
        print(f"Error preparing simulation: {exc}", file=sys.stderr)
        return 1

    if args.export:
        try:
            simulation.export_results(args.export)
            print(f"Exported results to {args.export}")
        except Exception as exc:  # noqa: BLE001
            print(f"Failed to export results: {exc}", file=sys.stderr)
            return 1

    if args.no_gui:
        run_cli_stepper(simulation)
    else:
        visualizer = BellmanFordVisualizer(simulation, auto_interval=args.auto_interval)
        visualizer.show()
    return 0


if __name__ == "__main__":  # pragma: no cover - manual invocation entry point
    raise SystemExit(main())
