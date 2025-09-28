"""Streamlit frontend for the Bellman–Ford simulation."""

from __future__ import annotations

import csv
import io
import json
import math
from pathlib import Path
from typing import Dict, Iterable, List, Sequence

import streamlit as st

from bellman_ford_simulation import (
    BellmanFordSimulation,
    Edge,
    Step,
    render_step,
)


def _parse_json_edges(text: str) -> List[Edge]:
    try:
        data = json.loads(text)
    except json.JSONDecodeError as exc:  # pragma: no cover - user feedback path
        raise ValueError(f"Invalid JSON: {exc}") from exc
    edges_raw = data.get("edges")
    if not edges_raw:
        raise ValueError("JSON graph must include an 'edges' array.")
    edges: List[Edge] = []
    for idx, edge in enumerate(edges_raw, start=1):
        try:
            source = str(edge["source"]).strip()
            target = str(edge["target"]).strip()
            weight = float(edge["weight"])
        except (KeyError, TypeError, ValueError) as exc:
            raise ValueError(f"Invalid edge entry at index {idx}: {edge}") from exc
        if source and target and source != target:
            edges.append(Edge(source=source, target=target, weight=weight))
    if not edges:
        raise ValueError("JSON graph did not contain any usable edges.")
    return edges


def _parse_csv_edges(text: str) -> List[Edge]:
    handle = io.StringIO(text)
    reader = csv.DictReader(handle)
    required = {"source", "target", "weight"}
    if not required.issubset(reader.fieldnames or {}):
        raise ValueError("CSV graph must contain source,target,weight columns.")
    edges: List[Edge] = []
    for idx, row in enumerate(reader, start=1):
        source = (row.get("source") or "").strip()
        target = (row.get("target") or "").strip()
        if not source or not target or source == target:
            continue
        try:
            weight = float(row["weight"])
        except (TypeError, ValueError) as exc:
            raise ValueError(
                f"Invalid weight on row {idx}: {row.get('weight')}"
            ) from exc
        edges.append(Edge(source=source, target=target, weight=weight))
    if not edges:
        raise ValueError("CSV graph did not contain any usable edges.")
    return edges


def _load_edges(uploaded_file) -> List[Edge]:
    if uploaded_file is None:
        sample_path = Path(__file__).with_name("sample_graph.json")
        return _parse_json_edges(sample_path.read_text(encoding="utf-8"))

    content = uploaded_file.getvalue().decode("utf-8")
    suffix = Path(uploaded_file.name).suffix.lower()
    if suffix == ".json":
        return _parse_json_edges(content)
    if suffix == ".csv":
        return _parse_csv_edges(content)
    raise ValueError("Unsupported file type. Please upload a JSON or CSV file.")


def _step_history_json(steps: Sequence[Step]) -> str:
    def encode_distance(value: float) -> float | None:
        return None if math.isinf(value) else round(value, 6)

    payload: List[Dict[str, object]] = []
    for step in steps:
        edge = (
            {
                "source": step.edge.source,
                "target": step.edge.target,
                "weight": step.edge.weight,
            }
            if step.edge
            else None
        )
        payload.append(
            {
                "index": step.index,
                "phase": step.phase,
                "iteration": step.iteration,
                "edge": edge,
                "updated": step.updated,
                "negative_cycle": step.negative_cycle,
                "distances": {
                    node: encode_distance(distance)
                    for node, distance in step.distances.items()
                },
                "predecessors": step.predecessors,
                "note": step.note,
            }
        )
    return json.dumps({"steps": payload}, indent=2)


def _step_history_csv(steps: Sequence[Step], nodes: Iterable[str]) -> str:
    buffer = io.StringIO()
    writer = csv.writer(buffer)
    node_list = list(nodes)
    header = [
        "index",
        "phase",
        "iteration",
        "edge_source",
        "edge_target",
        "edge_weight",
        "updated",
        "negative_cycle",
        "note",
    ]
    for node in node_list:
        header.extend([f"dist_{node}", f"pred_{node}"])
    writer.writerow(header)

    for step in steps:
        edge = step.edge
        row = [
            step.index,
            step.phase,
            step.iteration,
            edge.source if edge else "",
            edge.target if edge else "",
            edge.weight if edge else "",
            step.updated,
            step.negative_cycle,
            step.note,
        ]
        for node in node_list:
            distance = step.distances.get(node, math.inf)
            row.append("" if math.isinf(distance) else f"{distance:.6f}")
            row.append(step.predecessors.get(node) or "")
        writer.writerow(row)

    return buffer.getvalue()


def render() -> None:
    """Render the Bellman–Ford Streamlit interface."""

    st.set_page_config(page_title="Bellman–Ford Simulation", layout="wide")
    st.title("Bellman–Ford Simulation")
    st.write(
        "Upload a weighted directed graph (JSON or CSV) to explore the Bellman–Ford algorithm "
        "step-by-step. If no file is provided the bundled sample graph is used."
    )

    uploaded = st.file_uploader(
        "Graph file", type=["json", "csv"], accept_multiple_files=False
    )

    try:
        edges = _load_edges(uploaded)
    except Exception as exc:  # noqa: BLE001 - display friendly error
        st.error(str(exc))
        st.stop()

    nodes = sorted({edge.source for edge in edges} | {edge.target for edge in edges})
    if len(nodes) < 5:
        st.error(
            "Graph must contain at least five vertices for the simulation challenge."
        )
        st.stop()

    start_vertex = st.selectbox("Start vertex", options=nodes, index=0)

    try:
        simulation = BellmanFordSimulation(edges, start_vertex)
    except Exception as exc:  # noqa: BLE001 - display friendly error
        st.error(str(exc))
        st.stop()

    step_index = st.slider("Simulation step", 0, len(simulation.steps) - 1, 0)
    step = simulation.steps[step_index]

    fig = render_step(step, simulation.nodes, simulation.edges)
    st.pyplot(fig, use_container_width=True)

    iteration_label = (
        f"Iteration {step.iteration}"
        if step.phase in {"relax", "check"}
        else "Initialization"
    )
    st.subheader(
        f"Step {step.index + 1}/{len(simulation.steps)} — {step.phase.capitalize()} ({iteration_label})"
    )
    st.write(step.note)

    distance_lines = ["Node  Dist   Pred"]
    for node in simulation.nodes:
        dist = BellmanFordSimulation._fmt_distance(step.distances.get(node, math.inf))
        pred = step.predecessors.get(node) or "-"
        distance_lines.append(f"{node:>4}  {dist:>5}  {pred:>4}")
    st.code("\n".join(distance_lines))

    json_data = _step_history_json(simulation.steps)
    csv_data = _step_history_csv(simulation.steps, simulation.nodes)

    st.download_button(
        "Download step history (JSON)",
        data=json_data,
        file_name="bellman_ford_steps.json",
        mime="application/json",
    )
    st.download_button(
        "Download step history (CSV)",
        data=csv_data,
        file_name="bellman_ford_steps.csv",
        mime="text/csv",
    )


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover - manual execution helper
    render()
