"""Interactive Streamlit application for the Verlet cloth simulation."""

from __future__ import annotations

import io
import json
from typing import List

import numpy as np
import plotly.graph_objects as go
import streamlit as st

from cloth import simulate_cloth


def render() -> None:
    """Render the interactive cloth simulation controls."""

    st.set_page_config(
        page_title="Verlet Cloth Playground", page_icon="🧵", layout="wide"
    )
    st.title("Verlet Cloth Playground")
    st.markdown(
        """
    Experiment with the cloth simulation parameters and preview the resulting
    animation directly in the browser.  You can also download the generated frames
    for offline processing or visualisation.
    """
    )

    with st.sidebar:
        st.header("Simulation Parameters")
        num_x = st.slider("Grid columns", min_value=4, max_value=60, value=30)
        num_y = st.slider("Grid rows", min_value=4, max_value=60, value=20)
        gravity_strength = st.slider(
            "Gravity (m/s²)", min_value=0.0, max_value=25.0, value=9.81
        )
        constraint_iterations = st.slider(
            "Constraint iterations", min_value=1, max_value=20, value=5
        )
        steps = st.slider("Simulation steps", min_value=1, max_value=300, value=120)
        timestep = st.slider(
            "Timestep (dt)", min_value=0.005, max_value=0.05, value=0.016, step=0.001
        )
        pinned_rows = st.slider(
            "Pinned rows", min_value=0, max_value=num_y - 1, value=1
        )
        st.markdown("Use the controls above to tailor the simulation to your needs.")

    frames = run_simulation(
        num_x=num_x,
        num_y=num_y,
        gravity_strength=gravity_strength,
        constraint_iterations=constraint_iterations,
        steps=steps,
        timestep=timestep,
        pinned_rows=pinned_rows,
    )

    frame_index = st.slider(
        "Frame to display", min_value=0, max_value=len(frames) - 1, value=0
    )
    current_frame = frames[frame_index]
    positions = np.array(current_frame.get("positions", []))
    segments = np.array(current_frame.get("segments", []))

    line_x: List[float] = []
    line_y: List[float] = []
    for segment in segments:
        line_x.extend([segment[0, 0], segment[1, 0], None])
        line_y.extend([segment[0, 1], segment[1, 1], None])

    fig = go.Figure()
    if line_x:
        fig.add_trace(
            go.Scatter(
                x=line_x,
                y=line_y,
                mode="lines",
                line=dict(color="#424242", width=1),
                name="Constraints",
                hoverinfo="skip",
            )
        )

    if len(positions) > 0:
        fig.add_trace(
            go.Scatter(
                x=positions[:, 0],
                y=positions[:, 1],
                mode="markers",
                marker=dict(color="#1976d2", size=6),
                name="Particles",
            )
        )

    fig.update_layout(
        width=None,
        height=650,
        showlegend=False,
        margin=dict(l=10, r=10, t=30, b=10),
    )
    fig.update_xaxes(title_text="X position", showgrid=False, zeroline=False)
    fig.update_yaxes(
        title_text="Y position",
        showgrid=False,
        zeroline=False,
        scaleanchor="x",
        scaleratio=1,
    )

    st.plotly_chart(fig, use_container_width=True)

    st.markdown(
        f"Showing frame **{frame_index}** / **{len(frames) - 1}** — time = ``{current_frame['time']:.3f}s``"
    )

    json_bytes = serialise_frames_to_json(frames)
    gif_bytes = frames_to_gif(frames)

    col_json, col_gif = st.columns(2)
    with col_json:
        st.download_button(
            label="Download frames as JSON",
            data=json_bytes,
            file_name="verlet_cloth_frames.json",
            mime="application/json",
        )
    with col_gif:
        st.download_button(
            label="Download animation as GIF",
            data=gif_bytes,
            file_name="verlet_cloth_animation.gif",
            mime="image/gif",
        )


@st.cache_data(show_spinner=False)
def run_simulation(
    *,
    num_x: int,
    num_y: int,
    gravity_strength: float,
    constraint_iterations: int,
    steps: int,
    timestep: float,
    pinned_rows: int,
) -> List[dict]:
    frames = simulate_cloth(
        steps,
        dt=timestep,
        include_initial_state=True,
        record_positions=True,
        record_segments=True,
        width=2.0,
        height=1.5,
        num_x=num_x,
        num_y=num_y,
        pinned_rows=pinned_rows,
        gravity=(0.0, gravity_strength),
        wind=(0.0, 0.0),
        constraint_iterations=constraint_iterations,
    )
    return frames


def serialise_frames_to_json(frames: List[dict]) -> bytes:
    serialisable = []
    for frame in frames:
        frame_data = {"step": frame["step"], "time": frame["time"]}
        if "positions" in frame:
            frame_data["positions"] = np.asarray(frame["positions"]).tolist()
        if "segments" in frame:
            frame_data["segments"] = np.asarray(frame["segments"]).tolist()
        serialisable.append(frame_data)
    return json.dumps(serialisable, indent=2).encode("utf-8")


def frames_to_gif(frames: List[dict], duration: float = 0.08) -> bytes:
    import imageio.v2 as imageio
    import matplotlib.pyplot as plt

    all_positions = [
        np.asarray(frame.get("positions", []))
        for frame in frames
        if frame.get("positions") is not None and len(frame.get("positions", [])) > 0
    ]
    if not all_positions:
        raise ValueError("No position data available to render GIF")

    concatenated = np.concatenate(all_positions, axis=0)
    x_min, y_min = concatenated.min(axis=0)
    x_max, y_max = concatenated.max(axis=0)
    x_margin = (x_max - x_min) * 0.1 or 0.5
    y_margin = (y_max - y_min) * 0.1 or 0.5

    images = []
    for frame in frames:
        pts = np.asarray(frame.get("positions", []))
        segs = np.asarray(frame.get("segments", []))

        fig, ax = plt.subplots(figsize=(4, 4))
        if len(segs):
            for seg in segs:
                ax.plot(seg[:, 0], seg[:, 1], color="#616161", linewidth=1)
        if len(pts):
            ax.scatter(pts[:, 0], pts[:, 1], color="#1976d2", s=12)

        ax.set_xlim(x_min - x_margin, x_max + x_margin)
        ax.set_ylim(y_min - y_margin, y_max + y_margin)
        ax.set_aspect("equal")
        ax.axis("off")

        buffer = io.BytesIO()
        fig.savefig(buffer, format="png", bbox_inches="tight", pad_inches=0.05)
        plt.close(fig)
        buffer.seek(0)
        images.append(imageio.imread(buffer))

    output = io.BytesIO()
    imageio.mimsave(output, images, format="GIF", duration=duration)
    output.seek(0)
    return output.read()


__all__ = ["render"]


if __name__ == "__main__":  # pragma: no cover - manual execution helper
    render()
