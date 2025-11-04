"""Utilities to visualize and analyze crawler output."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import (
    Any,
    Dict,
    Iterable,
    List,
    Mapping,
    Optional,
    Sequence,
    Tuple,
    Union,
    Protocol,
)

import networkx as nx
import plotly.graph_objects as go
from plotly import io as pio
from plotly.colors import sample_colorscale

# Type aliases
Edge = Tuple[str, str]


@dataclass(frozen=True)
class CrawlGraphMetadata:
    """Container describing derived graph information."""

    root: str
    nodes: List[Dict[str, Any]]
    links: List[Dict[str, str]]
    graph: Dict[str, Any]


def build_graph_from_source(
    source: Union["CrawlerLike", Sequence[Sequence[str]], Mapping[str, Any], str, Path],
    *,
    root_url: Optional[str] = None,
) -> Tuple[nx.DiGraph, CrawlGraphMetadata]:
    """Create a graph + metadata from a crawler or serialized payload.

    Parameters
    ----------
    source:
        A :class:`WebCrawler` instance, a sequence of ``(src, dst)`` edges, a mapping
        containing serialized crawl data, or a path to a JSON file.
    root_url:
        Optional fallback for the crawl root. Required when ``source`` does not expose
        a ``start_url``/``root`` value.
    """

    edges, root, errors = _normalize_source(source, root_url=root_url)
    graph = nx.DiGraph()
    graph.add_edges_from(edges)
    if root not in graph:
        graph.add_node(root)
    metadata = _build_metadata(graph, root, errors, edges)
    return graph, metadata


def build_plotly_figure(
    graph: nx.DiGraph,
    metadata: CrawlGraphMetadata,
    *,
    layout: str = "spring",
    colorscale: str = "Viridis",
    seed: int = 42,
) -> go.Figure:
    """Render a Plotly figure for the crawl graph using the supplied metadata."""

    if layout == "spring":
        positions = nx.spring_layout(graph, seed=seed)
    elif layout == "kamada_kawai":
        positions = nx.kamada_kawai_layout(graph)
    elif layout == "shell":
        positions = nx.shell_layout(graph)
    else:
        raise ValueError(f"Unsupported layout '{layout}'")

    edge_x: List[float] = []
    edge_y: List[float] = []
    for src, dst in graph.edges():
        src_pos = positions.get(src)
        dst_pos = positions.get(dst)
        if src_pos is None or dst_pos is None:
            continue
        edge_x.extend([src_pos[0], dst_pos[0], None])
        edge_y.extend([src_pos[1], dst_pos[1], None])

    node_x: List[float] = []
    node_y: List[float] = []
    node_colors: List[str] = []
    node_sizes: List[float] = []
    node_lines: List[str] = []
    node_hover: List[str] = []

    max_depth = metadata.graph.get("max_depth", 0) or 0
    safe_max_depth = max_depth if max_depth > 0 else 1
    for item in metadata.nodes:
        node_id = item["id"]
        pos = positions.get(node_id)
        if pos is None:
            continue
        node_x.append(pos[0])
        node_y.append(pos[1])
        depth = item["depth"]
        if depth is None or depth < 0:
            node_colors.append("#bcbcbc")
        else:
            ratio = min(max(depth / safe_max_depth, 0.0), 1.0)
            node_colors.append(sample_colorscale(colorscale, [ratio])[0])
        node_sizes.append(18.0 if item.get("has_error") else 12.0)
        node_lines.append("crimson" if item.get("has_error") else "#2f2f2f")
        hover_lines = [f"URL: {node_id}"]
        depth_text = "unknown" if depth is None else str(depth)
        hover_lines.append(f"Depth: {depth_text}")
        hover_lines.append(f"Out degree: {item['out_degree']}")
        hover_lines.append(f"In degree: {item['in_degree']}")
        if item.get("error"):
            hover_lines.append(f"Error: {item['error']}")
        node_hover.append("<br>".join(hover_lines))

    edge_trace = go.Scatter(
        x=edge_x,
        y=edge_y,
        line=dict(width=1, color="#888"),
        hoverinfo="none",
        mode="lines",
    )

    node_trace = go.Scatter(
        x=node_x,
        y=node_y,
        mode="markers",
        hoverinfo="text",
        text=node_hover,
        marker=dict(
            color=node_colors,
            size=node_sizes,
            line=dict(width=2, color=node_lines),
        ),
    )

    figure = go.Figure(data=[edge_trace, node_trace])
    figure.update_layout(
        title=metadata.graph.get("title", "Crawl graph"),
        showlegend=False,
        hovermode="closest",
        margin=dict(l=30, r=30, t=50, b=30),
    )
    return figure


def export_html(figure: go.Figure, destination: Union[str, Path]) -> Path:
    """Write the figure to an HTML file and return the resolved path."""

    output_path = Path(destination).expanduser().resolve()
    output_path.parent.mkdir(parents=True, exist_ok=True)
    pio.write_html(figure, str(output_path), include_plotlyjs="cdn", auto_open=False)
    return output_path


def _normalize_source(
    source: Union["CrawlerLike", Sequence[Sequence[str]], Mapping[str, Any], str, Path],
    *,
    root_url: Optional[str],
) -> Tuple[List[Edge], str, Dict[str, str]]:
    if _looks_like_crawler(source):
        crawler = source  # type: ignore[assignment]
        edges = _normalize_edges(getattr(crawler, "edges", []))
        cfg = getattr(crawler, "cfg", None)
        start_url = getattr(cfg, "start_url", None)
        root = start_url or root_url
        if root is None:
            raise ValueError(
                "Crawler configuration must expose a start_url or provide root_url"
            )
        raw_errors = getattr(crawler, "errors", {})
        errors = {
            k: v
            for k, v in raw_errors.items()
            if isinstance(k, str) and isinstance(v, str)
        }
        return edges, root, errors

    if isinstance(source, (str, Path)):
        payload = json.loads(Path(source).expanduser().read_text(encoding="utf-8"))
    else:
        payload = source

    if isinstance(payload, Mapping):
        edges_data = payload.get("edges") or payload.get("links") or []
        edges = _normalize_edges(edges_data)
        root = payload.get("start_url") or payload.get("root") or root_url
        if root is None:
            raise ValueError(
                "Serialized crawl data requires a 'start_url'/'root' or root_url"
            )
        raw_errors = payload.get("errors", {})
        errors = {
            str(k): str(v)
            for k, v in raw_errors.items()
            if isinstance(k, str) and isinstance(v, str)
        }
        return edges, root, errors

    edges = _normalize_edges(payload) if isinstance(payload, Sequence) else []
    if root_url is None:
        raise ValueError("Edges-only payloads require root_url")
    return edges, root_url, {}


def _normalize_edges(edges: Iterable[Sequence[str]]) -> List[Edge]:
    normalized: List[Edge] = []
    seen = set()
    for edge in edges:
        if (
            isinstance(edge, (list, tuple))
            and len(edge) == 2
            and isinstance(edge[0], str)
            and isinstance(edge[1], str)
        ):
            pair = (edge[0], edge[1])
            if pair in seen:
                continue
            seen.add(pair)
            normalized.append(pair)
    return normalized


def _build_metadata(
    graph: nx.DiGraph,
    root: str,
    errors: Mapping[str, str],
    edges: Sequence[Edge],
) -> CrawlGraphMetadata:
    try:
        depth_map = nx.single_source_shortest_path_length(graph, root)
    except nx.NetworkXError:
        try:
            depth_map = nx.single_source_shortest_path_length(
                graph.to_undirected(), root
            )
        except nx.NetworkXError:
            depth_map = {root: 0}

    ordered_nodes = sorted(graph.nodes())
    ordered_nodes.sort(key=lambda node: (0 if node == root else 1, node))

    nodes: List[Dict[str, Any]] = []
    max_depth = 0
    error_nodes = 0
    for node in ordered_nodes:
        depth = depth_map.get(node)
        max_depth = max(max_depth, depth or 0)
        error_info = errors.get(node)
        if error_info:
            error_nodes += 1
        nodes.append(
            {
                "id": node,
                "depth": depth if depth is not None else None,
                "in_degree": int(graph.in_degree(node)),
                "out_degree": int(graph.out_degree(node)),
                "error": error_info,
                "has_error": bool(error_info),
            }
        )

    link_dicts = [{"source": src, "target": dst} for src, dst in edges]

    metadata = CrawlGraphMetadata(
        root=root,
        nodes=nodes,
        links=link_dicts,
        graph={
            "node_count": graph.number_of_nodes(),
            "edge_count": graph.number_of_edges(),
            "max_depth": max_depth,
            "error_count": error_nodes,
            "title": f"Crawl graph for {root}",
        },
    )
    return metadata


def _looks_like_crawler(obj: Any) -> bool:
    return hasattr(obj, "edges") and hasattr(obj, "cfg")


class CrawlerLike(Protocol):  # type: ignore[misc]
    edges: Sequence[Edge]
    errors: Mapping[str, str]
    cfg: Any
