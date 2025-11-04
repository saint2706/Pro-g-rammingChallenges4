"""Finds an Eulerian path or circuit in a graph using Hierholzer's algorithm.

This script provides a command-line interface for finding an Eulerian path or
circuit in a given graph. An Eulerian path visits every edge exactly once.
"""

from __future__ import annotations

from collections import Counter, defaultdict
from typing import Dict, Iterable, List, Optional, Set, Tuple
import argparse

AdjList = Dict[int, List[int]]


# ------------------------- Eulerian Helpers ------------------------- #


def _vertex_universe(adj_list: AdjList) -> Set[int]:
    """Returns the set of all vertices in the graph."""
    vertices: Set[int] = set(adj_list)
    for neighbours in adj_list.values():
        vertices.update(neighbours)
    return vertices


def degree_parity(adj_list: AdjList) -> Dict[int, int]:
    """Calculates the degree parity (0 for even, 1 for odd) for each vertex."""
    return {v: (len(adj_list[v]) % 2) for v in adj_list}


def is_connected_ignoring_isolated(adj_list: AdjList) -> bool:
    """Checks if the graph is connected, ignoring isolated vertices.

    Args:
        adj_list: The adjacency list of the graph.

    Returns:
        True if the graph is connected, False otherwise.
    """
    vertices = _vertex_universe(adj_list)
    non_isolated = [v for v in vertices if len(adj_list.get(v, [])) > 0]
    if not non_isolated:
        return True  # A graph with no edges is connected.

    start = non_isolated[0]
    visited = {start}
    stack = [start]
    while stack:
        u = stack.pop()
        for w in adj_list.get(u, []):
            if w not in visited:
                visited.add(w)
                stack.append(w)
    return all((len(adj_list.get(v, [])) == 0) or (v in visited) for v in vertices)


def classify_eulerian(adj_list: AdjList) -> str:
    """Classifies the graph as 'circuit', 'trail', or 'none'.

    Args:
        adj_list: The adjacency list of the graph.

    Returns:
        A string indicating the type of Eulerian path the graph has.
    """
    if not is_connected_ignoring_isolated(adj_list):
        return "none"
    vertices = _vertex_universe(adj_list)
    odd_vertices = [v for v in vertices if len(adj_list.get(v, [])) % 2 == 1]
    if len(odd_vertices) == 0:
        return "circuit"
    if len(odd_vertices) == 2:
        return "trail"
    return "none"


# ---------------------- Hierholzer Core Algorithm ------------------- #


def hierholzer(adj_list: AdjList, start: Optional[int] = None) -> List[int]:
    """Computes an Eulerian circuit or trail using Hierholzer's algorithm.

    Args:
        adj_list: The adjacency list of the graph.
        start: An optional starting vertex.

    Returns:
        A list of vertices representing the Eulerian path, or an empty list
        if no such path exists.
    """
    if not adj_list:
        return []

    vertices = _vertex_universe(adj_list)
    if not vertices:
        return []

    classification = classify_eulerian(adj_list)
    if classification == "none":
        return []

    # Create a copy of the adjacency list to modify during the algorithm.
    work: Dict[int, Counter[int]] = {u: Counter(neigh) for u, neigh in adj_list.items()}
    for v in vertices:
        work.setdefault(v, Counter())

    edge_count = sum(sum(counter.values()) for counter in work.values()) // 2
    if edge_count == 0:
        return []

    if start is None:
        # If no start vertex is specified, choose one based on the graph type.
        if classification == "trail":
            # Start at a vertex with an odd degree.
            for v in sorted(vertices):
                if len(adj_list.get(v, [])) % 2 == 1:
                    start = v
                    break
        else:
            # Start at any vertex for a circuit.
            start = min(vertices)
    if start is None or start not in vertices:
        return []

    path: List[int] = []
    stack: List[int] = [start]

    while stack:
        v = stack[-1]
        if work[v]:
            # Follow an edge to the next vertex.
            u = next(iter(work[v]))
            work[v][u] -= 1
            if work[v][u] == 0:
                del work[v][u]
            work[u][v] -= 1
            if work[u][v] == 0:
                del work[u][v]
            stack.append(u)
        else:
            # Backtrack and add the vertex to the path.
            path.append(stack.pop())

    path.reverse()
    if len(path) != edge_count + 1:
        return []  # Not all edges were visited.
    return path


# ------------------------------ CLI --------------------------------- #


def parse_edges(pairs: Iterable[str]) -> List[Tuple[int, int]]:
    """Parses a list of edge strings into a list of tuples."""
    edges: List[Tuple[int, int]] = []
    for token in pairs:
        if "-" in token:
            a, b = token.split("-", 1)
        elif "," in token:
            a, b = token.split(",", 1)
        else:
            raise ValueError(f"Edge '{token}' must be in the format 'u-v' or 'u,v'")
        edges.append((int(a), int(b)))
    return edges


def build_adj(edges: List[Tuple[int, int]]) -> AdjList:
    """Builds an adjacency list from a list of edges."""
    adj: AdjList = defaultdict(list)
    for u, v in edges:
        if u == v:
            raise ValueError("Self-loops are not supported.")
        adj[u].append(v)
        adj[v].append(u)
    return adj


def build_parser() -> argparse.ArgumentParser:
    """Builds the command-line argument parser."""
    p = argparse.ArgumentParser(
        description="Find an Eulerian trail or circuit using Hierholzer's algorithm."
    )
    p.add_argument("edges", nargs="*", help="Edges in the format 'u-v' (e.g., 0-1 1-2).")
    p.add_argument("--start", type=int, help="Optional starting vertex.")
    return p


def main(argv: Optional[List[str]] = None) -> int:
    """The main entry point for the script."""
    parser = build_parser()
    args = parser.parse_args(argv)
    if not args.edges:
        # Use a default example if no edges are provided.
        args.edges = ["0-1", "1-2", "2-0", "0-3", "3-4", "4-2"]
    try:
        edges = parse_edges(args.edges)
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1
    if not edges:
        print("No edges provided.")
        return 0
    adj = build_adj(edges)
    classification = classify_eulerian(adj)
    if classification == "none":
        print("The graph is not Eulerian and has no trail or circuit.")
        return 0
    path = hierholzer(adj, args.start)
    if not path:
        print("Failed to construct the Eulerian traversal.")
        return 1
    print(f"Classification: {classification}")
    print("Traversal: " + " -> ".join(map(str, path)))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
