from __future__ import annotations

from collections import defaultdict, deque
from typing import List, Dict, Optional, Tuple, Iterable
import argparse

AdjList = Dict[int, List[int]]

# ------------------------- Eulerian Helpers ------------------------- #


def degree_parity(adj_list: AdjList) -> Dict[int, int]:
    return {v: (len(adj_list[v]) % 2) for v in adj_list}


def is_connected_ignoring_isolated(adj_list: AdjList, num_vertices: int) -> bool:
    """Check connectivity ignoring isolated vertices (those with degree 0)."""
    # find a start vertex with edges
    start = next(
        (v for v in range(num_vertices) if v in adj_list and adj_list[v]), None
    )
    if start is None:
        return True  # no edges
    visited = [False] * num_vertices
    stack = [start]
    visited[start] = True
    while stack:
        u = stack.pop()
        for w in adj_list.get(u, []):
            if not visited[w]:
                visited[w] = True
                stack.append(w)
    for v in range(num_vertices):
        if v in adj_list and adj_list[v] and not visited[v]:
            return False
    return True


def classify_eulerian(adj_list: AdjList, num_vertices: int) -> str:
    """Return classification: 'none', 'trail', or 'circuit'."""
    if not is_connected_ignoring_isolated(adj_list, num_vertices):
        return "none"
    odd_vertices = [v for v in adj_list if len(adj_list[v]) % 2 == 1]
    if len(odd_vertices) == 0:
        return "circuit"
    if len(odd_vertices) == 2:
        return "trail"
    return "none"


# ---------------------- Hierholzer Core Algorithm ------------------- #


def hierholzer(adj_list: AdjList, start: Optional[int] = None) -> List[int]:
    """Compute Eulerian circuit or trail using Hierholzer's algorithm.

    The input adjacency structure is treated as undirected; reverse edges are removed.

    Args:
        adj_list: adjacency list mapping vertex -> list of neighbors. (Will NOT be mutated.)
        start: optional starting vertex. If None, selected automatically.

    Returns:
        List of vertices representing Eulerian traversal (length edges+1) or empty if none.
    """
    # Determine vertex universe
    if not adj_list:
        return []
    # Build a working copy (multigraph-friendly by removing per-incidence)
    work: AdjList = {u: list(neigh) for u, neigh in adj_list.items()}
    edge_count = sum(len(vs) for vs in work.values()) // 2
    if edge_count == 0:
        return []

    classification = classify_eulerian(work, max(work) + 1)
    if classification == "none":
        return []

    if start is None:
        if classification == "trail":
            # pick one odd vertex
            for v, vs in work.items():
                if len(vs) % 2 == 1:
                    start = v
                    break
        else:
            start = next(iter(work))
    assert start is not None

    path: List[int] = []
    stack: List[int] = [start]

    while stack:
        v = stack[-1]
        if work.get(v):
            u = work[v].pop()
            # remove reverse edge
            if u in work and v in work[u]:
                work[u].remove(v)
            stack.append(u)
        else:
            path.append(stack.pop())

    path.reverse()
    if len(path) != edge_count + 1:
        return []  # failed to use all edges
    return path


# ------------------------------ CLI --------------------------------- #


def parse_edges(pairs: Iterable[str]) -> List[Tuple[int, int]]:
    edges: List[Tuple[int, int]] = []
    for token in pairs:
        if "-" in token:
            a, b = token.split("-", 1)
        elif "," in token:
            a, b = token.split(",", 1)
        else:
            raise ValueError(
                f"Edge '{token}' must contain '-' or ',' separating vertices"
            )
        edges.append((int(a), int(b)))
    return edges


def build_adj(edges: List[Tuple[int, int]]) -> AdjList:
    adj: AdjList = defaultdict(list)
    for u, v in edges:
        if u == v:
            raise ValueError("Self-loops not supported in this simple model")
        adj[u].append(v)
        adj[v].append(u)
    return adj


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Find Eulerian trail/circuit using Hierholzer's algorithm"
    )
    p.add_argument("edges", nargs="*", help="Edges like 0-1 1-2 2-0 0-3 3-4 4-2")
    p.add_argument("--start", type=int, help="Optional starting vertex")
    return p


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not args.edges:
        # default demo
        args.edges = ["0-1", "1-2", "2-0", "0-3", "3-4", "4-2"]
    try:
        edges = parse_edges(args.edges)
    except ValueError as e:
        print(f"Error: {e}")
        return 1
    if not edges:
        print("No edges.")
        return 0
    max_v = max(max(u, v) for u, v in edges)
    adj = build_adj(edges)
    classification = classify_eulerian(adj, max_v + 1)
    if classification == "none":
        print("Graph is not Eulerian (no trail/circuit).")
        return 0
    path = hierholzer(adj, args.start)
    if not path:
        print("Failed to construct Eulerian traversal.")
        return 1
    print(f"Classification: {classification}")
    print("Traversal: " + " -> ".join(map(str, path)))
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
