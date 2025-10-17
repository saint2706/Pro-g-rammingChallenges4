from __future__ import annotations

from collections import Counter, defaultdict
from typing import Dict, Iterable, List, Optional, Set, Tuple
import argparse

AdjList = Dict[int, List[int]]

# ------------------------- Eulerian Helpers ------------------------- #


def _vertex_universe(adj_list: AdjList) -> Set[int]:
    vertices: Set[int] = set(adj_list)
    for neighbours in adj_list.values():
        vertices.update(neighbours)
    return vertices


def degree_parity(adj_list: AdjList) -> Dict[int, int]:
    return {v: (len(adj_list[v]) % 2) for v in adj_list}


def is_connected_ignoring_isolated(adj_list: AdjList) -> bool:
    """Check connectivity ignoring isolated vertices (those with degree 0)."""
    vertices = _vertex_universe(adj_list)
    non_isolated = [v for v in vertices if len(adj_list.get(v, [])) > 0]
    if not non_isolated:
        return True  # graph without edges is trivially connected

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
    """Return classification: 'none', 'trail', or 'circuit'."""
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
    """Compute Eulerian circuit or trail using Hierholzer's algorithm.

    The input adjacency structure is treated as undirected; reverse edges are removed.

    Args:
        adj_list: adjacency list mapping vertex -> list of neighbors. (Will NOT be mutated.)
        start: optional starting vertex. If None, selected automatically.

    Returns:
        List of vertices representing Eulerian traversal (length edges+1) or empty if none.
    """
    if not adj_list:
        return []

    vertices = _vertex_universe(adj_list)
    if not vertices:
        return []

    classification = classify_eulerian(adj_list)
    if classification == "none":
        return []

    work: Dict[int, Counter[int]] = {u: Counter(neigh) for u, neigh in adj_list.items()}
    for v in vertices:
        work.setdefault(v, Counter())

    edge_count = sum(sum(counter.values()) for counter in work.values()) // 2
    if edge_count == 0:
        return []

    if start is None:
        if classification == "trail":
            for v in sorted(vertices):
                if len(adj_list.get(v, [])) % 2 == 1:
                    start = v
                    break
        else:
            start = min(vertices)
    if start is None or start not in vertices:
        return []

    path: List[int] = []
    stack: List[int] = [start]

    while stack:
        v = stack[-1]
        counter = work[v]
        if counter:
            u = next(iter(counter))
            counter[u] -= 1
            if counter[u] == 0:
                del counter[u]

            rev = work[u]
            if rev.get(v, 0) <= 0:
                return []
            rev[v] -= 1
            if rev[v] == 0:
                del rev[v]

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
    adj = build_adj(edges)
    classification = classify_eulerian(adj)
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
