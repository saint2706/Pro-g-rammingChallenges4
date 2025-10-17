from collections import Counter

from challenges.Emulation.EulerianPath.hierholzer import (
    build_adj,
    classify_eulerian,
    hierholzer,
)


def test_hierholzer_handles_sparse_vertex_labels() -> None:
    edges = [(2, 3), (3, 4), (4, 2)]
    adj = build_adj(edges)

    assert classify_eulerian(adj) == "circuit"
    path = hierholzer(adj)

    assert path[0] == 2
    assert len(path) == len(edges) + 1
    used_edges = Counter(frozenset((path[i], path[i + 1])) for i in range(len(path) - 1))
    expected_edges = Counter(frozenset(edge) for edge in edges)
    assert used_edges == expected_edges


def test_hierholzer_preserves_parallel_edges() -> None:
    edges = [(0, 1), (0, 1), (1, 2), (2, 0)]
    adj = build_adj(edges)

    assert classify_eulerian(adj) == "trail"
    path = hierholzer(adj)

    assert len(path) == len(edges) + 1
    used_edges = Counter(frozenset((path[i], path[i + 1])) for i in range(len(path) - 1))
    expected_edges = Counter(frozenset(edge) for edge in edges)
    assert used_edges == expected_edges
