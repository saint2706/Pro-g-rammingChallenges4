from collections import defaultdict
from typing import List, Dict, Optional

def has_eulerian_circuit(adj_list: Dict[int, List[int]], num_vertices: int) -> bool:
    """
    Checks if a graph has an Eulerian circuit.
    A graph has an Eulerian circuit if and only if:
    1. It is connected (ignoring isolated vertices).
    2. Every vertex has an even degree.
    """
    # 1. Check for even degree
    for vertex in adj_list:
        if len(adj_list[vertex]) % 2 != 0:
            print(f"Error: Vertex {vertex} has an odd degree ({len(adj_list[vertex])}). Not Eulerian.")
            return False

    # 2. Check for connectivity (using a simple DFS/traversal)
    if not adj_list:
        return True # An empty graph is trivially Eulerian

    visited = {v: False for v in range(num_vertices)}

    # Find the first vertex with edges to start the traversal from
    start_node = -1
    for i in range(num_vertices):
        if i in adj_list and adj_list[i]:
            start_node = i
            break

    if start_node == -1:
        return True # Graph has no edges

    # Perform DFS to find all reachable nodes
    stack = [start_node]
    visited[start_node] = True
    count = 0
    while stack:
        u = stack.pop()
        count += 1
        for v in adj_list.get(u, []):
            if not visited[v]:
                visited[v] = True
                stack.append(v)

    # Check if all vertices with edges were visited
    for i in range(num_vertices):
        if i in adj_list and adj_list[i] and not visited[i]:
            print("Error: Graph is not connected. Not Eulerian.")
            return False

    return True

def find_eulerian_circuit(adj_list: Dict[int, List[int]], start_node: int) -> Optional[List[int]]:
    """
    Finds an Eulerian circuit in a graph using Hierholzer's algorithm.

    Args:
        adj_list: The graph represented as an adjacency list (dictionary).
                  The lists will be modified by the algorithm.
        start_node: The node to start the search from.

    Returns:
        A list of vertices representing the Eulerian circuit, or None if failed.
    """
    if not adj_list:
        return []

    current_path = [start_node]
    circuit = []

    while current_path:
        current_vertex = current_path[-1]

        # If there are unvisited edges from the current vertex
        if current_vertex in adj_list and adj_list[current_vertex]:
            next_vertex = adj_list[current_vertex].pop()
            # To make it work for an undirected graph, remove the reverse edge too
            if next_vertex in adj_list and current_vertex in adj_list[next_vertex]:
                 adj_list[next_vertex].remove(current_vertex)
            current_path.append(next_vertex)
        else:
            # If all edges from the current vertex are visited, backtrack
            circuit.append(current_path.pop())

    # The circuit is built in reverse order, so we reverse it at the end
    return circuit[::-1]

def main():
    """
    Main function to define a graph, validate it, and find the Eulerian circuit.
    """
    print("--- Hierholzer's Algorithm for Eulerian Circuits ---")

    # Example graph: A connected graph where all vertices have even degree
    # 0 -- 1 -- 2 -- 0 -- 3 -- 4 -- 2
    edges = [(0, 1), (1, 2), (2, 0), (0, 3), (3, 4), (4, 2)]
    num_vertices = 5

    # Build adjacency list from edges
    adj = defaultdict(list)
    for u, v in edges:
        adj[u].append(v)
        adj[v].append(u)

    print("Graph Adjacency List:", {k: v for k, v in adj.items()})

    if has_eulerian_circuit(adj.copy(), num_vertices):
        print("\nThe graph has an Eulerian circuit.")
        # We need a copy because the algorithm modifies the list
        adj_for_algo = {k: list(v) for k, v in adj.items()}

        start_node = next(iter(adj)) # Start at the first vertex with edges
        circuit = find_eulerian_circuit(adj_for_algo, start_node)

        if circuit:
            print("Eulerian Circuit Found:")
            print(" -> ".join(map(str, circuit)))
        else:
            print("Algorithm failed to find a circuit.")
    else:
        print("\nThe graph does not have an Eulerian circuit.")

if __name__ == "__main__":
    main()
