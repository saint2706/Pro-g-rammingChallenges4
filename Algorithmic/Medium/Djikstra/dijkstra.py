import heapq
import sys
from typing import Dict, List, Tuple, Union

# Define type aliases for better readability
Graph = Dict[str, Dict[str, Union[int, float]]]
Distances = Dict[str, Union[int, float]]

def dijkstra(graph: Graph, start_node: str) -> Distances:
    """
    Implements Dijkstra's algorithm to find the shortest paths from a single
    source node to all other nodes in a weighted graph.

    Args:
        graph: A dictionary representing the graph as an adjacency list.
               The keys are the nodes, and the values are dictionaries
               mapping neighboring nodes to the weight of the connecting edge.
               e.g., {'A': {'B': 1, 'C': 4}, ...}
        start_node: The node from which to calculate the shortest paths.

    Returns:
        A dictionary mapping each node to its shortest distance from the
        start_node. Unreachable nodes will have a distance of infinity.

    Raises:
        ValueError: If the start_node is not in the graph.
    """
    if start_node not in graph:
        raise ValueError(f"Start node '{start_node}' not found in the graph.")

    # Initialize distances to all nodes as infinity, except for the start node
    distances: Distances = {node: float("infinity") for node in graph}
    distances[start_node] = 0

    # The priority queue stores tuples of (distance, node)
    priority_queue: List[Tuple[Union[int, float], str]] = [(0, start_node)]

    while priority_queue:
        # Get the node with the smallest distance from the priority queue
        current_distance, current_node = heapq.heappop(priority_queue)

        # If we've already found a shorter path to this node, skip
        if current_distance > distances[current_node]:
            continue

        # Explore neighbors of the current node
        for neighbor, weight in graph[current_node].items():
            distance = current_distance + weight

            # If a shorter path to the neighbor is found
            if distance < distances[neighbor]:
                distances[neighbor] = distance
                heapq.heappush(priority_queue, (distance, neighbor))

    return distances

def main():
    """
    Main function to create an example graph and demonstrate Dijkstra's algorithm.
    """
    # Example graph represented as an adjacency list
    example_graph: Graph = {
        "A": {"B": 1, "C": 4},
        "B": {"A": 1, "C": 2, "D": 5},
        "C": {"A": 4, "B": 2, "D": 1},
        "D": {"B": 5, "C": 1, "E": 3},
        "E": {"D": 3},
        "F": {"G": 2}, # An unconnected component
        "G": {"F": 2},
    }

    print("--- Dijkstra's Shortest Path Algorithm Demonstration ---")
    print("Available nodes in the example graph:", ", ".join(example_graph.keys()))

    default_start_node = "A"

    try:
        start_node = input(f"Enter the start node (default: {default_start_node}): ").strip().upper()
        if not start_node:
            start_node = default_start_node

        shortest_distances = dijkstra(example_graph, start_node)

        print(f"\nShortest distances from node '{start_node}':")
        for node, distance in sorted(shortest_distances.items()):
            dist_str = str(distance) if distance != float('infinity') else "Not Reachable"
            print(f"  - To Node {node}: {dist_str}")

    except ValueError as e:
        print(f"\nError: {e}", file=sys.stderr)
    except (EOFError, KeyboardInterrupt):
        print("\n\nOperation cancelled by user.")

if __name__ == "__main__":
    main()
