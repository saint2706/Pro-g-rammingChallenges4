#!/usr/bin/env python3
"""
Dijkstra's Shortest Path Algorithm Implementation

This module provides a comprehensive implementation of Dijkstra's algorithm for finding
the shortest paths in weighted graphs. It includes both a standard implementation and
a generator-based version for step-by-step visualization.

The implementation supports:
- Weighted directed and undirected graphs
- Step-by-step algorithm execution for visualization
- Comprehensive error handling and input validation
- Type safety with modern Python typing features

Author: Enhanced implementation with modern Python practices
Date: September 2025
License: MIT

Example:
    Basic usage:
        >>> from dijkstra import dijkstra, example_graph
        >>> distances = dijkstra(example_graph, "A")
        >>> print(distances["E"])
        7

    Step-by-step visualization:
        >>> for step in dijkstra_stepper(example_graph, "A"):
        ...     print(step["message"])
"""

import heapq
import logging
import sys
import time
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Dict, List, Optional, Set, Tuple, Union, Iterator, Callable

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)

# Type aliases for better readability and maintainability
Weight = Union[int, float]
NodeId = str
Graph = Dict[NodeId, Dict[NodeId, Weight]]
Distances = Dict[NodeId, Weight]
PreviousNodes = Dict[NodeId, Optional[NodeId]]
PriorityQueue = List[Tuple[Weight, NodeId]]


class AlgorithmState(Enum):
    """Enumeration of algorithm execution states."""

    INITIALIZING = auto()
    PROCESSING = auto()
    UPDATING = auto()
    COMPLETED = auto()


@dataclass
class DijkstraConfiguration:
    """
    Configuration settings for Dijkstra's algorithm execution.

    Attributes:
        enable_logging: Enable detailed logging of algorithm steps
        validate_graph: Perform comprehensive graph validation before execution
        track_path: Track the actual shortest path, not just distances
        timeout_seconds: Maximum execution time (0 = no timeout)
        progress_callback: Optional callback function for progress updates
    """

    enable_logging: bool = True
    validate_graph: bool = True
    track_path: bool = True
    timeout_seconds: float = 0.0
    progress_callback: Optional[Callable[[int, int], None]] = None


@dataclass
class AlgorithmStep:
    """
    Represents a single step in the Dijkstra algorithm execution.

    This dataclass provides a structured way to capture and communicate
    the state of the algorithm at each iteration, making it ideal for
    visualization and debugging purposes.

    Attributes:
        step_number: Sequential step number (0-based)
        state: Current algorithm state
        current_node: Node currently being processed (None if not processing)
        distances: Current shortest distances to all nodes
        previous_nodes: Previous node in shortest path for each node
        priority_queue: Current state of the priority queue
        visited_nodes: Set of nodes that have been fully processed
        message: Human-readable description of the current step
        timestamp: When this step was recorded
        processing_time: Time elapsed since algorithm start
    """

    step_number: int
    state: AlgorithmState
    current_node: Optional[NodeId]
    distances: Distances
    previous_nodes: PreviousNodes
    priority_queue: List[NodeId]
    visited_nodes: Set[NodeId]
    message: str
    timestamp: float = field(default_factory=time.time)
    processing_time: float = 0.0


def validate_graph(graph: Graph) -> None:
    """
    Validate the graph structure and data integrity.

    Args:
        graph: The graph to validate

    Raises:
        ValueError: If the graph structure is invalid
        TypeError: If the graph contains invalid data types
    """
    if not isinstance(graph, dict):
        raise TypeError("Graph must be a dictionary")

    if not graph:
        raise ValueError("Graph cannot be empty")

    for node, neighbors in graph.items():
        if not isinstance(node, str):
            raise TypeError(f"Node identifiers must be strings, got {type(node)}")

        if not isinstance(neighbors, dict):
            raise TypeError(f"Neighbors for node {node} must be a dictionary")

        for neighbor, weight in neighbors.items():
            if not isinstance(neighbor, str):
                raise TypeError(
                    f"Neighbor identifiers must be strings, got {type(neighbor)}"
                )

            if not isinstance(weight, (int, float)):
                raise TypeError(
                    f"Edge weights must be numeric, got {type(weight)} for edge {node}->{neighbor}"
                )

            if weight < 0:
                raise ValueError(
                    f"Edge weights must be non-negative, got {weight} for edge {node}->{neighbor}"
                )

            # Warn about disconnected references
            if neighbor not in graph:
                logger.warning(
                    f"Node {neighbor} referenced by {node} but not defined in graph"
                )


def dijkstra(
    graph: Graph,
    start_node: NodeId,
    target_node: Optional[NodeId] = None,
    config: Optional[DijkstraConfiguration] = None,
) -> Distances:
    """
    Implements Dijkstra's algorithm to find the shortest paths from a single
    source node to all other nodes in a weighted graph.

    This implementation uses a binary heap (priority queue) for efficient
    extraction of the minimum distance node, achieving O((V + E) log V) time
    complexity where V is the number of vertices and E is the number of edges.

    Args:
        graph: A dictionary representing the graph as an adjacency list.
               The keys are the nodes, and the values are dictionaries
               mapping neighboring nodes to the weight of the connecting edge.
               e.g., {'A': {'B': 1, 'C': 4}, ...}
        start_node: The node from which to calculate the shortest paths.
        target_node: Optional target node. If specified, algorithm stops when
                    the shortest path to this node is found (optimization).
        config: Optional configuration for algorithm behavior.

    Returns:
        A dictionary mapping each node to its shortest distance from the
        start_node. Unreachable nodes will have a distance of infinity.

    Raises:
        ValueError: If the start_node is not in the graph or graph is invalid.
        TypeError: If the graph structure contains invalid data types.
        TimeoutError: If execution exceeds the configured timeout.

    Example:
        >>> graph = {'A': {'B': 1, 'C': 4}, 'B': {'C': 2}, 'C': {}}
        >>> distances = dijkstra(graph, 'A')
        >>> print(distances)  # {'A': 0, 'B': 1, 'C': 3}
    """
    # Initialize configuration
    if config is None:
        config = DijkstraConfiguration()

    # Validate inputs
    if config.validate_graph:
        validate_graph(graph)

    if start_node not in graph:
        raise ValueError(f"Start node '{start_node}' not found in the graph.")

    if target_node is not None and target_node not in graph:
        raise ValueError(f"Target node '{target_node}' not found in the graph.")

    if config.enable_logging:
        logger.info(f"Starting Dijkstra's algorithm from node '{start_node}'")
        if target_node:
            logger.info(f"Target node: '{target_node}'")

    # Initialize algorithm state
    start_time = time.time()
    distances: Distances = {node: float("infinity") for node in graph}
    distances[start_node] = 0

    # Track previous nodes for path reconstruction if enabled
    previous_nodes: PreviousNodes = (
        {node: None for node in graph} if config.track_path else {}
    )

    # Priority queue stores tuples of (distance, node)
    # Using heapq for efficient min-heap operations
    priority_queue: PriorityQueue = [(0, start_node)]
    visited_nodes: Set[NodeId] = set()

    # Statistics tracking
    nodes_processed = 0
    edges_relaxed = 0

    while priority_queue:
        # Check timeout if configured
        if config.timeout_seconds > 0:
            elapsed = time.time() - start_time
            if elapsed > config.timeout_seconds:
                raise TimeoutError(
                    f"Algorithm exceeded timeout of {config.timeout_seconds} seconds"
                )

        # Get the node with the smallest distance from the priority queue
        current_distance, current_node = heapq.heappop(priority_queue)

        # Skip if we've already found a shorter path to this node
        # This can happen when the same node is added multiple times with different distances
        if current_node in visited_nodes:
            continue

        # Mark node as visited
        visited_nodes.add(current_node)
        nodes_processed += 1

        # Early termination if we reached the target node
        if target_node and current_node == target_node:
            if config.enable_logging:
                logger.info(
                    f"Reached target node '{target_node}' with distance {current_distance}"
                )
            break

        # Progress callback if provided
        if config.progress_callback:
            config.progress_callback(nodes_processed, len(graph))

        if config.enable_logging and nodes_processed % 10 == 0:
            logger.debug(f"Processed {nodes_processed} nodes, current: {current_node}")

        # Explore neighbors of the current node
        for neighbor, weight in graph[current_node].items():
            # Skip already visited neighbors
            if neighbor in visited_nodes:
                continue

            distance = current_distance + weight
            edges_relaxed += 1

            # If a shorter path to the neighbor is found
            if distance < distances[neighbor]:
                distances[neighbor] = distance
                if config.track_path:
                    previous_nodes[neighbor] = current_node
                heapq.heappush(priority_queue, (distance, neighbor))

    # Log final statistics
    if config.enable_logging:
        elapsed_time = time.time() - start_time
        logger.info(f"Algorithm completed in {elapsed_time:.3f} seconds")
        logger.info(f"Processed {nodes_processed} nodes, relaxed {edges_relaxed} edges")

        # Count reachable nodes
        reachable_nodes = sum(1 for d in distances.values() if d != float("infinity"))
        logger.info(f"Found paths to {reachable_nodes}/{len(graph)} nodes")

    return distances


def get_shortest_path(
    previous_nodes: PreviousNodes, start_node: NodeId, target_node: NodeId
) -> List[NodeId]:
    """
    Reconstruct the shortest path from start to target node.

    Args:
        previous_nodes: Dictionary mapping each node to its predecessor
        start_node: Starting node of the path
        target_node: Target node of the path

    Returns:
        List of nodes representing the shortest path from start to target.
        Returns empty list if no path exists.

    Example:
        >>> previous = {'A': None, 'B': 'A', 'C': 'B'}
        >>> path = get_shortest_path(previous, 'A', 'C')
        >>> print(path)  # ['A', 'B', 'C']
    """
    if target_node not in previous_nodes:
        return []

    path = []
    current = target_node

    # Traverse backwards from target to start
    while current is not None:
        path.append(current)
        current = previous_nodes.get(current)

    # Reverse to get path from start to target
    path.reverse()

    # Verify the path starts with start_node
    if path and path[0] != start_node:
        return []  # No valid path exists

    return path


def dijkstra_stepper(
    graph: Graph, start_node: NodeId, config: Optional[DijkstraConfiguration] = None
) -> Iterator[AlgorithmStep]:
    """
    A generator version of Dijkstra's algorithm that yields its state at each step,
    making it suitable for visualization and step-by-step analysis.

    This function provides complete transparency into the algorithm's execution,
    yielding detailed state information at each iteration. It's particularly
    useful for educational purposes and algorithm visualization.

    Args:
        graph: A dictionary representing the graph as an adjacency list
        start_node: The node from which to calculate the shortest paths
        config: Optional configuration for algorithm behavior

    Yields:
        AlgorithmStep: Detailed state information for each algorithm step

    Raises:
        ValueError: If the start_node is not in the graph or graph is invalid
        TypeError: If the graph structure contains invalid data types

    Example:
        >>> graph = {'A': {'B': 1}, 'B': {'C': 2}, 'C': {}}
        >>> for step in dijkstra_stepper(graph, 'A'):
        ...     print(f"Step {step.step_number}: {step.message}")
    """
    # Initialize configuration
    if config is None:
        config = DijkstraConfiguration()

    # Validate inputs
    if config.validate_graph:
        validate_graph(graph)

    if start_node not in graph:
        raise ValueError(f"Start node '{start_node}' not found in the graph.")

    # Initialize algorithm state
    start_time = time.time()
    step_counter = 0

    distances: Distances = {node: float("infinity") for node in graph}
    distances[start_node] = 0

    previous_nodes: PreviousNodes = {node: None for node in graph}
    priority_queue: PriorityQueue = [(0, start_node)]
    visited_nodes: Set[NodeId] = set()

    if config.enable_logging:
        logger.info(f"Starting step-by-step Dijkstra execution from '{start_node}'")

    # Yield initial state
    yield AlgorithmStep(
        step_number=step_counter,
        state=AlgorithmState.INITIALIZING,
        current_node=None,
        distances=distances.copy(),
        previous_nodes=previous_nodes.copy(),
        priority_queue=[item[1] for item in priority_queue],
        visited_nodes=visited_nodes.copy(),
        message=f"Initialized algorithm with start node '{start_node}'. All distances set to infinity except start node (distance 0).",
        processing_time=time.time() - start_time,
    )
    step_counter += 1

    # Main algorithm loop
    while priority_queue:
        # Extract minimum distance node
        current_distance, current_node = heapq.heappop(priority_queue)

        # Skip if already visited (can happen with duplicate entries)
        if current_node in visited_nodes:
            continue

        # Mark as visited
        visited_nodes.add(current_node)

        # Yield processing state
        yield AlgorithmStep(
            step_number=step_counter,
            state=AlgorithmState.PROCESSING,
            current_node=current_node,
            distances=distances.copy(),
            previous_nodes=previous_nodes.copy(),
            priority_queue=[
                item[1] for item in priority_queue if item[1] not in visited_nodes
            ],
            visited_nodes=visited_nodes.copy(),
            message=f"Processing node '{current_node}' with current shortest distance {current_distance}. "
            f"Exploring {len(graph[current_node])} neighbors.",
            processing_time=time.time() - start_time,
        )
        step_counter += 1

        # Process each neighbor
        neighbors_updated = []
        for neighbor, weight in graph[current_node].items():
            if neighbor in visited_nodes:
                continue

            new_distance = current_distance + weight

            # If we found a shorter path
            if new_distance < distances[neighbor]:
                old_distance = distances[neighbor]
                distances[neighbor] = new_distance
                previous_nodes[neighbor] = current_node
                heapq.heappush(priority_queue, (new_distance, neighbor))
                neighbors_updated.append((neighbor, old_distance, new_distance))

        # Yield update state if any neighbors were updated
        if neighbors_updated:
            update_messages = []
            for neighbor, old_dist, new_dist in neighbors_updated:
                old_str = "∞" if old_dist == float("infinity") else f"{old_dist}"
                update_messages.append(f"{neighbor}: {old_str} → {new_dist}")

            yield AlgorithmStep(
                step_number=step_counter,
                state=AlgorithmState.UPDATING,
                current_node=current_node,
                distances=distances.copy(),
                previous_nodes=previous_nodes.copy(),
                priority_queue=[
                    item[1] for item in priority_queue if item[1] not in visited_nodes
                ],
                visited_nodes=visited_nodes.copy(),
                message=f"Updated distances via '{current_node}': {', '.join(update_messages)}",
                processing_time=time.time() - start_time,
            )
            step_counter += 1

    # Yield completion state
    reachable_count = sum(1 for d in distances.values() if d != float("infinity"))
    total_time = time.time() - start_time

    yield AlgorithmStep(
        step_number=step_counter,
        state=AlgorithmState.COMPLETED,
        current_node=None,
        distances=distances.copy(),
        previous_nodes=previous_nodes.copy(),
        priority_queue=[],
        visited_nodes=visited_nodes.copy(),
        message=f"Algorithm completed! Found shortest paths to {reachable_count}/{len(graph)} nodes "
        f"in {total_time:.3f} seconds.",
        processing_time=total_time,
    )

    if config.enable_logging:
        logger.info(f"Step-by-step execution completed in {step_counter} steps")


# Example graph for demonstration and testing
example_graph: Graph = {
    "A": {"B": 1, "C": 4},
    "B": {"A": 1, "C": 2, "D": 5},
    "C": {"A": 4, "B": 2, "D": 1},
    "D": {"B": 5, "C": 1, "E": 3},
    "E": {"D": 3},
    "F": {"G": 2},  # An unconnected component
    "G": {"F": 2},
}


def print_shortest_paths(distances: Distances, start_node: NodeId) -> None:
    """
    Print the shortest distances in a formatted manner.

    Args:
        distances: Dictionary of shortest distances
        start_node: The starting node for context
    """
    print(f"\nShortest distances from node '{start_node}':")
    print("=" * 50)

    # Sort nodes for consistent output
    for node in sorted(distances.keys()):
        distance = distances[node]
        if distance == float("infinity"):
            dist_str = "Not Reachable"
            status = "❌"
        else:
            dist_str = f"{distance}"
            status = "✓"
        print(f"  {status} To Node {node}: {dist_str}")


def demonstrate_algorithm() -> None:
    """
    Demonstrate the algorithm with the example graph.
    """
    print("🔗 Dijkstra's Shortest Path Algorithm Demonstration")
    print("=" * 60)
    print(
        "Available nodes in the example graph:", ", ".join(sorted(example_graph.keys()))
    )
    print("\nExample graph structure:")
    for node, neighbors in sorted(example_graph.items()):
        neighbor_list = [f"{n}({w})" for n, w in neighbors.items()]
        print(
            f"  {node} → {', '.join(neighbor_list) if neighbor_list else 'no connections'}"
        )

    default_start_node = "A"

    try:
        start_input = (
            input(f"\nEnter the start node (default: {default_start_node}): ")
            .strip()
            .upper()
        )
        start_node = start_input if start_input else default_start_node

        if start_node not in example_graph:
            print(f"❌ Error: Node '{start_node}' not found in graph")
            return

        print(f"\n🚀 Computing shortest paths from '{start_node}'...")

        # Create configuration for demonstration
        config = DijkstraConfiguration(enable_logging=True)

        # Compute shortest distances
        start_time = time.time()
        shortest_distances = dijkstra(example_graph, start_node, config=config)
        computation_time = time.time() - start_time

        # Display results
        print_shortest_paths(shortest_distances, start_node)
        print(f"\n⏱️  Computation completed in {computation_time:.4f} seconds")

        # Demonstrate path reconstruction if tracking is enabled
        if config.track_path:
            print("\n🛤️  Example path reconstruction:")
            # Find a reachable target
            reachable_nodes = [
                node
                for node, dist in shortest_distances.items()
                if dist != float("infinity") and node != start_node
            ]
            if reachable_nodes:
                target = reachable_nodes[-1]  # Pick the last reachable node
                # We would need to run dijkstra again to get previous_nodes for path reconstruction
                print(
                    f"   Path reconstruction would show route from {start_node} to {target}"
                )

    except (EOFError, KeyboardInterrupt):
        print("\n\n👋 Operation cancelled by user.")
    except Exception as e:
        print(f"\n❌ Error: {e}", file=sys.stderr)


def main() -> None:
    """
    Main function to run the Dijkstra algorithm demonstration.

    This function provides an interactive demonstration of Dijkstra's algorithm
    using the predefined example graph. Users can select different starting nodes
    to see how the algorithm computes shortest paths.
    """
    try:
        demonstrate_algorithm()
    except Exception as e:
        logger.error(f"Unexpected error in main: {e}")
        print(f"An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
