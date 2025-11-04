#!/usr/bin/env python3
"""
Test Suite for Dijkstra's Algorithm Implementation

This module contains comprehensive tests for the modernized Dijkstra algorithm
implementation to ensure correctness and reliability.

Author: Enhanced implementation testing
Date: September 2025
"""

import unittest
from dijkstra import (
    dijkstra,
    dijkstra_stepper,
    example_graph,
    validate_graph,
    get_shortest_path,
    DijkstraConfiguration,
    AlgorithmState,
)


class TestDijkstraAlgorithm(unittest.TestCase):
    """Test cases for the Dijkstra algorithm implementation."""

    def setUp(self):
        """Set up test fixtures."""
        self.simple_graph = {
            "A": {"B": 1, "C": 4},
            "B": {"C": 2, "D": 5},
            "C": {"D": 1},
            "D": {},
        }

        self.disconnected_graph = {
            "A": {"B": 1},
            "B": {"A": 1},
            "C": {"D": 2},
            "D": {"C": 2},
        }

    def test_basic_shortest_paths(self):
        """Test basic shortest path computation."""
        distances = dijkstra(self.simple_graph, "A")

        expected = {
            "A": 0,
            "B": 1,
            "C": 3,  # A->B->C is shorter than A->C
            "D": 4,  # A->B->C->D
        }

        self.assertEqual(distances, expected)

    def test_example_graph_from_a(self):
        """Test the example graph starting from A."""
        distances = dijkstra(example_graph, "A")

        # Known shortest distances from A
        expected = {
            "A": 0,
            "B": 1,
            "C": 3,  # A->B->C (1+2) is better than A->C (4)
            "D": 4,  # A->B->C->D (1+2+1)
            "E": 7,  # A->B->C->D->E (1+2+1+3)
            "F": float("infinity"),
            "G": float("infinity"),
        }

        self.assertEqual(distances, expected)

    def test_disconnected_components(self):
        """Test handling of disconnected graph components."""
        distances = dijkstra(self.disconnected_graph, "A")

        expected = {"A": 0, "B": 1, "C": float("infinity"), "D": float("infinity")}

        self.assertEqual(distances, expected)

    def test_edges_to_undefined_nodes_are_skipped(self):
        """Ensure edges referencing undefined nodes are ignored without crashing."""
        graph = {
            "A": {"B": 1, "Z": 5},
            "B": {},
        }

        config = DijkstraConfiguration(enable_logging=False, validate_graph=False)

        distances = dijkstra(graph, "A", config=config)

        self.assertEqual(distances["A"], 0)
        self.assertEqual(distances["B"], 1)
        self.assertNotIn("Z", distances)

    def test_single_node_graph(self):
        """Test graph with single node."""
        single_graph = {"A": {}}
        distances = dijkstra(single_graph, "A")

        self.assertEqual(distances, {"A": 0})

    def test_invalid_start_node(self):
        """Test error handling for invalid start node."""
        with self.assertRaises(ValueError):
            dijkstra(example_graph, "INVALID")

    def test_graph_validation(self):
        """Test graph validation functionality."""
        # Valid graph should not raise
        validate_graph(example_graph)

        # Invalid graph structures should raise
        with self.assertRaises(TypeError):
            validate_graph("not a dict")

        with self.assertRaises(ValueError):
            validate_graph({})

        with self.assertRaises(ValueError):
            invalid_graph = {"A": {"B": -1}}  # Negative weight
            validate_graph(invalid_graph)

    def test_configuration_options(self):
        """Test different configuration options."""
        config = DijkstraConfiguration(
            enable_logging=False, validate_graph=True, track_path=True
        )

        distances = dijkstra(example_graph, "A", config=config)
        self.assertIsInstance(distances, dict)
        self.assertEqual(distances["A"], 0)

    def test_target_node_optimization(self):
        """Test early termination with target node."""
        # Should work the same as normal computation
        distances = dijkstra(example_graph, "A", target_node="D")
        self.assertEqual(distances["D"], 4)

    def test_stepper_generator(self):
        """Test the step-by-step generator function."""
        steps = list(dijkstra_stepper(self.simple_graph, "A"))

        # Should have multiple steps
        self.assertGreater(len(steps), 1)

        # First step should be initialization
        first_step = steps[0]
        self.assertEqual(first_step.state, AlgorithmState.INITIALIZING)
        self.assertIsNone(first_step.current_node)
        self.assertEqual(first_step.distances["A"], 0)

        # Last step should be completion
        last_step = steps[-1]
        self.assertEqual(last_step.state, AlgorithmState.COMPLETED)

        # Final distances should match regular algorithm
        regular_distances = dijkstra(self.simple_graph, "A")
        self.assertEqual(last_step.distances, regular_distances)

    def test_path_reconstruction(self):
        """Test shortest path reconstruction."""
        # Create previous nodes manually for testing
        previous = {"A": None, "B": "A", "C": "B", "D": "C"}

        path = get_shortest_path(previous, "A", "D")
        expected_path = ["A", "B", "C", "D"]
        self.assertEqual(path, expected_path)

        # Test no path case
        path = get_shortest_path(previous, "A", "NONEXISTENT")
        self.assertEqual(path, [])


class TestPerformance(unittest.TestCase):
    """Performance and stress tests."""

    def test_larger_graph_performance(self):
        """Test performance with a larger graph."""
        # Create a larger graph programmatically
        large_graph = {}
        for i in range(20):
            node = f"N{i}"
            large_graph[node] = {}
            # Connect to next few nodes
            for j in range(1, min(4, 20 - i)):
                if i + j < 20:
                    neighbor = f"N{i + j}"
                    large_graph[node][neighbor] = j

        # Should complete without error
        distances = dijkstra(large_graph, "N0")
        self.assertEqual(distances["N0"], 0)
        self.assertIsInstance(distances["N19"], (int, float))


def run_tests():
    """Run all tests and display results."""
    print("🧪 Running Dijkstra Algorithm Tests")
    print("=" * 50)

    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add test cases
    suite.addTests(loader.loadTestsFromTestCase(TestDijkstraAlgorithm))
    suite.addTests(loader.loadTestsFromTestCase(TestPerformance))

    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    # Summary
    print(f"\n📊 Test Results:")
    print(f"  Tests run: {result.testsRun}")
    print(f"  Failures: {len(result.failures)}")
    print(f"  Errors: {len(result.errors)}")

    if result.wasSuccessful():
        print("  ✅ All tests passed!")
        return True
    else:
        print("  ❌ Some tests failed!")
        return False


if __name__ == "__main__":
    success = run_tests()
    exit(0 if success else 1)
