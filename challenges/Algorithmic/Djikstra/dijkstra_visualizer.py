#!/usr/bin/env python3
"""
Dijkstra's Algorithm Visualization

This module provides an interactive visualization of Dijkstra's shortest path algorithm
using matplotlib and networkx. It creates step-by-step animations showing how the
algorithm explores the graph and finds shortest paths.

Features:
- Interactive step-by-step visualization of Dijkstra's algorithm
- Color-coded nodes showing algorithm state (current, visited, in queue)
- Real-time distance updates and path highlighting
- Configurable animation settings and graph layouts
- Professional error handling and user feedback
- Support for custom graphs and visualization themes

Dependencies:
    - networkx: Graph data structures and algorithms
    - matplotlib: Plotting and animation
    - dijkstra: Core algorithm implementation

Author: Enhanced implementation with modern Python practices
Date: September 2025
License: MIT

Example:
    Basic usage:
        python dijkstra_visualizer.py -s A

    Custom animation speed:
        python dijkstra_visualizer.py -s B --speed 2000
"""

import argparse
import logging
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Union, Any

try:
    import matplotlib.pyplot as plt
    import matplotlib.animation as animation
    from matplotlib.animation import FuncAnimation
    import networkx as nx
except ImportError as e:
    print(f"❌ Error: Required visualization libraries not found: {e}")
    print("Please install required packages: pip install matplotlib networkx")
    sys.exit(1)

# Import the generator function and the example graph
try:
    from dijkstra import dijkstra_stepper, example_graph, AlgorithmStep, AlgorithmState
except ImportError:
    print(
        "❌ Error: This script requires 'dijkstra.py' to be in the same directory.",
        file=sys.stderr,
    )
    print("Please ensure dijkstra.py is present and contains the required functions.")
    sys.exit(1)

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    datefmt="%H:%M:%S",
)
logger = logging.getLogger(__name__)


class VisualizationTheme(Enum):
    """Color themes for the visualization."""

    DEFAULT = "default"
    DARK = "dark"
    COLORBLIND = "colorblind"
    MINIMAL = "minimal"


class AnimationSpeed(Enum):
    """Predefined animation speeds in milliseconds."""

    VERY_SLOW = 3000
    SLOW = 2000
    NORMAL = 1200
    FAST = 800
    VERY_FAST = 400


@dataclass
class VisualizationConfig:
    """
    Configuration settings for the Dijkstra visualization.

    Attributes:
        theme: Color theme for the visualization
        animation_speed: Interval between animation frames in milliseconds
        figure_size: Size of the matplotlib figure (width, height)
        node_size: Size of nodes in the graph
        font_size: Font size for labels
        show_weights: Whether to display edge weights
        show_progress: Show progress information during animation
        save_animation: Save animation as video file
        output_path: Path to save animation (if save_animation is True)
        layout_seed: Random seed for consistent graph layout
    """

    theme: VisualizationTheme = VisualizationTheme.DEFAULT
    animation_speed: int = AnimationSpeed.NORMAL.value
    figure_size: Tuple[int, int] = (12, 8)
    node_size: int = 800
    font_size: int = 10
    show_weights: bool = True
    show_progress: bool = True
    save_animation: bool = False
    output_path: Optional[Path] = None
    layout_seed: int = 42


@dataclass
class ThemeColors:
    """Color scheme for visualization themes."""

    current_node: str = "red"
    visited_node: str = "gray"
    queue_node: str = "skyblue"
    unvisited_node: str = "lightgray"
    path_edge: str = "green"
    normal_edge: str = "black"
    background: str = "white"
    text: str = "black"


# Define color themes
THEMES: Dict[VisualizationTheme, ThemeColors] = {
    VisualizationTheme.DEFAULT: ThemeColors(),
    VisualizationTheme.DARK: ThemeColors(
        current_node="#ff6b6b",
        visited_node="#4a4a4a",
        queue_node="#74b9ff",
        unvisited_node="#2d3436",
        path_edge="#00b894",
        normal_edge="#636e72",
        background="#2d3436",
        text="white",
    ),
    VisualizationTheme.COLORBLIND: ThemeColors(
        current_node="#d63031",
        visited_node="#636e72",
        queue_node="#0984e3",
        unvisited_node="#b2bec3",
        path_edge="#00b894",
        normal_edge="#2d3436",
    ),
    VisualizationTheme.MINIMAL: ThemeColors(
        current_node="black",
        visited_node="#636e72",
        queue_node="#ddd",
        unvisited_node="white",
        path_edge="black",
        normal_edge="#ddd",
    ),
}


class DijkstraVisualizer:
    """
    Interactive visualization of Dijkstra's shortest path algorithm.

    This class creates an animated visualization showing the step-by-step execution
    of Dijkstra's algorithm on a graph. It uses matplotlib for rendering and
    networkx for graph layout and manipulation.

    Features:
    - Color-coded nodes showing algorithm state
    - Real-time distance updates
    - Path highlighting as algorithm progresses
    - Customizable themes and animation settings
    - Progress tracking and step information

    Attributes:
        graph_dict: Original graph as adjacency dictionary
        start_node: Starting node for the algorithm
        config: Visualization configuration settings
        G: NetworkX graph object for visualization
        pos: Node positions for consistent layout
        steps: List of algorithm steps to animate
        fig: Matplotlib figure object
        ax: Matplotlib axes object
        colors: Current theme colors
    """

    def __init__(
        self,
        graph: Dict[str, Dict[str, Union[int, float]]],
        start_node: str,
        config: Optional[VisualizationConfig] = None,
    ):
        """
        Initialize the visualizer with a graph and starting node.

        Args:
            graph: Graph represented as adjacency dictionary
            start_node: Node to start the algorithm from
            config: Optional visualization configuration

        Raises:
            ValueError: If start_node is not in the graph
            RuntimeError: If required visualization libraries are not available
        """
        if start_node not in graph:
            raise ValueError(f"Start node '{start_node}' not found in the graph.")

        self.graph_dict = graph
        self.start_node = start_node
        self.config = config or VisualizationConfig()
        self.colors = THEMES[self.config.theme]

        # Create NetworkX graph
        self.G = nx.Graph()
        self._setup_graph()

        # Generate algorithm steps
        logger.info(f"Generating algorithm steps for visualization from '{start_node}'")
        start_time = time.time()
        self.steps = list(dijkstra_stepper(self.graph_dict, self.start_node))
        generation_time = time.time() - start_time
        logger.info(
            f"Generated {len(self.steps)} steps in {generation_time:.3f} seconds"
        )

        # Setup matplotlib figure
        self._setup_figure()

        # Generate consistent node layout
        self.pos = nx.spring_layout(
            self.G, seed=self.config.layout_seed, k=2, iterations=50
        )

        logger.info("Visualizer initialized successfully")

    def _setup_graph(self) -> None:
        """Initialize the NetworkX graph from the adjacency dictionary."""
        for node, edges in self.graph_dict.items():
            for neighbor, weight in edges.items():
                self.G.add_edge(node, neighbor, weight=weight)

        logger.debug(
            f"Created graph with {self.G.number_of_nodes()} nodes and {self.G.number_of_edges()} edges"
        )

    def _setup_figure(self) -> None:
        """Setup the matplotlib figure and axes with theme settings."""
        self.fig, self.ax = plt.subplots(figsize=self.config.figure_size)

        # Apply theme
        self.fig.patch.set_facecolor(self.colors.background)
        self.ax.set_facecolor(self.colors.background)

        # Remove axes for cleaner appearance
        self.ax.set_xticks([])
        self.ax.set_yticks([])

        # Set window title (safely handle potential None)
        try:
            if self.fig.canvas.manager:
                self.fig.canvas.manager.set_window_title(
                    "Dijkstra's Algorithm Visualization"
                )
        except AttributeError:
            pass  # Some backends don't support window titles

    def _get_node_colors(self, step: AlgorithmStep) -> List[str]:
        """
        Determine node colors based on algorithm state.

        Args:
            step: Current algorithm step

        Returns:
            List of colors for each node
        """
        node_colors = []
        for node in self.G.nodes():
            if node == step.current_node:
                node_colors.append(self.colors.current_node)
            elif node in step.visited_nodes:
                node_colors.append(self.colors.visited_node)
            elif node in step.priority_queue:
                node_colors.append(self.colors.queue_node)
            else:
                node_colors.append(self.colors.unvisited_node)

        return node_colors

    def _get_edge_colors(self, step: AlgorithmStep) -> List[str]:
        """
        Determine edge colors based on shortest path tree.

        Args:
            step: Current algorithm step

        Returns:
            List of colors for each edge
        """
        path_edges = set()

        # Build set of edges in the shortest path tree
        for node, prev_node in step.previous_nodes.items():
            if prev_node is not None:
                # Add both directions since networkx.Graph is undirected
                path_edges.add((prev_node, node))
                path_edges.add((node, prev_node))

        edge_colors = []
        for edge in self.G.edges():
            if edge in path_edges:
                edge_colors.append(self.colors.path_edge)
            else:
                edge_colors.append(self.colors.normal_edge)

        return edge_colors

    def _create_node_labels(self, step: AlgorithmStep) -> Dict[str, str]:
        """
        Create node labels showing distances.

        Args:
            step: Current algorithm step

        Returns:
            Dictionary mapping nodes to label strings
        """
        node_labels = {}
        for node in self.G.nodes():
            distance = step.distances[node]
            if distance == float("inf"):
                dist_str = "∞"
            else:
                # Format distance to avoid very long decimal representations
                dist_str = (
                    f"{distance:.1f}"
                    if distance != int(distance)
                    else str(int(distance))
                )

            node_labels[node] = f"{node}\\n({dist_str})"

        return node_labels

    def _update(self, frame_num: int) -> List[Any]:
        """
        Update function for animation frame.

        Args:
            frame_num: Current frame number

        Returns:
            List of matplotlib artists (required for FuncAnimation)
        """
        self.ax.clear()
        self.ax.set_facecolor(self.colors.background)

        # Get current step
        step = self.steps[frame_num]

        # Determine colors
        node_colors = self._get_node_colors(step)
        edge_colors = self._get_edge_colors(step)
        edges = list(
            self.G.edges()
        )  # Maintain explicit order to align with edge_colors

        # Draw the graph
        nx.draw_networkx_nodes(
            self.G,
            self.pos,
            nodelist=list(self.G.nodes()),  # Explicit to align lengths with colors
            node_color=node_colors,  # type: ignore[arg-type]  # networkx accepts List[str]; stub expects str
            ax=self.ax,
            node_size=self.config.node_size,
            alpha=0.9,
        )

        nx.draw_networkx_edges(
            self.G,
            self.pos,
            ax=self.ax,
            edgelist=edges,
            edge_color=edge_colors,  # type: ignore[arg-type]  # networkx accepts List[str]; stub expects str
            width=2.5,
            alpha=0.8,
        )

        # Draw node labels
        node_labels = self._create_node_labels(step)
        nx.draw_networkx_labels(
            self.G,
            self.pos,
            labels=node_labels,
            ax=self.ax,
            font_size=self.config.font_size,
            font_color=self.colors.text,
            font_weight="bold",
        )

        # Draw edge labels (weights) if enabled
        if self.config.show_weights:
            edge_labels = nx.get_edge_attributes(self.G, "weight")
            nx.draw_networkx_edge_labels(
                self.G,
                self.pos,
                edge_labels=edge_labels,
                ax=self.ax,
                font_size=self.config.font_size - 2,
                font_color=self.colors.text,
            )

        # Create title with step information
        progress_percent = (frame_num + 1) / len(self.steps) * 100
        title = (
            f"Dijkstra's Algorithm Visualization\\n"
            f"Step {frame_num + 1}/{len(self.steps)} ({progress_percent:.1f}%) | "
            f"State: {step.state.name}\\n"
            f"{step.message}"
        )

        self.ax.set_title(
            title,
            loc="left",
            pad=20,
            fontsize=self.config.font_size + 2,
            color=self.colors.text,
            weight="bold",
        )

        # Remove axes for cleaner look
        self.ax.set_xticks([])
        self.ax.set_yticks([])

        # Progress information
        if self.config.show_progress and frame_num % 5 == 0:
            logger.debug(f"Animation progress: {progress_percent:.1f}%")

        return []  # Return empty list for FuncAnimation

    def animate(self, save_path: Optional[Path] = None) -> None:
        """
        Create and display the animation.

        Args:
            save_path: Optional path to save the animation as video
        """
        logger.info(f"Starting animation with {len(self.steps)} frames")
        logger.info(f"Animation speed: {self.config.animation_speed}ms per frame")

        # Create animation
        anim = FuncAnimation(
            self.fig,
            self._update,
            frames=len(self.steps),
            interval=self.config.animation_speed,
            repeat=False,
            blit=False,  # Disable blitting for better compatibility
        )

        # Save animation if requested
        if save_path or self.config.save_animation:
            output_path = (
                save_path or self.config.output_path or Path("dijkstra_animation.mp4")
            )
            logger.info(f"Saving animation to {output_path}")
            try:
                fps = int(1000 / self.config.animation_speed)
                anim.save(str(output_path), writer="pillow", fps=fps)
                logger.info(f"Animation saved successfully to {output_path}")
            except Exception as e:
                logger.error(f"Failed to save animation: {e}")

        # Display the animation
        plt.tight_layout()
        plt.show()

    def get_statistics(self) -> Dict[str, Any]:
        """
        Get statistics about the visualization.

        Returns:
            Dictionary containing visualization statistics
        """
        return {
            "total_steps": len(self.steps),
            "nodes": self.G.number_of_nodes(),
            "edges": self.G.number_of_edges(),
            "start_node": self.start_node,
            "theme": self.config.theme.value,
            "animation_speed": self.config.animation_speed,
            "estimated_duration": len(self.steps) * self.config.animation_speed / 1000,
        }


def create_argument_parser() -> argparse.ArgumentParser:
    """
    Create and configure the command-line argument parser.

    Returns:
        Configured ArgumentParser instance with comprehensive options
    """
    parser = argparse.ArgumentParser(
        description="🔗 Interactive Visualization of Dijkstra's Shortest Path Algorithm",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s -s A                    # Visualize from node A (default)
  %(prog)s -s B --speed fast       # Fast animation from node B  
  %(prog)s -s C --theme dark       # Dark theme visualization
  %(prog)s -s D --save output.gif  # Save animation to file
  %(prog)s --list-nodes            # Show available nodes in example graph

Themes:
  default     - Standard color scheme with bright colors
  dark        - Dark background with high contrast colors  
  colorblind  - Colorblind-friendly palette
  minimal     - Minimal monochrome design

Animation Speeds:
  very_slow (3000ms)  slow (2000ms)  normal (1200ms)  fast (800ms)  very_fast (400ms)

Note: This visualization requires 'dijkstra.py' in the same directory.
      Example graph has nodes: {nodes}
        """.format(
            nodes=", ".join(sorted(example_graph.keys()))
        ),
    )

    # Core visualization arguments
    parser.add_argument(
        "-s",
        "--start",
        type=str,
        default="A",
        help="Starting node for the algorithm (default: %(default)s)",
    )

    parser.add_argument(
        "--list-nodes",
        action="store_true",
        help="List all available nodes in the example graph and exit",
    )

    # Animation control arguments
    speed_group = parser.add_mutually_exclusive_group()
    speed_group.add_argument(
        "--speed",
        type=str,
        choices=["very_slow", "slow", "normal", "fast", "very_fast"],
        default="normal",
        help="Animation speed (default: %(default)s)",
    )

    speed_group.add_argument(
        "--interval",
        type=int,
        metavar="MS",
        help="Custom animation interval in milliseconds",
    )

    # Visual appearance arguments
    parser.add_argument(
        "--theme",
        type=str,
        choices=[theme.value for theme in VisualizationTheme],
        default=VisualizationTheme.DEFAULT.value,
        help="Color theme for visualization (default: %(default)s)",
    )

    parser.add_argument(
        "--size",
        type=str,
        default="12x8",
        help="Figure size as WIDTHxHEIGHT (default: %(default)s)",
    )

    parser.add_argument(
        "--no-weights", action="store_true", help="Hide edge weight labels"
    )

    # Output arguments
    parser.add_argument(
        "--save",
        type=Path,
        metavar="FILE",
        help="Save animation to file (supports .gif, .mp4)",
    )

    parser.add_argument(
        "--no-progress",
        action="store_true",
        help="Disable progress logging during animation",
    )

    # Debugging arguments
    parser.add_argument(
        "-v", "--verbose", action="store_true", help="Enable verbose logging"
    )

    parser.add_argument(
        "--stats",
        action="store_true",
        help="Show visualization statistics before starting",
    )

    return parser


def parse_figure_size(size_str: str) -> Tuple[int, int]:
    """
    Parse figure size string into width and height.

    Args:
        size_str: Size string in format "WIDTHxHEIGHT"

    Returns:
        Tuple of (width, height)

    Raises:
        ValueError: If size string is invalid
    """
    try:
        parts = size_str.lower().split("x")
        if len(parts) != 2:
            raise ValueError("Size must be in format WIDTHxHEIGHT")

        width, height = int(parts[0]), int(parts[1])

        if width < 4 or height < 3:
            raise ValueError("Figure size must be at least 4x3")
        if width > 30 or height > 20:
            raise ValueError("Figure size cannot exceed 30x20")

        return (width, height)
    except ValueError as e:
        if "invalid literal" in str(e):
            raise ValueError("Size dimensions must be integers")
        raise


def validate_arguments(args: argparse.Namespace) -> None:
    """
    Validate command line arguments.

    Args:
        args: Parsed command line arguments

    Raises:
        ValueError: If arguments are invalid
    """
    # Validate start node
    if args.start.upper() not in example_graph:
        available_nodes = ", ".join(sorted(example_graph.keys()))
        raise ValueError(
            f"Start node '{args.start}' not found in graph. "
            f"Available nodes: {available_nodes}"
        )

    # Validate save path extension if provided
    if args.save:
        valid_extensions = {".gif", ".mp4", ".avi", ".mov"}
        if args.save.suffix.lower() not in valid_extensions:
            raise ValueError(
                f"Save format '{args.save.suffix}' not supported. "
                f"Use: {', '.join(valid_extensions)}"
            )


def setup_logging(verbose: bool) -> None:
    """
    Configure logging based on verbosity level.

    Args:
        verbose: Whether to enable verbose logging
    """
    level = logging.DEBUG if verbose else logging.INFO

    # Update existing logger
    logger.setLevel(level)

    # Update root logger for matplotlib/networkx messages
    if verbose:
        logging.getLogger().setLevel(logging.INFO)
    else:
        # Suppress verbose output from dependencies
        logging.getLogger("matplotlib").setLevel(logging.WARNING)
        logging.getLogger("PIL").setLevel(logging.WARNING)


def main() -> None:
    """
    Main function to run the Dijkstra algorithm visualizer.

    This function handles command-line argument parsing, configuration setup,
    and runs the interactive visualization.
    """
    verbose_mode = False

    try:
        # Parse command line arguments
        parser = create_argument_parser()
        args = parser.parse_args()
        verbose_mode = args.verbose

        # Setup logging
        setup_logging(args.verbose)

        # Handle list-nodes option
        if args.list_nodes:
            print("📋 Available nodes in the example graph:")
            print("=" * 40)
            for node, neighbors in sorted(example_graph.items()):
                neighbor_info = [f"{n}({w})" for n, w in neighbors.items()]
                connections = (
                    ", ".join(neighbor_info) if neighbor_info else "no connections"
                )
                print(f"  🔹 {node}: {connections}")
            return

        # Validate arguments
        validate_arguments(args)

        # Parse configuration
        start_node = args.start.upper()

        # Determine animation speed
        if args.interval:
            animation_speed = args.interval
        else:
            speed_map = {
                "very_slow": AnimationSpeed.VERY_SLOW.value,
                "slow": AnimationSpeed.SLOW.value,
                "normal": AnimationSpeed.NORMAL.value,
                "fast": AnimationSpeed.FAST.value,
                "very_fast": AnimationSpeed.VERY_FAST.value,
            }
            animation_speed = speed_map[args.speed]

        # Parse figure size
        figure_size = parse_figure_size(args.size)

        # Create visualization configuration
        config = VisualizationConfig(
            theme=VisualizationTheme(args.theme),
            animation_speed=animation_speed,
            figure_size=figure_size,
            show_weights=not args.no_weights,
            show_progress=not args.no_progress,
            save_animation=bool(args.save),
            output_path=args.save,
        )

        logger.info(f"🚀 Starting Dijkstra visualization from node '{start_node}'")
        logger.info(f"📊 Configuration: {args.theme} theme, {args.speed} speed")

        # Create and run visualizer
        visualizer = DijkstraVisualizer(example_graph, start_node, config)

        # Show statistics if requested
        if args.stats:
            stats = visualizer.get_statistics()
            print("\\n📈 Visualization Statistics:")
            print("=" * 30)
            for key, value in stats.items():
                print(f"  {key.replace('_', ' ').title()}: {value}")
            print()

        # Start animation
        visualizer.animate()

        logger.info("✅ Visualization completed successfully")

    except KeyboardInterrupt:
        logger.warning("🛑 Visualization interrupted by user")
        print("\\n👋 Visualization cancelled.")

    except Exception as e:
        logger.error(f"❌ Error: {e}")
        print(f"Error: {e}", file=sys.stderr)

        if verbose_mode:
            import traceback

            traceback.print_exc()

        sys.exit(1)


if __name__ == "__main__":
    main()
