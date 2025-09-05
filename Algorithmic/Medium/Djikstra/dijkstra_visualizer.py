import networkx as nx
import matplotlib.pyplot as plt
from matplotlib.animation import FuncAnimation
import argparse
import sys

# Import the generator function and the example graph
try:
    from dijkstra import dijkstra_stepper, example_graph
except ImportError:
    print("Error: This script requires 'dijkstra.py' to be in the same directory.", file=sys.stderr)
    sys.exit(1)

class DijkstraVisualizer:
    """
    Animates the step-by-step execution of Dijkstra's algorithm on a graph.
    """
    def __init__(self, graph: dict, start_node: str):
        if start_node not in graph:
            raise ValueError(f"Start node '{start_node}' not found in the graph.")

        self.graph_dict = graph
        self.start_node = start_node
        self.G = nx.Graph()
        self._setup_graph()

        # Get the sequence of algorithm states
        self.steps = list(dijkstra_stepper(self.graph_dict, self.start_node))

        # Setup the plot
        self.fig, self.ax = plt.subplots(figsize=(12, 8))
        self.pos = nx.spring_layout(self.G, seed=42) # Seed for reproducible layout

    def _setup_graph(self):
        """Initializes the networkx graph from the dictionary."""
        for node, edges in self.graph_dict.items():
            for neighbor, weight in edges.items():
                self.G.add_edge(node, neighbor, weight=weight)

    def _update(self, frame_num: int):
        """The update function for the animation frame."""
        self.ax.clear()
        state = self.steps[frame_num]

        current_node = state["current_node"]
        distances = state["distances"]
        previous_nodes = state["previous_nodes"]
        queue = state["queue"]
        visited = state["visited"]
        message = state["message"]

        # --- Determine Node Colors ---
        node_colors = []
        for node in self.G.nodes():
            if node == current_node:
                node_colors.append('red') # Current node being processed
            elif node in visited:
                node_colors.append('gray') # Visited nodes
            elif node in queue:
                node_colors.append('skyblue') # In the priority queue
            else:
                node_colors.append('lightgray')

        # --- Determine Edge Colors ---
        path_edges = []
        for node, prev_node in previous_nodes.items():
            if prev_node is not None:
                path_edges.append((prev_node, node))

        edge_colors = ['green' if edge in path_edges or (edge[1], edge[0]) in path_edges else 'black' for edge in self.G.edges()]

        # --- Draw Graph ---
        nx.draw(self.G, self.pos, ax=self.ax, with_labels=False, node_color=node_colors, edge_color=edge_colors, width=2)

        # --- Draw Labels ---
        node_labels = {node: f"{node}\n({dist if dist != float('inf') else '∞'})" for node, dist in distances.items()}
        nx.draw_networkx_labels(self.G, self.pos, labels=node_labels, ax=self.ax, font_color='black')

        edge_labels = nx.get_edge_attributes(self.G, 'weight')
        nx.draw_networkx_edge_labels(self.G, self.pos, edge_labels=edge_labels, ax=self.ax)

        self.ax.set_title(f"Dijkstra's Algorithm | Step {frame_num + 1}/{len(self.steps)}\n{message}", loc='left')

    def animate(self):
        """Creates and shows the animation."""
        anim = FuncAnimation(self.fig, self._update, frames=len(self.steps), interval=1200, repeat=False)
        plt.show()

def main():
    """Main function to run the visualizer."""
    parser = argparse.ArgumentParser(description="Visualize Dijkstra's shortest path algorithm.")
    parser.add_argument("-s", "--start", default="A",
                        help=f"The start node. Available nodes: {', '.join(example_graph.keys())}")

    args = parser.parse_args()

    if args.start.upper() not in example_graph:
        print(f"Error: Start node '{args.start}' is not in the example graph.", file=sys.stderr)
        sys.exit(1)

    try:
        visualizer = DijkstraVisualizer(example_graph, args.start.upper())
        visualizer.animate()
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)

if __name__ == "__main__":
    main()
