/**
 * @file fleury.java
 * @brief An implementation of Fleury's algorithm for finding an Eulerian path.
 *
 * This file contains a Java implementation of Fleury's algorithm, which finds
 * an Eulerian path or circuit in a graph.
 */
package Emulation.EulerianPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Implements Fleury's Algorithm to find an Eulerian trail or circuit in an
 * undirected graph.
 *
 * <p>Algorithm overview:
 * <ol>
 *   <li>Verify that the graph has an Eulerian path (at most two vertices with odd degree).</li>
 *   <li>Choose a starting vertex.</li>
 *   <li>At each step, choose an edge that is not a bridge unless there is no other choice.</li>
 *   <li>Remove the chosen edge and move to the next vertex.</li>
 *   <li>Repeat until all edges have been traversed.</li>
 * </ol>
 *
 * <p>The complexity of this implementation is O(E^2), where E is the number of edges.
 */
public class fleury {
  private final int vertexCount;
  private final List<Integer>[] adj;

  /**
   * Constructs a graph with a given number of vertices.
   *
   * @param vertexCount The number of vertices in the graph.
   */
  @SuppressWarnings("unchecked")
  public fleury(int vertexCount) {
    if (vertexCount <= 0) throw new IllegalArgumentException("Vertex count must be positive.");
    this.vertexCount = vertexCount;
    this.adj = new ArrayList[vertexCount];
    for (int i = 0; i < vertexCount; i++) {
      adj[i] = new ArrayList<>();
    }
  }

  /**
   * Adds an undirected edge between two vertices.
   *
   * @param u The first vertex.
   * @param v The second vertex.
   */
  public void addEdge(int u, int v) {
    validateVertex(u);
    validateVertex(v);
    if (u == v) throw new IllegalArgumentException("Self-loops are not supported.");
    adj[u].add(v);
    adj[v].add(u);
  }

  /**
   * Removes an undirected edge between two vertices.
   *
   * @param u The first vertex.
   * @param v The second vertex.
   */
  private void removeEdge(int u, int v) {
    adj[u].remove(Integer.valueOf(v));
    adj[v].remove(Integer.valueOf(u));
  }

  /**
   * Validates that a vertex is within the valid range.
   *
   * @param v The vertex to validate.
   */
  private void validateVertex(int v) {
    if (v < 0 || v >= vertexCount) {
      throw new IllegalArgumentException("Vertex " + v + " is out of bounds.");
    }
  }

  /**
   * Computes an Eulerian trail or circuit using Fleury's algorithm.
   *
   * @return A list of vertices in the order of the trail, or an empty list if none exists.
   */
  public List<Integer> eulerTrail() {
    if (!isGraphConnectedIgnoringIsolated()) {
      return Collections.emptyList();
    }
    int oddCount = 0;
    int start = 0;
    for (int v = 0; v < vertexCount; v++) {
      if (adj[v].size() % 2 != 0) {
        oddCount++;
        start = v;
      }
    }
    if (oddCount > 2) {
      return Collections.emptyList();
    }
    if (oddCount == 0) {
      for (int v = 0; v < vertexCount; v++) {
        if (!adj[v].isEmpty()) {
          start = v;
          break;
        }
      }
    }
    List<Integer> path = new ArrayList<>();
    fleuryDFS(start, path);
    return path;
  }

  /**
   * A recursive helper function to find the Eulerian path.
   *
   * @param u The current vertex.
   * @param path The list to store the path.
   */
  private void fleuryDFS(int u, List<Integer> path) {
    for (int i = 0; i < adj[u].size(); ) {
      int v = adj[u].get(i);
      if (isValidNextEdge(u, v)) {
        removeEdge(u, v);
        path.add(u);
        fleuryDFS(v, path);
      } else {
        i++;
      }
    }
    if (path.isEmpty() || path.get(path.size() - 1) != u) {
      path.add(u);
    }
  }

  /**
   * Checks if an edge is a valid next edge to traverse.
   * An edge is valid if it's not a bridge, unless it's the only edge left.
   *
   * @param u The current vertex.
   * @param v The next vertex.
   * @return True if the edge is a valid next edge, false otherwise.
   */
  private boolean isValidNextEdge(int u, int v) {
    if (adj[u].size() == 1) return true;
    int countBefore = reachableVertexCount(u);
    removeEdge(u, v);
    int countAfter = reachableVertexCount(u);
    addEdge(u, v);
    return countAfter >= countBefore;
  }

  /**
   * Counts the number of reachable vertices from a starting vertex.
   *
   * @param start The starting vertex.
   * @return The number of reachable vertices.
   */
  private int reachableVertexCount(int start) {
    boolean[] visited = new boolean[vertexCount];
    return dfsCount(start, visited);
  }

  /**
   * A recursive helper for counting reachable vertices.
   *
   * @param v The current vertex.
   * @param visited An array to keep track of visited vertices.
   * @return The number of reachable vertices.
   */
  private int dfsCount(int v, boolean[] visited) {
    visited[v] = true;
    int count = 1;
    for (int w : adj[v]) {
      if (!visited[w]) {
        count += dfsCount(w, visited);
      }
    }
    return count;
  }

  /**
   * Checks if the graph is connected, ignoring isolated vertices.
   *
   * @return True if the graph is connected, false otherwise.
   */
  private boolean isGraphConnectedIgnoringIsolated() {
    boolean[] visited = new boolean[vertexCount];
    int start = -1;
    for (int v = 0; v < vertexCount; v++) {
      if (!adj[v].isEmpty()) {
        start = v;
        break;
      }
    }
    if (start == -1) return true;
    dfsCount(start, visited);
    for (int v = 0; v < vertexCount; v++) {
      if (!adj[v].isEmpty() && !visited[v]) return false;
    }
    return true;
  }

  /**
   * The main method for demonstrating the Fleury's algorithm implementation.
   *
   * @param args Command-line arguments (not used).
   */
  public static void main(String[] args) {
    fleury g = new fleury(6);
    g.addEdge(0, 1);
    g.addEdge(2, 0);
    g.addEdge(1, 2);
    g.addEdge(3, 0);
    g.addEdge(4, 3);
    g.addEdge(5, 0);

    List<Integer> trail = g.eulerTrail();
    if (trail.isEmpty()) {
      System.out.println("No Eulerian trail/circuit found.");
    } else {
      System.out.println("Eulerian traversal: " + trail);
    }
  }
}
