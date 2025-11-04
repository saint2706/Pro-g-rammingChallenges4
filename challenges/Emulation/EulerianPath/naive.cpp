/**
 * @file naive.cpp
 * @brief A naive implementation for classifying Eulerian graphs.
 *
 * This file contains a C++ implementation that checks if a graph has an
 * Eulerian path or circuit. It does so by checking the connectivity of the
 * graph and the degree of its vertices.
 */

#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <string>
#include <stdexcept>

/**
 * @class Graph
 * @brief Represents an undirected graph and provides methods for Eulerian classification.
 */
class Graph
{
public:
  /**
   * @enum EulerianType
   * @brief An enumeration for the different types of Eulerian classifications.
   */
  enum class EulerianType
  {
    NotEulerian = 0,
    Trail = 1,
    Circuit = 2
  };

  /**
   * @brief Constructs a graph with a given number of vertices.
   * @param vertices The number of vertices in the graph.
   */
  explicit Graph(int vertices) : V(vertices), adj(vertices)
  {
    if (vertices <= 0)
      throw std::invalid_argument("Vertex count must be positive");
  }

  /**
   * @brief Adds an undirected edge between two vertices.
   * @param u The first vertex.
   * @param v The second vertex.
   */
  void addEdge(int u, int v)
  {
    validateVertex(u);
    validateVertex(v);
    if (u == v)
    {
      throw std::invalid_argument("Self-loops are not supported");
    }
    adj[u].push_back(v);
    adj[v].push_back(u);
  }

  /**
   * @brief Classifies the graph as Eulerian, Semi-Eulerian, or not Eulerian.
   * @return The EulerianType of the graph.
   */
  EulerianType classify() const
  {
    if (!isConnectedIgnoringIsolated())
      return EulerianType::NotEulerian;
    int odd = 0;
    for (int v = 0; v < V; ++v)
    {
      if (adj[v].size() % 2 != 0)
        ++odd;
    }
    if (odd == 0)
      return EulerianType::Circuit;
    if (odd == 2)
      return EulerianType::Trail;
    return EulerianType::NotEulerian;
  }

  /**
   * @brief Converts an EulerianType to a string.
   * @param t The EulerianType to convert.
   * @return A string representation of the EulerianType.
   */
  static std::string toString(EulerianType t)
  {
    switch (t)
    {
    case EulerianType::NotEulerian:
      return "Not Eulerian";
    case EulerianType::Trail:
      return "Semi-Eulerian (Trail)";
    case EulerianType::Circuit:
      return "Eulerian Circuit";
    }
    return "Unknown";
  }

private:
  int V;
  std::vector<std::vector<int>> adj;

  /**
   * @brief Validates that a vertex is within the valid range.
   * @param v The vertex to validate.
   */
  void validateVertex(int v) const
  {
    if (v < 0 || v >= V)
      throw std::out_of_range("Vertex out of range");
  }

  /**
   * @brief Checks if the graph is connected, ignoring isolated vertices.
   * @return True if the graph is connected, false otherwise.
   */
  bool isConnectedIgnoringIsolated() const
  {
    int start = -1;
    for (int v = 0; v < V; ++v)
    {
      if (!adj[v].empty())
      {
        start = v;
        break;
      }
    }
    if (start == -1)
      return true; // A graph with no edges is considered connected.

    std::vector<bool> visited(V, false);
    dfs(start, visited);
    for (int v = 0; v < V; ++v)
    {
      if (!adj[v].empty() && !visited[v])
        return false;
    }
    return true;
  }

  /**
   * @brief A recursive helper for the depth-first search.
   * @param v The current vertex.
   * @param visited A vector to keep track of visited vertices.
   */
  void dfs(int v, std::vector<bool> &visited) const
  {
    visited[v] = true;
    for (int w : adj[v])
    {
      if (!visited[w])
        dfs(w, visited);
    }
  }
};

/**
 * @brief A helper function to test and print the classification of a graph.
 * @param g The graph to test.
 */
static void test(const Graph &g)
{
  std::cout << Graph::toString(g.classify()) << '\n';
}

/**
 * @brief The main entry point for the program.
 * @return 0 on success.
 */
int main()
{
  // Example 1: A graph with an Eulerian trail.
  Graph g1(6);
  g1.addEdge(0, 1);
  g1.addEdge(2, 0);
  g1.addEdge(1, 2);
  g1.addEdge(3, 0);
  g1.addEdge(4, 3);
  g1.addEdge(5, 0);
  test(g1);

  // Example 2: A graph that is not Eulerian.
  Graph g2(5);
  g2.addEdge(0, 1);
  g2.addEdge(2, 0);
  g2.addEdge(2, 1);
  g2.addEdge(3, 0);
  g2.addEdge(4, 3);
  g2.addEdge(0, 4);
  test(g2);

  // Example 3: A graph with an Eulerian circuit.
  Graph g3(3);
  g3.addEdge(0, 1);
  g3.addEdge(1, 2);
  g3.addEdge(2, 0);
  test(g3);

  return 0;
}
