#include <iostream>
#include <vector>
#include <queue>
#include <algorithm>
#include <string>

// -----------------------------------------------------------------------------
// Eulerian Graph Classification & Connectedness Checker (Undirected Simple Graph)
// -----------------------------------------------------------------------------
// Modernized version of the original naive implementation.
// Improvements:
//  * Uses std::vector instead of manual dynamic arrays (RAII safety)
//  * Adds enum class EulerianType for clearer return semantics
//  * Explicit connectivity ignoring isolated vertices
//  * Clear comments and function responsibilities
//  * Avoids 'using namespace std;' to prevent namespace pollution
//  * Provides utility to print classification results
//  * Maintains O(V + E) complexity for connectivity/degree checks
//
// Eulerian definitions:
//  * Eulerian Circuit: All vertices have even degree and the graph is connected (ignoring isolated vertices)
//  * Eulerian Trail (Semi-Eulerian): Exactly 2 vertices have odd degree and the graph is connected
//  * Not Eulerian: Otherwise.
// ----------------------------------------------------------------------------/

class Graph
{
public:
  enum class EulerianType
  {
    NotEulerian = 0,
    Trail = 1,
    Circuit = 2
  };

  explicit Graph(int vertices) : V(vertices), adj(vertices)
  {
    if (vertices <= 0)
      throw std::invalid_argument("Vertex count must be positive");
  }

  void addEdge(int u, int v)
  {
    validateVertex(u);
    validateVertex(v);
    if (u == v)
    {
      throw std::invalid_argument("Self-loops not supported in this simple model");
    }
    adj[u].push_back(v);
    adj[v].push_back(u);
  }

  EulerianType classify() const
  {
    if (!isConnectedIgnoringIsolated())
      return EulerianType::NotEulerian;
    int odd = 0;
    for (int v = 0; v < V; ++v)
    {
      if (adj[v].size() % 2 == 1)
        ++odd;
    }
    if (odd == 0)
      return EulerianType::Circuit;
    if (odd == 2)
      return EulerianType::Trail;
    return EulerianType::NotEulerian;
  }

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

  void validateVertex(int v) const
  {
    if (v < 0 || v >= V)
      throw std::out_of_range("Vertex out of range");
  }

  bool isConnectedIgnoringIsolated() const
  {
    // Find a start vertex with non-zero degree
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
      return true; // no edges => trivially Eulerian

    std::vector<bool> visited(V, false);
    dfs(start, visited);
    for (int v = 0; v < V; ++v)
    {
      if (!adj[v].empty() && !visited[v])
        return false;
    }
    return true;
  }

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

// ------------------------------- Demo -------------------------------- //

static void test(const Graph &g)
{
  std::cout << Graph::toString(g.classify()) << '\n';
}

int main()
{
  {
    Graph g(6);
    g.addEdge(0, 1);
    g.addEdge(2, 0);
    g.addEdge(1, 2);
    g.addEdge(3, 0);
    g.addEdge(4, 3);
    g.addEdge(5, 0);
    test(g);
  }

  {
    Graph g(5);
    g.addEdge(0, 1);
    g.addEdge(2, 0);
    g.addEdge(2, 1);
    g.addEdge(3, 0);
    g.addEdge(4, 3);
    g.addEdge(0, 4);
    test(g);
  }

  {
    Graph g(3);
    g.addEdge(0, 1);
    g.addEdge(1, 2);
    g.addEdge(2, 0);
    test(g);
  }

  return 0;
}