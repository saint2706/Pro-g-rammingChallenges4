#include <iostream>
#include <list>
using namespace std;

class Graph {
  // Number of vertices in the graph
  int V;
  // Dynamic array containing adjacency lists
  list<int> *adj;

public:
  Graph(int V) {
    this->V = V;
    adj = new list<int>[V];
  }
  // lol mem leak
  ~Graph() { delete[] adj; }

  void addEdge(int v, int w);
  int isEulerian();
  bool isConnected();
  void DFSUtil(int v, bool visited[]);
};

void Graph::addEdge(int v, int w) {
  // Undirected graph
  adj[v].push_back(w);
  adj[w].push_back(v);
}

void Graph::DFSUtil(int v, bool visited[]) {
  // Mark the current node as visited and print it
  visited[v] = true;

  // Recur for all the vertices adjacent to this vertex
  list<int>::iterator i;
  for (i = adj[v].begin(); i != adj[v].end(); ++i)
    if (!visited[*i])
      DFSUtil(*i, visited);
}

bool Graph::isConnected() {
  bool visited[V];
  int i;
  for (i = 0; i < V; i++) {
    visited[i] = false;
  }

  for (i = 0; i < V; i++) {
    if (adj[i].size() != 0) {
      break;
    }
  }

  if (i == V) {
    return true;
  }

  DFSUtil(i, visited);

  for (i = 0; i < V; i++) {
    if (visited[i] == false && adj[i].size() > 0) {
      return false;
    }
  }
  return true;
}

/*
    0 -> if not eulerian
    1 -> if semi-eulerian
    2 -> if euler circuit
*/
int Graph::isEulerian() {
  if (!isConnected()) {
    return 0;
  }

  int odd = 0;

  for (int i = 0; i < V; i++) {
    if (adj[i].size() & 1) {
      odd++;
    }
  }

  if (odd > 2) {
    return 0;
  }

  return (odd) ? 1 : 2;
}

void test(Graph &g) {
  int n = g.isEulerian();
  if (n == 0) {
    cout << "Not Eulerian" << endl;
  } else if (n == 1) {
    cout << "Semi-Eulerian" << endl;
  } else if (n == 2) {
    cout << "Eulerian" << endl;
  }
}

int main() {
  Graph g(6);
  g.addEdge(0, 1);
  g.addEdge(2, 0);
  g.addEdge(1, 2);
  g.addEdge(3, 0);
  g.addEdge(4, 3);
  g.addEdge(5, 0);
  test(g);

  Graph g1(5);
  g1.addEdge(0, 1);
  g1.addEdge(2, 0);
  g1.addEdge(2, 1);
  g1.addEdge(3, 0);
  g1.addEdge(4, 3);
  g1.addEdge(0, 4);
  test(g1);

  Graph g2(3);
  g2.addEdge(0, 1);
  g2.addEdge(1, 2);
  g2.addEdge(2, 0);
  test(g2);

  return 0;
}