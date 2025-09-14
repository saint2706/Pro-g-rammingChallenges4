package Emulation.EulerianPath;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Implementation of Fleury's Algorithm to produce an Eulerian trail (or
 * circuit)
 * in an undirected graph if one exists.
 *
 * <p>
 * Algorithm overview:
 * <ol>
 * <li>Choose a starting vertex: if graph has 0 or 2 vertices of odd degree,
 * start at an odd-degree vertex (trail case) otherwise any vertex with edges
 * (circuit case).</li>
 * <li>At each step pick the next edge that is NOT a bridge unless it is the
 * only remaining edge.</li>
 * <li>Remove the edge and move to the adjacent vertex, recording
 * traversal.</li>
 * </ol>
 * Complexity is O(E^2) in the worst case because each bridge test can be O(E),
 * and we perform one for (almost) each edge. For large graphs consider
 * Hierholzer's algorithm instead.
 */
public class Fleury {
    private final int vertexCount;
    private final List<Integer>[] adj; // adjacency lists (undirected, simple graph)

    @SuppressWarnings("unchecked")
    public Fleury(int vertexCount) {
        if (vertexCount <= 0)
            throw new IllegalArgumentException("vertexCount must be positive");
        this.vertexCount = vertexCount;
        this.adj = new ArrayList[vertexCount];
        for (int i = 0; i < vertexCount; i++) {
            adj[i] = new ArrayList<>();
        }
    }

    /** Add an undirected edge between u and v. */
    public void addEdge(int u, int v) {
        validateVertex(u);
        validateVertex(v);
        if (u == v)
            throw new IllegalArgumentException("Self loops not supported in this simple implementation");
        adj[u].add(v);
        adj[v].add(u);
    }

    /** Remove a single undirected edge between u and v (one incidence). */
    private void removeEdge(int u, int v) {
        adj[u].remove((Integer) v);
        adj[v].remove((Integer) u);
    }

    private void validateVertex(int v) {
        if (v < 0 || v >= vertexCount) {
            throw new IllegalArgumentException("Vertex out of range: " + v);
        }
    }

    /**
     * Compute an Eulerian trail or circuit using Fleury's algorithm.
     * 
     * @return list of vertices in visitation order (length edges+1) or empty list
     *         if graph disconnected or no trail exists.
     */
    public List<Integer> eulerTrail() {
        if (!isGraphConnectedIgnoringIsolated()) {
            return Collections.emptyList();
        }
        int oddCount = 0;
        int start = 0;
        for (int v = 0; v < vertexCount; v++) {
            if (adj[v].size() % 2 == 1) {
                oddCount++;
                start = v; // last seen odd; first is fine
            }
        }
        if (!(oddCount == 0 || oddCount == 2)) {
            return Collections.emptyList(); // no Eulerian trail/circuit
        }
        if (oddCount == 0) {
            // choose any vertex with edges
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

    private void fleuryDFS(int u, List<Integer> path) {
        for (int i = 0; i < adj[u].size();) { // manual index so we handle removal
            int v = adj[u].get(i);
            if (isValidNextEdge(u, v)) {
                removeEdge(u, v);
                path.add(u);
                fleuryDFS(v, path);
            } else {
                i++; // try next edge
            }
        }
        if (path.isEmpty() || path.get(path.size() - 1) != u) {
            path.add(u); // ensure last vertex appended
        }
    }

    /**
     * Edge validity test: an edge is valid if it's the only one left or NOT a
     * bridge.
     */
    private boolean isValidNextEdge(int u, int v) {
        if (adj[u].size() == 1)
            return true; // only choice
        int countBefore = reachableVertexCount(u);
        removeEdge(u, v);
        int countAfter = reachableVertexCount(u);
        // restore
        addEdge(u, v);
        // if removing the edge reduces reachable vertices, it was a bridge
        return countAfter == countBefore;
    }

    private int reachableVertexCount(int start) {
        boolean[] visited = new boolean[vertexCount];
        return dfsCount(start, visited);
    }

    private int dfsCount(int v, boolean[] visited) {
        visited[v] = true;
        int c = 1;
        for (int w : adj[v]) {
            if (!visited[w])
                c += dfsCount(w, visited);
        }
        return c;
    }

    private boolean isGraphConnectedIgnoringIsolated() {
        boolean[] visited = new boolean[vertexCount];
        int start = -1;
        for (int v = 0; v < vertexCount; v++) {
            if (!adj[v].isEmpty()) {
                start = v;
                break;
            }
        }
        if (start == -1)
            return true; // no edges
        dfsCount(start, visited);
        for (int v = 0; v < vertexCount; v++) {
            if (!adj[v].isEmpty() && !visited[v])
                return false;
        }
        return true;
    }

    public static void main(String[] args) {
        // Example similar to original but corrected (original code added edge with
        // vertex 5 on a size-5 graph)
        Fleury g = new Fleury(6);
        g.addEdge(0, 1);
        g.addEdge(2, 0);
        g.addEdge(1, 2);
        g.addEdge(3, 0);
        g.addEdge(4, 3);
        g.addEdge(5, 0);

        List<Integer> trail = g.eulerTrail();
        if (trail.isEmpty()) {
            System.out.println("No Eulerian trail/circuit.");
        } else {
            System.out.println("Eulerian traversal: " + trail);
        }
    }
}