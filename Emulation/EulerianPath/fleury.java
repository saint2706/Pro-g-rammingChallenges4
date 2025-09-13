package Emulation.Easy.EulerianPath;

import java.util.ArrayList;

public class fleury {
    private int vertices;
    private ArrayList<Integer>[] adj;

    public fleury(int vertices) {
        this.vertices = vertices;
        initGraph();
    }

    @SuppressWarnings("unchecked")
    private void initGraph() {
        adj = new ArrayList[vertices];
        for (int i = 0; i < vertices; i++) {
            adj[i] = new ArrayList<>();
        }
    }

    public void addEdge(Integer u, Integer v) {
        adj[u].add(v);
        adj[v].add(u);
    }

    public void removeEdge(Integer u, Integer v) {
        adj[u].remove(v);
        adj[v].remove(u);
    }

    private void printEulerPath() {
        Integer u = 0;
        for (int i = 0; i < vertices; i++) {
            if (adj[i].size() % 2 == 1) {
                u = i;
                break;
            }
        }

        printEulerUtil(u);
        System.out.println();
    }

    private void printEulerUtil(Integer u) {
        for (int i = 0; i < adj[u].size(); i++) {
            Integer v = adj[u].get(i);
            if (isValid(u, v)) {
                System.out.println(u + "-" + v + " ");

                removeEdge(u, v);
                printEulerUtil(v);
            }
        }
    }

    private boolean isValid(Integer u, Integer v) {
        if (adj[u].size() == 1) {
            return true;
        }

        boolean[] isVisited = new boolean[this.vertices];
        int cnt1 = dfsCount(u, isVisited);

        removeEdge(u, v);
        isVisited = new boolean[this.vertices];
        int cnt2 = dfsCount(u, isVisited);

        addEdge(u, v);
        return (cnt1 > cnt2) ? false : true;
    }

    private int dfsCount(Integer v, boolean[] isVisited) {
        isVisited[v] = true;
        int cnt = 1;
        for (int adj : adj[v]) {
            if (!isVisited[adj]) {
                cnt += dfsCount(adj, isVisited);
            }
        }
        return cnt;
    }

    public static void main(String[] args) {
        fleury g = new fleury(5);
        g.addEdge(0, 1);
        g.addEdge(2, 0);
        g.addEdge(1, 2);
        g.addEdge(3, 0);
        g.addEdge(4, 3);
        g.addEdge(5, 0);
        g.printEulerPath();
    }
}