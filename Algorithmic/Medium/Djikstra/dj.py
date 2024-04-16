import heapq


def dijkstra(graph, start):
    distances = {node: float("infinity") for node in graph}
    distances[start] = 0
    pq = [(0, start)]

    while pq:
        current_dist, current_node = heapq.heappop(pq)

        if current_dist > distances[current_node]:
            continue

        for neighbor, weight in graph[current_node].items():
            distance = current_dist + weight

            if distance < distances[neighbor]:
                distances[neighbor] = distance
                heapq.heappush(pq, (distance, neighbor))

    return distances


# Example graph as an adjacency list
graph = {
    "A": {"B": 1, "C": 4},
    "B": {"A": 1, "C": 2, "D": 5},
    "C": {"A": 4, "B": 2, "D": 1},
    "D": {"B": 5, "C": 1},
}

start_node = "A"
shortest_distances = dijkstra(graph, start_node)

print("Shortest distances from node", start_node)
for node, distance in shortest_distances.items():
    print(f"Node {node}: Distance {distance}")
