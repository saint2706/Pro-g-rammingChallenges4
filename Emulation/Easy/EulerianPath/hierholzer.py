def print_circle(adj):
    if len(adj) == 0:
        return
    current_path = [0]
    circuit = []

    while current_path:
        curr_v = current_path[-1]
        if adj[curr_v]:
            next_v = adj[curr_v].pop()
            current_path.append(next_v)
        else:
            circuit.append(current_path.pop())

    for i in range(len(circuit) - 1, -1, -1):
        print(circuit[i], end=" ")
        if i:
            print("->", end="")


adj1 = [[] for _ in range(6)]
adj1[0].append(1)
adj1[1].append(2)
adj1[2].append(3)
adj1[3].append(4)
adj1[4].append(5)
adj1[5].append(0)
print_circle(adj1)
