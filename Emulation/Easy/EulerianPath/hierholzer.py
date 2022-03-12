def printCirc(adj):
    if len(adj) == 0:
        return
    currPath = [0]
    circuit = []

    while currPath:
        currV = currPath[-1]
        if adj[currV]:
            nextV = adj[currV].pop()
            currPath.append(nextV)
        else:
            circuit.append(currPath.pop())

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
printCirc(adj1)
