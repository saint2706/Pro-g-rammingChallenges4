import numpy as np
import matplotlib.pyplot as plt

inps = np.array([[0, 1, 0], [0, 1, 1], [1, 0, 0], [1, 1, 0], [1, 1, 1], [1, 0, 1]])
outs = np.array([[0], [1], [1], [0], [1], [0]])


def sigmoid(x, der=False):
    if der:
        return x * (1 - x)
    return 1 / (1 + np.exp(-x))


class NeuralNetwork:
    def __init__(self, inputs, outputs):
        self.hidden = None
        self.inps = inputs
        self.outs = outputs
        self.weights = np.array([[0.50], [0.50], [0.50]])
        self.error = []
        self.epochs = []

    # S(x) = 1/1+e^(-x)

    # Forward Propagation
    def forward(self):
        self.hidden = sigmoid(np.dot(self.inps, self.weights))

    # Backward Propagation
    def backward(self):
        self.error = self.outs - self.hidden
        delta = self.error * sigmoid(self.hidden, der=True)
        self.weights += np.dot(self.inps.T, delta)

    # Train the Neural Network
    def train(self, epochs=25000):
        for i in range(epochs):
            self.forward()
            self.backward()
            self.epochs.append(i)
            np.append(self.error, np.mean(np.abs(self.error)))

    # Predict the output
    def predict(self, inp):
        return sigmoid(np.dot(inp, self.weights))


neuralNetwork = NeuralNetwork(inps, outs)
neuralNetwork.train(epochs=len(inps))

test1 = np.array([[0, 0, 0]])
test2 = np.array([[0, 0, 1]])

print(neuralNetwork.predict(test1), "\nCorrect Answer:", test1[0][0])
print(neuralNetwork.predict(test2), "\nCorrect Answer:", test2[0][0])

plt.figure(figsize=(15, 5))
plt.plot(neuralNetwork.epochs, neuralNetwork.error)
plt.xlabel("Epochs")
plt.ylabel("Error")
plt.show()
