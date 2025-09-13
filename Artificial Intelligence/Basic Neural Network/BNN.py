import numpy as np
import matplotlib.pyplot as plt
from typing import List

def sigmoid(x: np.ndarray, derivative: bool = False) -> np.ndarray:
    """
    The sigmoid activation function and its derivative.

    Args:
        x: The input numpy array.
        derivative: If True, computes the derivative of the sigmoid function.

    Returns:
        The result of the sigmoid function or its derivative.
    """
    if derivative:
        # Assumes the input 'x' is already the output of a sigmoid function.
        return x * (1 - x)
    return 1 / (1 + np.exp(-x))

class NeuralNetwork:
    """
    A simple single-layer neural network with one neuron.
    This network learns to predict an output based on a set of inputs.
    """
    def __init__(self, training_inputs: np.ndarray, training_outputs: np.ndarray):
        self.training_inputs = training_inputs
        self.training_outputs = training_outputs

        # Initialize weights randomly with a mean of 0.
        # The number of weights must match the number of input features.
        num_inputs = training_inputs.shape[1]
        self.weights = 2 * np.random.random((num_inputs, 1)) - 1

        self.hidden_layer_output = None

    def forward_propagation(self):
        """
        Passes inputs through the network to generate an output.
        """
        self.hidden_layer_output = sigmoid(np.dot(self.training_inputs, self.weights))

    def backward_propagation(self):
        """
        Calculates the error and adjusts the weights.
        """
        # Calculate the error (difference between desired output and predicted output)
        error = self.training_outputs - self.hidden_layer_output

        # Calculate the delta: error * derivative of sigmoid(output)
        # This determines the magnitude of the weight adjustment.
        delta = error * sigmoid(self.hidden_layer_output, derivative=True)

        # Update the weights
        # The adjustment is the dot product of the inputs (transposed) and the delta.
        self.weights += np.dot(self.training_inputs.T, delta)

        return np.mean(np.abs(error))

    def train(self, epochs: int) -> List[float]:
        """
        Trains the neural network over a specified number of epochs.

        Args:
            epochs: The number of iterations to train for.

        Returns:
            A list containing the mean absolute error at each epoch.
        """
        print(f"Training for {epochs} epochs...")
        error_history = []
        for i in range(epochs):
            self.forward_propagation()
            error = self.backward_propagation()
            error_history.append(error)
            if (i % 1000) == 0:
                print(f"  Epoch {i}, Error: {error:.4f}")
        print("Training complete.")
        return error_history

    def predict(self, inputs: np.ndarray) -> np.ndarray:
        """
        Makes a prediction for a given set of inputs.
        """
        return sigmoid(np.dot(inputs, self.weights))

def main():
    """
    Main function to set up the neural network, train it, and show the results.
    """
    # --- 1. Define the training dataset ---
    # The pattern to learn: the output is simply the value of the first input neuron.
    training_inputs = np.array([[0, 0, 1], [1, 1, 1], [1, 0, 1], [0, 1, 1]])
    training_outputs = np.array([[0], [1], [1], [0]])

    # --- 2. Create and train the neural network ---
    nn = NeuralNetwork(training_inputs, training_outputs)
    error_history = nn.train(epochs=10000)

    # --- 3. Make predictions on new data ---
    print("\n--- Predictions ---")
    print(f"Considering new situation [1, 0, 0] -> ?: {nn.predict(np.array([1, 0, 0]))[0]:.4f} (should be close to 1)")
    print(f"Considering new situation [0, 1, 0] -> ?: {nn.predict(np.array([0, 1, 0]))[0]:.4f} (should be close to 0)")

    # --- 4. Plot the training error ---
    plt.figure(figsize=(10, 6))
    plt.plot(range(len(error_history)), error_history)
    plt.title("Training Error Progression", fontsize=16)
    plt.xlabel("Epoch")
    plt.ylabel("Mean Absolute Error")
    plt.grid(True)
    plt.show()

if __name__ == "__main__":
    main()
