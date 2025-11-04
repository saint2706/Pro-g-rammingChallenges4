# Basic Neural Network

This project is a minimal, educational implementation of a multi-layer perceptron (MLP) from scratch using NumPy. It is designed to be a learning tool for understanding the fundamentals of neural networks, including forward and backward propagation, activation functions, and gradient descent.

## Features

- **Single Hidden Layer**: A simple architecture with one hidden layer, making it easy to understand and debug.
- **Configurable**: The number of hidden units, learning rate, and number of epochs can be configured through the command line.
- **Activation Functions**: Uses the hyperbolic tangent (tanh) activation function for the hidden layer and the sigmoid function for the output layer, suitable for binary classification tasks.
- **Binary Cross-Entropy Loss**: Implements binary cross-entropy loss, a common loss function for binary classification problems.
- **Visualization**: Plots the training error progression, allowing you to visualize the learning process.

## Code Structure

- **`BNN.py`**: The main file, containing the entire implementation of the neural network.
  - **`sigmoid(x)`**: The sigmoid activation function.
  - **`tanh(x)`**: The hyperbolic tangent activation function.
  - **`tanh_derivative(output)`**: The derivative of the tanh function.
  - **`NNConfig`**: A data class for configuring the neural network.
  - **`NeuralNetwork`**: The main class that encapsulates the neural network's logic.
    - **`__init__(...)`**: Initializes the network's weights and biases.
    - **`forward()`**: Performs the forward pass.
    - **`backward()`**: Performs the backward pass and updates the weights.
    - **`train(...)`**: Trains the network for a specified number of epochs.
    - **`predict(...)`**: Makes predictions on new data.
  - **`main()`**: The main function that parses command-line arguments, creates a neural network, trains it, and visualizes the results.

## Usage

To run the script, use the following command:

```bash
python BNN.py [OPTIONS]
```

### Options

- `--epochs`: The number of training epochs (default: 10000).
- `--lr`: The learning rate (default: 0.1).
- `--seed`: A random seed for reproducibility (default: None).
- `--hidden-units`: The number of units in the hidden layer (default: 4).
- `--log-interval`: How often to log progress in epochs (default: 1000).

### Example

```bash
python BNN.py --epochs 10000 --lr 0.1 --seed 42
```

## How it Works

The neural network is trained on a simple XOR-like dataset. The training process consists of the following steps:

1. **Initialization**: The weights and biases of the network are initialized with random values.
2. **Forward Propagation**: The input data is fed through the network, and the output is calculated.
3. **Loss Calculation**: The binary cross-entropy loss between the predicted output and the actual output is calculated.
4. **Backward Propagation**: The gradients of the loss with respect to the weights and biases are calculated using the chain rule.
5. **Weight Update**: The weights and biases are updated using gradient descent.

This process is repeated for a specified number of epochs, and the loss is plotted to visualize the learning process.
