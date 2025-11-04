# Real Neural Network (Challenge #75)

This project implements a configurable multilayer perceptron (MLP) using
NumPy matrix operations only.  It can be trained on MNIST from
OpenML and includes scripts for evaluation and model serialization.

## Features

- Arbitrary depth network with ReLU/Tanh/Sigmoid hidden activations
- Mini-batch gradient descent with optional L2 regularisation
- Softmax output layer with cross-entropy loss
- Checkpoint saving/loading via compressed `.npz` files
- Training history export for plotting

## Usage

Install the AI extras to grab the required dependencies:

```bash
python -m pip install -e .[ai,developer]
```

### Training

```bash
python "challenges/Artificial Intelligence/Real Neural Network/train.py" \
    --hidden-layers 256 128 \
    --epochs 15 \
    --batch-size 128 \
    --learning-rate 0.01 \
    --model-out mnist-mlp.npz
```

Key hyperparameters:

- `--hidden-layers`: Sequence of hidden layer sizes (default: `128 64`)
- `--epochs`: Number of passes over the training data (default: `10`)
- `--batch-size`: Mini-batch size for gradient descent (default: `64`)
- `--learning-rate`: Optimisation step size (default: `0.01`)
- `--l2`: L2 regularisation strength (default: `0.0`)
- `--validation-split`: Fraction of the training split used for
  validation metrics (default: `0.1`)
- `--data-home`: Cache directory for downloaded datasets

Training prints per-epoch metrics, persists the model weights, and writes
a `*.history.npz` file containing the loss/accuracy curves.

### Evaluation

```bash
python "challenges/Artificial Intelligence/Real Neural Network/evaluate.py" mnist-mlp.npz
```

This reloads the saved checkpoint, recreates the MNIST split using the
same random seed, and reports loss, accuracy, and a confusion matrix.
The optional `--limit` flag constrains evaluation to the first *N*
samples of the test split for quick smoke tests.

### Module API

You can also import the `MLP` class directly for use in notebooks or
other scripts:

```python
from pathlib import Path
import numpy as np

from mlp import MLP

model = MLP([4, 8, 3], seed=0)
X = np.random.rand(32, 4)
y = np.random.randint(0, 3, size=32)
model.fit(X, y, epochs=25, batch_size=16)
model.save(Path("toy_model.npz"))
```

The weights are stored in compressed NumPy archives with keys `W0`,
`b0`, etc., making it easy to inspect or port to other frameworks.

## Code Structure

- **`mlp.py`**: This is the core of the project, containing the `MLP` class that implements the neural network. It includes methods for forward and backward propagation, training, evaluation, and serialization.
- **`data.py`**: This module provides the `load_mnist` function for downloading, caching, and loading the MNIST dataset.
- **`train.py`**: This script provides a command-line interface for training the `MLP` model on the MNIST dataset. It handles argument parsing, data loading, model initialization, and training.
- **`evaluate.py`**: This script provides a command-line interface for evaluating a trained `MLP` model. It loads a model from a checkpoint and reports its performance on the test set, including a confusion matrix.

## Notes

- The MNIST loader uses `sklearn.datasets.fetch_openml`.  Ensure
  internet access for the initial download or pass a `--data-home`
  directory containing a cached copy.
- The model operates entirely in NumPy and is intended for educational
  purposes rather than state-of-the-art accuracy.
