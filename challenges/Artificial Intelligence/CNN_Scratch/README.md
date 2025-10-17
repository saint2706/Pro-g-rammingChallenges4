# Scratch CNN for MNIST

This project implements challenge **#77 – Convolutional Neural Network without deep learning frameworks** by building and training a convolutional neural network using **only NumPy**. The model performs handwritten digit recognition on the MNIST dataset while exposing simple training and evaluation scripts.

## Architecture

The network mirrors a classic small CNN:

1. **Conv2D(1 → 8, 3×3, stride 1, padding 1)** – manual convolution implemented via an `im2col` transformation.
2. **ReLU** – element-wise non-linearity.
3. **MaxPool2D(2×2, stride 2)** – manual max pooling with recorded arg-max indices for backpropagation.
4. **Conv2D(8 → 16, 3×3, stride 1, padding 1)**.
5. **ReLU**.
6. **MaxPool2D(2×2, stride 2)**.
7. **Flatten** – reshape to a dense feature vector.
8. **Dense(16·7·7 → 64)**.
9. **ReLU**.
10. **Dense(64 → 10)** producing logits for the ten classes.

The loss is the categorical cross-entropy with softmax, and parameters are updated via stochastic gradient descent.

### Convolution math

For each spatial location `(i, j)` in the output feature map the convolution layer computes

\[
Y_{b, k, i, j} = b_k + \sum_{c=0}^{C-1} \sum_{u=0}^{K-1} \sum_{v=0}^{K-1} W_{k, c, u, v} \cdot X_{b, c, i+u, j+v}
\]

where `b` indexes the batch, `k` the output channel, `c` the input channel, and `K` is the kernel size. The implementation converts the input tensor into columns (`im2col`) to perform the multiply-add as a batched matrix product. Gradients use the inverse transformation (`col2im`) to accumulate updates back into image space.

### Pooling

Max pooling selects the largest activation within each `2×2` window. Backpropagation routes the gradient only to the position that achieved the maximum, ensuring sparse gradient flow.

### Backpropagation

Each layer implements its own backward pass:

- **Convolution** computes gradients with respect to weights and inputs using the stored column representation.
- **Max pooling** reuses the recorded arg-max indices to scatter the upstream gradients.
- **Dense layers** perform standard matrix calculus derivatives.
- **ReLU** multiplies the gradient by a binary mask of positive activations.

## Usage

The scripts live in this folder and can be executed after installing the repository dependencies (NumPy). Both scripts expose command-line arguments so you can balance speed and accuracy.

### Training

```bash
python Artificial\ Intelligence/CNN_Scratch/train.py \
    --epochs 5 \
    --batch-size 64 \
    --learning-rate 0.01 \
    --data-dir ./data \
    --model-out ./artifacts/cnn_mnist.npz
```

Useful flags:

- `--train-limit`: train on a subset of the data (e.g. 10_000 samples) for quicker experiments.
- `--validation-size`: reserve part of the training set for validation (default: 10,000).

The script automatically downloads MNIST the first time it runs. Training on CPU with the full dataset typically takes **15–20 minutes** depending on hardware because all operations are pure Python/NumPy.

### Evaluation

```bash
python Artificial\ Intelligence/CNN_Scratch/evaluate.py \
    --model ./artifacts/cnn_mnist.npz \
    --data-dir ./data
```

Add `--limit` to run on a subset of the test set for quick sanity checks.

## Limitations

- **Performance:** The network relies on NumPy matrix multiplications and Python loops; it is orders of magnitude slower than optimized frameworks. Expect long training times beyond a few epochs.
- **Feature set:** The implementation only supports the fixed architecture described above (two convolution blocks and two fully connected layers).
- **Numerical stability:** Although the loss includes a small epsilon, extreme learning rates can still cause divergence.
- **No GPU / autograd:** All gradients are derived manually and run on the CPU; there is no automatic differentiation or GPU acceleration.

## Reproducing results

1. Install dependencies: `python -m pip install -r requirements.txt` (NumPy is sufficient).
2. Train using the command above. Start with `--train-limit 20000` to verify everything runs in a couple of minutes.
3. Evaluate the saved weights with the evaluation script.

Regression tests cover tensor shape correctness for the forward pass and confirm that a brief training session on a synthetic dataset reduces the loss, guarding against regressions in the manual backpropagation routines.
