# CNN Framework – MNIST with PyTorch

This mini-framework trains a convolutional neural network on the classic MNIST handwritten digit dataset. It provides
ready-to-run command line tools for downloading data, training the model, evaluating checkpoints, and running quick smoke
checks. The code is CPU friendly, yet automatically takes advantage of GPU hardware when available.

## Environment setup

```bash
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\Activate.ps1
python -m pip install --upgrade pip
python -m pip install -e .[ai]
```

The `ai` extra now bundles PyTorch (`torch`, `torchvision`) in addition to the lighter scientific stack used by the other AI
projects. The training and evaluation entry points live next to this README and automatically add the directory to
`PYTHONPATH`, so you can execute them directly from the repository root once the dependencies are installed.

## Training the network

```bash
python Artificial\ Intelligence/CNN_Framework/train.py \
    --data-dir ./data \
    --batch-size 128 \
    --epochs 5 \
    --learning-rate 1e-3
```

The script will download MNIST on first run, train `MNISTConvNet`, print epoch summaries, and save the weights to
`challenges/Artificial Intelligence/CNN_Framework/checkpoints/mnist_cnn.pt` by default. You can override the checkpoint location and
limit samples for rapid experiments with `--checkpoint-path`, `--train-limit`, and `--eval-limit`.

### Expected accuracy

On CPU-only runs the model reaches **~98% accuracy** on the MNIST test split after five epochs. GPU training typically
finishes in under a minute and achieves the same accuracy. Due to the inherent stochasticity of SGD, your exact numbers may
vary by a few tenths of a percent.

## Evaluating a checkpoint

```bash
python Artificial\ Intelligence/CNN_Framework/evaluate.py \
    Artificial\ Intelligence/CNN_Framework/checkpoints/mnist_cnn.pt \
    --data-dir ./data
```

The evaluation CLI loads the stored weights, runs through the MNIST test split, and prints final loss/accuracy metrics. Use
`--cpu` to force CPU execution or `--eval-limit` to run on a smaller sample for quicker validation.

## Smoke tests

A repository test (`tests/test_cnn_framework.py`) performs a single training step and evaluation batch to ensure that the
dataset downloads correctly, the model executes forward/backward passes, and checkpoints round-trip without errors. Run it
with `pytest` or invoke the helper directly:

```bash
pytest tests/test_cnn_framework.py -k smoke
```

## Project layout

```
CNN_Framework/
├── README.md
├── cnn_framework/
│   ├── __init__.py
│   ├── data.py          # MNIST data loaders and download pipeline
│   ├── evaluate.py      # Evaluation helpers and CLI
│   ├── model.py         # MNISTConvNet architecture
│   ├── train.py         # Training loop and CLI entrypoint
│   └── utils.py         # Device helpers, metrics, checkpoint tools
├── checkpoints/         # Saved model weights (created at runtime)
├── evaluate.py          # Wrapper that exposes cnn_framework.evaluate.main
└── train.py             # Thin wrapper re-exporting the training CLI
```

The top-level `train.py` and `evaluate.py` wrappers proxy to the package entry points so that IDEs and command lines can run
the scripts from the project root without manual `PYTHONPATH` juggling.
