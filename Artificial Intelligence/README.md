# Artificial Intelligence

This directory collects classic, self-contained AI projects implemented in Python. Each subproject highlights a particular concept—search, reinforcement learning, neural networks, or gameplay—and ships with runnable scripts so you can reproduce the results locally.

## Environment setup

1. **Create a virtual environment (recommended):**

   ```bash
   python -m venv .venv
   source .venv/bin/activate  # Windows: .venv\Scripts\Activate.ps1
   ```

2. **Install extras defined in `pyproject.toml`:**

   ```bash
   python -m pip install -e .[ai]
   # Optional helpers
   python -m pip install -e .[algorithmic]  # plotting + scipy utilities
   python -m pip install -e .[developer]    # pytest, ruff, mypy
   ```

The `ai` extra bundles NumPy/SciPy, PyTorch, Gymnasium, Stable-Baselines3, and other shared dependencies so you can work across projects without juggling per-folder requirements.

## Project guides

### Basic Neural Network (`Basic Neural Network/BNN.py`)

- **Dataset preparation:** Uses an embedded XOR-style toy dataset declared at the top of the script—no external downloads required. Modify `training_inputs` / `training_outputs` to experiment with your own binary classification tasks. 【F:Artificial Intelligence/Basic Neural Network/BNN.py†L72-L90】
- **Train & inspect:**

  ```bash
  python "Artificial Intelligence/Basic Neural Network/BNN.py" --epochs 10000 --lr 0.1 --seed 42
  ```

  The CLI prints intermediate loss values every 1,000 epochs and opens a Matplotlib window with the loss curve when training completes. Predictions for a few held-out samples are printed to stdout for a quick sanity check. 【F:Artificial Intelligence/Basic Neural Network/BNN.py†L104-L152】
- **Checkpoints:** The perceptron keeps weights in memory only. To reuse a model, capture `nn.weights` after training or export them with `numpy.save` from a Python session.

### Real Neural Network (`Real Neural Network/`)

- **Dataset preparation:** `train.py` automatically downloads MNIST via `sklearn.datasets.fetch_openml` on first run. Pass `--data-home /path/to/cache` if you already have the dataset or want to store it elsewhere. Ensure internet access for the initial download. 【F:Artificial Intelligence/Real Neural Network/train.py†L1-L87】
- **Training command:**

  ```bash
  python "Artificial Intelligence/Real Neural Network/train.py" \
      --hidden-layers 256 128 \
      --epochs 15 \
      --batch-size 128 \
      --learning-rate 0.01 \
      --model-out mnist-mlp.npz
  ```

  Training prints epoch metrics, persists weights to `mnist-mlp.npz`, and writes a companion `mnist-mlp.history.npz` containing loss/accuracy curves. 【F:Artificial Intelligence/Real Neural Network/train.py†L88-L125】
- **Evaluation command:**

  ```bash
  python "Artificial Intelligence/Real Neural Network/evaluate.py" mnist-mlp.npz
  ```

  This recreates the MNIST split, reloads weights, and reports loss/accuracy plus a confusion matrix. 【F:Artificial Intelligence/Real Neural Network/README.md†L39-L49】
- **Checkpoints & resume:** Models are stored as compressed NumPy archives. Reload them with `MLP.load("mnist-mlp.npz")` and call `fit` again to continue training or fine-tune with new hyperparameters. 【F:Artificial Intelligence/Real Neural Network/mlp.py†L118-L151】
- **Automated validation:** Run the targeted regression tests with `pytest tests/artificial_intelligence/test_real_neural_network.py` after modifying training logic or checkpoint formats.

### CNN Framework (`CNN_Framework/`)

- **Dataset preparation:** The PyTorch training loop requests MNIST through `torchvision.datasets.MNIST` the first time it runs, storing files under the directory passed to `--data-dir` (default: `./data`). 【F:Artificial Intelligence/CNN_Framework/cnn_framework/data.py†L1-L120】
- **Training command:**

  ```bash
  python Artificial\ Intelligence/CNN_Framework/train.py \
      --data-dir ./data \
      --batch-size 128 \
      --epochs 5 \
      --learning-rate 1e-3
  ```

  Checkpoints land in `Artificial Intelligence/CNN_Framework/checkpoints/mnist_cnn.pt` unless you override `--checkpoint-path`. 【F:Artificial Intelligence/CNN_Framework/README.md†L18-L45】
- **Evaluation command:**

  ```bash
  python Artificial\ Intelligence/CNN_Framework/evaluate.py \
      Artificial\ Intelligence/CNN_Framework/checkpoints/mnist_cnn.pt \
      --data-dir ./data
  ```

  Use `--cpu` to bypass GPU requirements on constrained environments. 【F:Artificial Intelligence/CNN_Framework/README.md†L47-L61】
- **Checkpoints & resuming runs:**
  - The CLI always starts from scratch, but you can warm-start training with a short Python script that reloads the saved state and reuses the same helper functions:

    ```bash
    python - <<'PY'
    import sys
    from pathlib import Path

    project_root = Path("Artificial Intelligence/CNN_Framework").resolve()
    sys.path.insert(0, str(project_root))

    import torch
    from torch import nn

    from cnn_framework.data import create_data_loaders
    from cnn_framework.model import MNISTConvNet
    from cnn_framework.train import TrainConfig
    from cnn_framework.utils import evaluate, get_device, load_checkpoint, save_checkpoint

    config = TrainConfig(
        data_dir=Path("./data"),
        epochs=2,
        checkpoint_path=Path("Artificial Intelligence/CNN_Framework/checkpoints/mnist_cnn.pt"),
    )

    device = get_device()
    train_loader, eval_loader = create_data_loaders(
        data_dir=config.data_dir,
        batch_size=config.batch_size,
        num_workers=config.num_workers,
        train_limit=config.train_limit,
        eval_limit=config.eval_limit,
    )

    model = MNISTConvNet().to(device)
    load_checkpoint(model, config.checkpoint_path, map_location=device)
    optimizer = torch.optim.Adam(model.parameters(), lr=config.learning_rate)
    loss_fn: nn.Module = nn.CrossEntropyLoss()

    for epoch in range(1, config.epochs + 1):
        model.train()
        for inputs, targets in train_loader:
            inputs, targets = inputs.to(device), targets.to(device)
            optimizer.zero_grad()
            loss = loss_fn(model(inputs), targets)
            loss.backward()
            optimizer.step()
        metrics = evaluate(model, eval_loader, device=device, loss_fn=loss_fn)
        print(f"Resumed epoch {epoch}: {metrics}")

    save_checkpoint(model, config.checkpoint_path)
    PY
    ```

    Adjust `epochs`, `learning_rate`, or `checkpoint_path` as needed. This reproduces the default training loop while starting from previously saved weights. 【F:Artificial Intelligence/CNN_Framework/cnn_framework/train.py†L1-L132】【F:Artificial Intelligence/CNN_Framework/cnn_framework/utils.py†L61-L82】
- **Automated validation:** `pytest tests/test_cnn_framework.py -k smoke` executes a minimal train/eval loop to verify data downloads and checkpoint round-tripping. 【F:Artificial Intelligence/CNN_Framework/README.md†L63-L70】

### CNN from Scratch (`CNN_Scratch/`)

- **Dataset preparation:** Downloads MNIST with a lightweight helper the first time you call `train.py`, saving the cache under `--data-dir` (default: `./data`). 【F:Artificial Intelligence/CNN_Scratch/README.md†L40-L62】
- **Training command:**

  ```bash
  python Artificial\ Intelligence/CNN_Scratch/train.py \
      --epochs 5 \
      --batch-size 64 \
      --learning-rate 0.01 \
      --data-dir ./data \
      --model-out ./artifacts/cnn_mnist.npz
  ```

- **Evaluation command:**

  ```bash
  python Artificial\ Intelligence/CNN_Scratch/evaluate.py \
      --model ./artifacts/cnn_mnist.npz \
      --data-dir ./data
  ```

- **Checkpoints:** Saved as `.npz` archives containing convolution, pooling, and dense weights. Reuse them via `SimpleCNN().load(model_path)` in a Python session to inspect or continue training. 【F:Artificial Intelligence/CNN_Scratch/cnn.py†L288-L424】
- **Automated validation:** Run `pytest tests/ai/test_cnn_scratch.py` after refactoring low-level layers to ensure tensor shapes and loss reductions remain correct.

### AI Roguelike (`AI Roguelike/`)

- **Environment preparation:** No datasets are required; the agent pulls maps and combat mechanics from `Games/Roguelike`. Install the `ai_roguelike` extra (`pip install -e .[ai_roguelike]`) to grab `tcod` and supporting libraries. 【F:Artificial Intelligence/AI Roguelike/README.md†L13-L30】
- **Run the Monte Carlo agent:**

  ```bash
  PYTHONPATH="Artificial Intelligence/AI Roguelike" \
      python -m ai_roguelike.cli --turns 30 --iterations 128 --visualise
  ```

  Additional CLI flags (e.g., `--seed`, `--log-file`) are documented in the project README. 【F:Artificial Intelligence/AI Roguelike/README.md†L32-L76】
- **Automated validation:** `pytest tests/test_ai_roguelike_smoke.py` simulates several headless turns to confirm the agent remains stable after changes. 【F:Artificial Intelligence/AI Roguelike/README.md†L86-L95】

### Connect4 (`Connect4/c4.py`)

- **Dataset preparation:** None required—the script procedurally generates game states.
- **Run the game or pit the AI:**

  ```bash
  python "Artificial Intelligence/Connect4/c4.py"
  ```

  Choose between human vs. AI and AI vs. AI modes from the interactive prompts.

### Sudoku Solver (`Sudoku/astar.py`)

- **Dataset preparation:** Provide a plain-text file with nine rows of nine digits (`0` for blanks). Sample puzzles can be crafted manually or sourced from Sudoku generators.
- **Solve with A* (and optionally compare backtracking):**

  ```bash
  python "Artificial Intelligence/Sudoku/astar.py" --file puzzles/hard.txt --compare
  ```

  The solver prints the initial puzzle, the A* solution, and (with `--compare`) the backtracking result for cross-verification. 【F:Artificial Intelligence/Sudoku/astar.py†L61-L137】

### OpenAI Gym Workflow (`OpenAI Gym/`)

- **Environment preparation:** Uses Gymnasium environments and Stable-Baselines3 algorithms. Install via `pip install -e .[ai]`, optionally pinning a CPU-only PyTorch wheel if you lack GPU support. 【F:Artificial Intelligence/OpenAI Gym/README.md†L11-L34】
- **Training command:**

  ```bash
  python "Artificial Intelligence/OpenAI Gym/train.py" \
      --mode train \
      --env-id CartPole-v1 \
      --total-timesteps 80000 \
      --checkpoint-dir "Artificial Intelligence/OpenAI Gym/checkpoints" \
      --model-path "Artificial Intelligence/OpenAI Gym/models/dqn_cartpole"
  ```

- **Evaluation command:**

  ```bash
  python "Artificial Intelligence/OpenAI Gym/train.py" \
      --mode eval \
      --env-id CartPole-v1 \
      --model-path "Artificial Intelligence/OpenAI Gym/checkpoints/best/best_model" \
      --eval-episodes 5
  ```

- **Checkpoints:** Stable-Baselines3 saves `.zip` snapshots in the chosen directory. Use `--model-path` to pick which policy to evaluate, or load the archive with `DQN.load(model_path)` inside a Python session to continue training runs programmatically. 【F:Artificial Intelligence/OpenAI Gym/README.md†L36-L78】
- **Automated validation:** Run `pytest tests/artificial_intelligence/test_openai_gym.py` to ensure deterministic policies and environment wrappers still function. 【F:Artificial Intelligence/OpenAI Gym/README.md†L80-L101】

## Troubleshooting

- **Missing or incompatible GPU drivers:** All training scripts accept CPU fallbacks—use `--cpu` with the CNN Framework or install a CPU-only PyTorch wheel as outlined in the OpenAI Gym README before running GPU-dependent workflows. 【F:Artificial Intelligence/CNN_Framework/README.md†L47-L61】【F:Artificial Intelligence/OpenAI Gym/README.md†L21-L34】
- **Interpreting loss curves:** The Real Neural Network exporter produces `*.history.npz`. Load it with NumPy or Matplotlib (`np.load("mnist-mlp.history.npz")`) to diagnose overfitting/underfitting trends when tuning hyperparameters. 【F:Artificial Intelligence/Real Neural Network/train.py†L116-L125】
- **Verifying Sudoku solutions:** Pair the A* solver with the optional backtracking check (`--compare`) to confirm the filled grid is consistent. For automated regression, wrap the solver call in a unit test that asserts `np.array_equal` between the two solutions before committing. 【F:Artificial Intelligence/Sudoku/astar.py†L117-L137】

## Further reading

- [Stanford CS231n Convolutional Neural Networks notes](http://cs231n.stanford.edu/) – foundational theory for CNN projects.
- [A* pathfinding primers](https://www.redblobgames.com/pathfinding/a-star/introduction.html) – excellent visual explanations for the Sudoku solver’s search strategy.
- [Neural network tutorials by Michael Nielsen](http://neuralnetworksanddeeplearning.com/) – complements the Basic and Real Neural Network implementations.
- [Monte Carlo Tree Search overviews](https://jeffbradberry.com/posts/2015/09/intro-to-monte-carlo-tree-search/) – background reading for the AI Roguelike agent.

## Contributing

Pull requests are welcome! Keep documentation and command examples up to date, run the relevant pytest suites (see project guides above), and follow the repository’s formatting conventions to maintain consistency across tutorials.

---

*Explore, learn, and experiment with classic AI algorithms and games!*
