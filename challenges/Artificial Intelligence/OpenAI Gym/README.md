# OpenAI Gym DQN Trainer

This project packages a small, reproducible reinforcement-learning workflow around [Gymnasium](https://gymnasium.farama.org/) and [Stable-Baselines3](https://stable-baselines3.readthedocs.io/en/master/). It provides a single training script with:

- Deterministic seeding for Python, NumPy, and PyTorch
- Train/eval command-line flags
- Periodic checkpointing and evaluation callbacks
- Support for changing environments and basic hyperparameters

The default configuration targets `CartPole-v1`, a classic control task that solves quickly on commodity hardware.

## Setup

Install the AI extra to pull in Gymnasium, Stable-Baselines3, and PyTorch (plus shared scientific dependencies):

```bash
python -m venv .venv
source .venv/bin/activate  # Windows: .venv\Scripts\Activate.ps1
python -m pip install -e .[ai]
```

> **Tip:** If you prefer a CPU-only PyTorch build, install it first via
> `python -m pip install torch==2.5.1+cpu --index-url https://download.pytorch.org/whl/cpu`
> before running the editable extra command above.

## Usage

### Train a policy

```bash
python "challenges/Artificial Intelligence/OpenAI Gym/train.py" \
    --mode train \
    --env-id CartPole-v1 \
    --total-timesteps 80000 \
    --checkpoint-dir "challenges/Artificial Intelligence/OpenAI Gym/checkpoints" \
    --model-path "challenges/Artificial Intelligence/OpenAI Gym/models/dqn_cartpole"
```

Key flags:

- `--env-id`: Any Gymnasium environment you have installed.
- `--total-timesteps`: Training budget in environment steps.
- `--checkpoint-dir`: Directory to drop `.zip` snapshots and evaluation logs.
- `--seed`: Seed for RNGs to keep runs comparable.

The script will create the checkpoint/model directories if they do not yet exist.

### Evaluate an existing checkpoint

```bash
python "challenges/Artificial Intelligence/OpenAI Gym/train.py" \
    --mode eval \
    --env-id CartPole-v1 \
    --model-path "challenges/Artificial Intelligence/OpenAI Gym/checkpoints/best/best_model" \
    --eval-episodes 5
```

The script prints the average episodic reward and standard deviation across evaluation runs.

## Lightweight heuristic

To keep regression tests fast and the repository binary-free, the project ships a small `policies.py` helper containing a deterministic CartPole controller. The heuristic uses the pole angle and angular velocity to decide which direction to push the cart and reliably averages well over the 150 reward threshold.

Use it as a quick environment smoke test:

```python
from runpy import run_path

module_globals = run_path("challenges/Artificial Intelligence/OpenAI Gym/policies.py")
policy = module_globals["cartpole_balance_policy"]()
```

The pytest suite exercises this controller to ensure the environment remains stable without committing large binary checkpoints to the repository.
