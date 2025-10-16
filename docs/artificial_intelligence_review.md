# Artificial Intelligence Challenge Review

## Challenge Inventory
- AI Roguelike
- Basic Neural Network
- CNN_Framework
- CNN_Scratch
- Connect4
- OpenAI Gym
- Real Neural Network
- Sudoku

## Solution Assessments

### AI Roguelike
- **Correctness:** The environment wrapper exposes legal action filtering, terminal detection, and heuristic evaluation directly over the live roguelike engine, which keeps the planner aligned with the underlying game rules.【F:Artificial Intelligence/AI Roguelike/ai_roguelike/environment.py†L42-L169】 The MCTS controller follows the expected selection/expansion/rollout/backpropagation cycle and guards against degenerate trees, supporting sound decision making.【F:Artificial Intelligence/AI Roguelike/ai_roguelike/mcts.py†L26-L167】
- **Efficiency:** Deep-copying the full engine on every node expansion and rollout is safe but expensive. Introducing a lighter-weight state clone or incremental undo/redo system would reduce per-iteration cost and enable higher iteration budgets.【F:Artificial Intelligence/AI Roguelike/ai_roguelike/environment.py†L110-L169】【F:Artificial Intelligence/AI Roguelike/ai_roguelike/mcts.py†L74-L166】
- **Readability:** Both modules are well-structured, with docstrings and clear method boundaries that make the planning pipeline easy to follow.【F:Artificial Intelligence/AI Roguelike/ai_roguelike/environment.py†L1-L170】【F:Artificial Intelligence/AI Roguelike/ai_roguelike/mcts.py†L1-L167】
- **Best Practices:** The code is idiomatic Python (type hints, dataclasses, clear separation of concerns). Consider parameterising the heuristic and rollout policies for experimentation flexibility without modifying core classes.

### Basic Neural Network
- **Correctness:** The model is a single neuron trained on an XOR-like dataset, which is not linearly separable; no amount of training will achieve low error, so the solver cannot meet the stated goal.【F:Artificial Intelligence/Basic Neural Network/BNN.py†L69-L141】
- **Efficiency:** For such a small model the implementation is fine, but the futile training loops waste compute due to the structural mismatch between model capacity and dataset.
- **Readability:** The script is approachable and well-commented. Plotting and CLI scaffolding are cleanly separated.【F:Artificial Intelligence/Basic Neural Network/BNN.py†L1-L150】
- **Best Practices:** Consider switching to a minimal multi-layer perceptron or swapping in a linearly separable dataset so that training succeeds; additionally, avoid `print` in tight training loops in favour of logging hooks.

### CNN_Framework
- **Correctness:** Uses torchvision datasets and a conventional PyTorch CNN; training and evaluation loops follow standard practice and compute metrics correctly.【F:Artificial Intelligence/CNN_Framework/cnn_framework/data.py†L12-L77】【F:Artificial Intelligence/CNN_Framework/cnn_framework/train.py†L34-L79】
- **Efficiency:** Defaults are sensible (Adam optimiser, batching). However, if `epochs` is set to 0, the function returns `metrics` without initialisation, which will raise; guarding this edge case would harden the workflow.【F:Artificial Intelligence/CNN_Framework/cnn_framework/train.py†L50-L79】
- **Readability:** Modules are modular (data/model/utils) and documented, making it easy to extend.【F:Artificial Intelligence/CNN_Framework/cnn_framework/model.py†L1-L29】【F:Artificial Intelligence/CNN_Framework/cnn_framework/utils.py†L1-L69】
- **Best Practices:** Consider persisting optimiser state when checkpointing and exposing configurable learning-rate schedulers to align with modern PyTorch training conventions.

### CNN_Scratch
- **Correctness:** Convolution, pooling, and dense layers are implemented from first principles and combined into a working CNN; training loops update parameters as expected.【F:Artificial Intelligence/CNN_Scratch/cnn.py†L83-L215】【F:Artificial Intelligence/CNN_Scratch/cnn.py†L289-L372】
- **Efficiency:** Max-pooling and data preparation rely on deeply nested Python loops, which become a bottleneck for larger datasets; vectorising these sections or using NumPy strides would provide substantial speedups.【F:Artificial Intelligence/CNN_Scratch/cnn.py†L157-L199】
- **Readability:** The module is lengthy but logically partitioned by layer type, with clear docstrings. The training script, however, reports “Validation accuracy” even when no validation split is provided, which can confuse users.【F:Artificial Intelligence/CNN_Scratch/train.py†L90-L115】
- **Best Practices:** Add gradient checks or unit tests around `im2col/col2im` to prevent silent math errors, and ensure CLI argument combinations (e.g., zero validation size) produce accurate status messages.

### Connect4
- **Correctness:** Board management correctly enforces column capacity, win detection across all directions, and integrates with the minimax-based AI for legal play.【F:Artificial Intelligence/Connect4/c4.py†L33-L199】
- **Efficiency:** Each minimax expansion instantiates a fresh `Board`, incurring repeated allocations; reusing board arrays or applying moves in-place with undo would reduce overhead.【F:Artificial Intelligence/Connect4/c4.py†L181-L203】
- **Readability:** The file mixes game logic, AI, and UI, but remains readable thanks to docstrings and structured sections.【F:Artificial Intelligence/Connect4/c4.py†L1-L210】 Splitting the Pygame loop into a separate module would improve maintainability.
- **Best Practices:** Consider adding depth-dependent evaluation cutoffs or transposition caching to make the AI more competitive while keeping response times acceptable.

### OpenAI Gym
- **Correctness:** Training and evaluation use Stable Baselines3’s DQN with deterministic seeding and scheduled evaluations, yielding reproducible runs.【F:Artificial Intelligence/OpenAI Gym/train.py†L1-L120】【F:Artificial Intelligence/OpenAI Gym/train.py†L132-L199】
- **Efficiency:** Callback-based checkpointing is standard. Allowing multi-environment vectorisation or GPU policy execution would further speed experimentation.
- **Readability:** CLI parsing and helper functions (seed management, environment factory) are concise and well-documented.【F:Artificial Intelligence/OpenAI Gym/train.py†L21-L119】
- **Best Practices:** Consider surfacing configuration presets for common Gym tasks and adding tests that exercise the heuristic policy module to prevent regressions.【F:Artificial Intelligence/OpenAI Gym/policies.py†L1-L28】

### Real Neural Network
- **Correctness:** Implements a full MLP with softmax cross-entropy and optional L2 regularisation; forward/backward passes follow textbook derivations.【F:Artificial Intelligence/Real Neural Network/mlp.py†L146-L285】 Training and evaluation scripts orchestrate data loading, shuffling, and checkpointing coherently.【F:Artificial Intelligence/Real Neural Network/train.py†L80-L147】
- **Efficiency:** Pure NumPy operations scale reasonably but recompute full-dataset metrics each epoch, which is costly; tracking running means during minibatch iteration would be lighter. Fetching MNIST from OpenML every run also slows iterations unless cached.【F:Artificial Intelligence/Real Neural Network/data.py†L9-L29】
- **Readability:** Modules are cleanly separated with docstrings and typed interfaces, easing extension.【F:Artificial Intelligence/Real Neural Network/mlp.py†L1-L205】
- **Best Practices:** Preserve the original training arrays when shuffling inside `fit` (currently overwrites arguments) and add gradient-norm logging to spot exploding updates.【F:Artificial Intelligence/Real Neural Network/mlp.py†L234-L247】

### Sudoku
- **Correctness:** A* search prioritises states by the number of empty cells, but using `Sudoku(board.tolist())` inside the inner loop repeatedly re-validates via a fresh object, dramatically slowing search; factoring `is_valid_move` to work on raw arrays would help.【F:Artificial Intelligence/Sudoku/astar.py†L28-L45】 The solver falls back to backtracking, but the heuristic is too weak to prune effectively.
- **Efficiency:** The heuristic counts empty cells only, so the priority queue explores many equivalent states; incorporating row/column constraint weighting would improve convergence.【F:Artificial Intelligence/Sudoku/astar.py†L21-L45】
- **Readability:** The script is otherwise straightforward with CLI support.【F:Artificial Intelligence/Sudoku/astar.py†L1-L146】
- **Best Practices:** Remove unused imports and add unit tests with known puzzles to guard against regressions; consider returning solution grids instead of mutating state for easier integration.

## Prioritised Improvement Backlog
1. **Basic Neural Network:** Replace the single-layer perceptron with a minimal hidden-layer network or adjust the dataset so the example actually learns; this is the only solution that cannot solve its own demonstration problem.【F:Artificial Intelligence/Basic Neural Network/BNN.py†L69-L141】
2. **Sudoku:** Optimise the A* search by eliminating per-move object reconstruction and strengthening the heuristic; performance currently degrades severely on moderate puzzles.【F:Artificial Intelligence/Sudoku/astar.py†L28-L45】
3. **CNN_Scratch:** Vectorise pooling/backprop hot spots and fix misleading validation messaging to make the educational pipeline faster and less confusing.【F:Artificial Intelligence/CNN_Scratch/cnn.py†L157-L199】【F:Artificial Intelligence/CNN_Scratch/train.py†L90-L115】
4. **CNN_Framework:** Harden edge cases (e.g., zero-epoch runs) and enrich checkpoint metadata for smoother experimentation workflows.【F:Artificial Intelligence/CNN_Framework/cnn_framework/train.py†L50-L79】
5. **Connect4:** Reduce minimax allocation churn and modularise UI vs. logic to improve responsiveness and maintainability.【F:Artificial Intelligence/Connect4/c4.py†L181-L210】
6. **Real Neural Network & OpenAI Gym:** Mostly solid; focus on incremental ergonomics (dataset caching, richer logging, configuration presets).【F:Artificial Intelligence/Real Neural Network/data.py†L9-L29】【F:Artificial Intelligence/OpenAI Gym/train.py†L1-L199】
7. **AI Roguelike:** Already robust; future work could target performance optimisations in state cloning for deeper searches.【F:Artificial Intelligence/AI Roguelike/ai_roguelike/mcts.py†L74-L166】
