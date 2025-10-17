# Debugging & Test Guide

This guide documents the automated tests that ship with the repository and provides quick playbooks for exercising the most common project categories. Use it alongside the category READMEs when preparing fixes or validating contributions.

## Automated Test Coverage

| Test file | Category | Project(s) exercised | Highlights |
| --- | --- | --- | --- |
| `tests/test_cli_help.py` | Cross-category | All CLI entry points under `challenges/Algorithmic/`, `challenges/Practical/`, `challenges/Games/`, `challenges/Artificial Intelligence/`, and `challenges/Emulation/` | Discovers scripts with a `__main__` guard and asserts their `--help` output runs without error while skipping GUI- or network-heavy tools. 【F:tests/test_cli_help.py†L1-L82】 |
| `tests/test_ai_roguelike_smoke.py` | Artificial Intelligence | `challenges/Artificial Intelligence/AI Roguelike` | Installs a lightweight `tcod` stub, runs the MCTS agent for a handful of turns, and checks ASCII rendering output. 【F:tests/test_ai_roguelike_smoke.py†L1-L194】 |
| `tests/ai/test_cnn_scratch.py` | Artificial Intelligence | `challenges/Artificial Intelligence/CNN_Scratch` | Loads the scratch CNN, checks forward-pass shapes, and ensures brief training reduces loss on a synthetic dataset. 【F:tests/ai/test_cnn_scratch.py†L1-L78】 |
| `tests/test_cnn_framework.py` | Artificial Intelligence | `challenges/Artificial Intelligence/CNN_Framework` | Requires the PyTorch stack, runs a one-epoch training/evaluation cycle, and verifies checkpoints and accuracy metrics. 【F:tests/test_cnn_framework.py†L1-L56】 |
| `tests/artificial_intelligence/test_openai_gym.py` | Artificial Intelligence | `challenges/Artificial Intelligence/OpenAI Gym` | Executes the bundled CartPole heuristic policy via Gymnasium/Stable Baselines and asserts the mean reward stays high. 【F:tests/artificial_intelligence/test_openai_gym.py†L1-L46】 |
| `tests/artificial_intelligence/test_real_neural_network.py` | Artificial Intelligence | `challenges/Artificial Intelligence/Real Neural Network` | Imports the MLP, validates forward shapes, ensures training improves loss/accuracy, and exercises save/load round-trips. 【F:tests/artificial_intelligence/test_real_neural_network.py†L1-L63】 |
| `tests/test_wolfenstein_clone.py` | Games | `challenges/Games/WolfensteinClone` | Boots the raycasting engine with SDL's dummy video driver, advances a few frames, and validates map loading. 【F:tests/test_wolfenstein_clone.py†L1-L45】 |
| `tests/test_raytracer.py` | Emulation & Modeling | `challenges/Emulation/RayTracer` | Renders sample scenes at multiple resolutions and compares SHA-256 hashes of the pixel buffers. 【F:tests/test_raytracer.py†L1-L26】 |
| `tests/test_constructive_solid_geometry.py` | Emulation & Modeling | `challenges/Emulation/ConstructiveSolidGeometry` | Meshing helpers are exercised by comparing computed volumes for sphere, union, intersection, and difference operations. 【F:tests/test_constructive_solid_geometry.py†L1-L38】 |
| `tests/emulation/test_lambert.py` | Emulation & Modeling | `challenges/Emulation/LambertsProblem` | Confirms Lambert solver outputs match Vallado's reference case and validates error handling. 【F:tests/emulation/test_lambert.py†L1-L52】 |
| `tests/emulation/test_ti86_cpu.py` | Emulation & Modeling | `challenges/Emulation/TI86` | Runs opcode truth-table fixtures against the Z80 core, exercises memory semantics, breakpoints, and keypad/LCD integration. 【F:tests/emulation/test_ti86_cpu.py†L1-L66】 |
| `tests/test_chip8_cpu.py` | Emulation & Modeling | `challenges/Emulation/Chip8` | Validates load/store, drawing collisions, control flow, RNG, and BCD encoding on the CHIP-8 emulator. 【F:tests/test_chip8_cpu.py†L1-L96】 |
| `tests/test_double_pendulum.py` | Emulation & Modeling | `src/pro_g_rammingchallenges4/emulation/double_pendulum.py` | Checks that the integrator conserves energy for small-angle initial conditions. 【F:tests/test_double_pendulum.py†L1-L24】 |
| `tests/test_fft_spectrum.py` | Emulation & Modeling | `challenges/Emulation/FFTSpectrum` | Loads the bundled tone sample, computes FFT magnitudes, and asserts the dominant frequency is ~440 Hz. 【F:tests/test_fft_spectrum.py†L1-L23】 |
| `tests/test_nbody.py` | Emulation & Modeling | `src/pro_g_rammingchallenges4/nbody.py` | Exercises gravitational force calculations, collision merges, and radius scaling helpers. 【F:tests/test_nbody.py†L1-L38】 |

## Category Debugging Playbooks

### Practical

**Manual smoke checks**

- Use `python path/to/script.py --help` to confirm entry points execute without side-effects; the CLI test uses the same invocation across Practical utilities. 【F:tests/test_cli_help.py†L1-L82】
- GUI-heavy apps listed in the CLI skip table require a desktop stack; run them locally instead of headless environments when reproducing GUI bugs. 【F:tests/test_cli_help.py†L20-L63】

**Dataset & dependency prerequisites**

- Many Practical challenges rely on optional extras (see `pyproject.toml` extras); install `.[practical,desktop]` when working with GUI clients or media tooling.
- Networking tools (chat server, torrent client, download manager) expect reachable services—stage mock endpoints before regression testing.

**Targeted pytest commands**

- `pytest tests/test_cli_help.py -k "Practical"` filters the CLI matrix to Practical scripts only.
- Combine with `-x` to stop on the first failing CLI for faster iteration.

### Algorithmic

**Manual smoke checks**

- Algorithmic scripts are typically CLI utilities—exercise them with representative sample inputs via `python challenges/Algorithmic/<project>/main.py --help` or short input files. 【F:tests/test_cli_help.py†L1-L82】

**Dataset & dependency prerequisites**

- Most Algorithmic solutions are pure-Python. Optional visualizers listed in the skip table (e.g., Djikstra visualizer) depend on plotting/GUI stacks. 【F:tests/test_cli_help.py†L24-L27】

**Targeted pytest commands**

- `pytest tests/test_cli_help.py -k "Algorithmic"` runs the CLI smoke tests for algorithmic utilities only.

### Games

**Manual smoke checks**

- Launch games with `python challenges/Games/<Game>/game.py --help` (where available) to ensure command-line arguments parse cleanly. The CLI test will surface failures early. 【F:tests/test_cli_help.py†L1-L82】
- For interactive debugging, run `python challenges/Games/WolfensteinClone/game.py` locally with SDL configured (no dummy driver) to reproduce rendering issues.

**Dataset & dependency prerequisites**

- Install `pygame` and other game-specific requirements; the Wolfenstein smoke test bootstraps SDL with the `SDL_VIDEODRIVER` dummy backend to avoid GUI requirements. 【F:tests/test_wolfenstein_clone.py†L1-L30】

**Targeted pytest commands**

- `pytest tests/test_wolfenstein_clone.py` validates the raycasting engine headlessly.
- `pytest tests/test_cli_help.py -k "Games"` checks CLI entry points for console helpers under `challenges/Games/`.

### Artificial Intelligence

**Manual smoke checks**

- Verify reinforcement-learning or planning agents (e.g., AI Roguelike) can step through sample environments without crashing before running long training sessions. 【F:tests/test_ai_roguelike_smoke.py†L108-L194】
- Inspect generated checkpoints/logs for neural-network projects to confirm metrics improve across epochs; the CNN framework test writes checkpoints under `checkpoints/`. 【F:tests/test_cnn_framework.py†L24-L49】

**Dataset & dependency prerequisites**

- Install the `.[ai]` extra to pull in `torch`, `torchvision`, and Gym dependencies required by CNN and OpenAI Gym projects. 【F:tests/test_cnn_framework.py†L9-L17】【F:tests/artificial_intelligence/test_openai_gym.py†L9-L21】
- Allow the CNN framework to download MNIST into a writable data directory (`data_dir` in `TrainConfig`) during first-run training. 【F:tests/test_cnn_framework.py†L24-L39】
- AI Roguelike expects `tcod`; the smoke test injects a stub, but full runs should use the real library for rendering fidelity. 【F:tests/test_ai_roguelike_smoke.py†L15-L107】

**Targeted pytest commands**

- `pytest tests/test_cnn_framework.py` for the PyTorch training/evaluation loop.
- `pytest tests/ai/test_cnn_scratch.py` for the scratch CNN implementation.
- `pytest tests/test_ai_roguelike_smoke.py` to exercise the roguelike agent end-to-end.
- `pytest tests/artificial_intelligence/test_openai_gym.py` after installing Gymnasium/Stable Baselines extras.

### Emulation & Modeling

**Manual smoke checks**

- Run emulator cores (CHIP-8, TI-86) against bundled ROMs or opcode fixtures to reproduce CPU-level issues; the tests show how to load programs and assert state. 【F:tests/test_chip8_cpu.py†L14-L96】【F:tests/emulation/test_ti86_cpu.py†L17-L57】
- For physics/visualization projects, render reference scenes (`challenges/Emulation/RayTracer/scenes/minimal.json`) or compare energy curves for the double pendulum to validate numerical stability. 【F:tests/test_raytracer.py†L11-L25】【F:tests/test_double_pendulum.py†L1-L24】

**Dataset & dependency prerequisites**

- Ensure binary fixtures remain accessible: `challenges/Emulation/TI86/opcode_truth.json` for TI-86 and `challenges/Emulation/FFTSpectrum/test_tone.wav` for the FFT analyzer. 【F:tests/emulation/test_ti86_cpu.py†L11-L16】【F:tests/test_fft_spectrum.py†L1-L14】
- Numerical solvers depend on `numpy`/`pytest`; install `.[emulation]` extras if available to pull consistent versions.

**Targeted pytest commands**

- `pytest tests/test_raytracer.py tests/test_constructive_solid_geometry.py` for rendering/meshing regressions.
- `pytest tests/test_chip8_cpu.py tests/emulation/test_ti86_cpu.py` to exercise emulator cores.
- `pytest tests/test_fft_spectrum.py tests/test_nbody.py tests/emulation/test_lambert.py` for physics/FFT coverage.
