# Key Press Bot

Automate keyboard input across operating systems using Python. This tool offers repeatable key sequences, macro-style scripts, and recording/playback features so you can quickly demo workflows, fill repetitive forms, or smoke-test shortcuts.

## Why this exists

Manual keying is tedious and error-prone when you need to run the same shortcut chain again and again. Key Press Bot lets you:

- Prototype keyboard-driven UI automations without committing to a heavyweight RPA suite.
- Capture a live interaction and replay it later for demos or regression checks.
- Script reproducible examples when writing documentation or teaching material.

## Safety & Responsible Use

Keyboard automation can interfere with active sessions. Keep these safeguards in mind:

- **Use a dedicated desktop or virtual machine** for testing—unintended input can disrupt other apps or trigger destructive actions.
- **Add generous delays** between steps if you plan to interact with remote desktops or laggy VMs; otherwise playback may outrun the UI.
- **Keep stop hotkeys within reach.** The default `Ctrl+Alt+.` instantly aborts playback, while `Ctrl+Alt+Q` ends a recording.
- **Avoid storing sensitive sequences** (passwords, personal data) in plain-text scripts. If you must, protect files with OS-level encryption.
- **Disable or pause the bot** before switching workspaces or focus; scripted keystrokes always target the active window.

## Feature Highlights

- Cross-platform simulation built on [`pynput`](https://pynput.readthedocs.io/) and [`pyautogui`](https://pyautogui.readthedocs.io/).
- JSON-based scripting language with explicit delays, modifiers, and text typing helpers.
- Live recording with precise timing capture—export to JSON for later tweaks.
- Global hotkeys to start/stop recording or playback without returning to the terminal.
- Sample scripts and manual test plans to help you validate setup safely.

## Installation

Create/activate a virtual environment, then install Practical requirements (or selectively install `pynput` + `pyautogui`):

```bash
pip install -r ../requirements.txt
```

## CLI Overview

```bash
python key_press_bot.py --help
```

Key commands:

| Command | Purpose |
| --- | --- |
| `python key_press_bot.py --script scripts/sample_sequence.json` | Run a high-level scripted macro once. |
| `python key_press_bot.py --play run.json` | Replay a previously recorded session. |
| `python key_press_bot.py --play run.json --watch-hotkeys` | Register global hotkeys (`Ctrl+Alt+P` start, `Ctrl+Alt+.` stop). |
| `python key_press_bot.py --record capture.json` | Start recording immediately; stop with `Ctrl+Alt+Q`. |

Use `--start-hotkey`, `--stop-hotkey`, and `--record-stop-hotkey` to customise the triggers.

## Script Format

Scripts are JSON documents with `mode` set to `sequence` (high-level actions) or `recording` (raw events captured from a live session).

High-level sequence snippet (from [`scripts/sample_sequence.json`](./scripts/sample_sequence.json)):

```json
{
  "mode": "sequence",
  "meta": {"name": "Open Run dialog"},
  "steps": [
    {"action": "press", "keys": ["win", "r"], "hold": 0.15, "delay_after": 0.4},
    {"action": "type_text", "text": "notepad", "delay_after": 0.1},
    {"action": "press", "keys": ["enter"], "hold": 0.05}
  ]
}
```

Recording output uses precise timestamps and the `recording` mode:

```json
{
  "mode": "recording",
  "meta": {"name": "demo"},
  "events": [
    {"timestamp": 0.0, "event": "press", "key": "ctrl"},
    {"timestamp": 0.14, "event": "press", "key": "c"},
    {"timestamp": 0.27, "event": "release", "key": "c"},
    {"timestamp": 0.31, "event": "release", "key": "ctrl"}
  ]
}
```

Both formats support delays, combos, and playback cancellation via the stop hotkey.

## Recording Workflow

1. Run `python key_press_bot.py --record capture.json`.
2. Perform your actions. The script captures press/release timestamps.
3. Hit `Ctrl+Alt+Q` to stop.
4. Inspect or tweak the JSON file before replaying.

## Manual Test Plan

Because automated GUI tests are unreliable in headless environments, validate new setups manually:

1. **Dry-run a script:** Focus a harmless window (e.g., text editor) and execute the sample sequence. Confirm the expected text is typed.
2. **Recording sanity check:** Record a short sequence (`Ctrl+C`, `Ctrl+V`), stop with `Ctrl+Alt+Q`, and review the JSON for ordered events.
3. **Playback cancel:** Start playback with `--watch-hotkeys`, trigger it via `Ctrl+Alt+P`, then immediately stop with `Ctrl+Alt+.` and ensure no further keys fire.
4. **Custom delays:** Edit the sample script to introduce long waits and confirm timing honours your changes.

Document any issues and always keep a window selected where automated input is safe.

## Troubleshooting

- **`ImportError` for `pynput`/`pyautogui`:** Install the missing dependency in your environment.
- **Playback too fast:** Increase `delay_after` and `hold` values in scripts, or add explicit `sleep` steps.
- **Stop hotkeys ignored:** Another app may capture them first—change the combination to something unique.
- **Recording stops instantly:** Ensure no macro utility is already sending `Ctrl+Alt+Q`.

## Next Ideas

- Allow mouse movements/clicks alongside keyboard steps.
- Expose a YAML loader for more human-friendly scripting.
- Bundle a small GUI for recording management and script organisation.
