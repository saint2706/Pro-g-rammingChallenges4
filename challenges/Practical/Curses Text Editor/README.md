# Curses Text Editor

A modal terminal text editor built with Python's `curses` module. It blends familiar Vim- and Emacs-style keymaps, supports configurable bindings, and adds guard rails (autosave, command palette, contextual help) for new users.

## Features

- **Modal editing**: Command and Insert modes with intuitive transitions (`Esc`, `i`, `a`, `o`, etc.).
- **Hybrid keybindings**: Vim navigation (`h`, `j`, `k`, `l`) alongside Emacs-style movement (`Ctrl+A/E/N/P`) and editing shortcuts (`Ctrl+K`, `Ctrl+Y`).
- **Configurable maps**: Override any binding via JSON config (`~/.ctedit.json` or path passed at runtime).
- **Command palette**: Press `:` in command mode to issue `open`, `write`, `quit`, `search`, and substitution commands (`:s/old/new/`).
- **Search & replace**: Incremental highlighting and `n` / `Shift+N` navigation for repeated matches.
- **File management**: Open, save-as, autosave (timed) with recovery files (`.autosave` suffix).
- **Status bar**: Displays mode, file path, cursor position, dirty flag, and last message.
- **Help overlay**: `F1` (or `:help`) shows in-app cheat sheet derived from `HELP.md`.

## Quick Start

```bash
pip install -r ../../requirements.txt  # optional, no external deps required
python editor.py --file notes.txt
```

### Launch arguments

| Flag | Description |
|------|-------------|
| `--file PATH` | Open an existing file or create PATH on save. |
| `--config PATH` | Load keybinding/options config from JSON. |
| `--autosave-interval SECONDS` | Override autosave throttle (default 10s). |
| `--encoding NAME` | Encoding for IO operations (default `utf-8`). |

## Default Keybindings

### Command mode

| Keys | Action |
|------|--------|
| `h` / `←` | Move left |
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `l` / `→` | Move right |
| `0` / `Ctrl+A` | Move to line start |
| `$` / `Ctrl+E` | Move to line end |
| `w` | Jump to next word |
| `b` | Jump to previous word |
| `gg` / `Ctrl+Home` | Jump to file start |
| `G` / `Ctrl+End` | Jump to file end |
| `i` | Enter insert mode |
| `a` | Append (move right then insert) |
| `o` / `O` | Insert new line below/above and enter insert |
| `x` / `Del` | Delete char under cursor |
| `dd` / `Ctrl+K` | Delete current line |
| `p` / `Ctrl+Y` | Paste most recent yank |
| `/` | Search forward |
| `?` | Search backward |
| `n` / `Shift+N` | Repeat search forward/back |
| `:` | Open command palette |
| `F1` / `:help` | Show help overlay |
| `Ctrl+S` | Save |
| `Ctrl+Q` | Quit (requires clean buffer or `:q!`) |

### Insert mode

| Keys | Action |
|------|--------|
| Printable characters | Insert at cursor |
| `Enter` | New line |
| `Backspace` | Delete before cursor |
| `Ctrl+D` | Delete under cursor |
| `Ctrl+S` | Save |
| `Esc` | Return to command mode |

## Configuration

Create `~/.ctedit.json` to override defaults. A minimal example:

```json
{
  "options": {
    "autosave_interval": 5,
    "autosave_suffix": ".bak"
  },
  "mappings": {
    "command": {
      "Ctrl+L": "refresh_screen",
      "Ctrl+S": "write"
    },
    "insert": {
      "Ctrl+K": "delete_line"
    }
  }
}
```

Bindings use human-readable names defined in `config.py`. See `HELP.md` for the complete list of commands and actions that can be mapped.

## Autosave & Recovery

The editor writes to `FILENAME.autosave` when the buffer is dirty and idle for the configured interval. On launch, if an autosave newer than the main file exists, you'll be prompted to load it.

## Running Tests

```bash
pytest challenges/Practical/'Curses Text Editor'/tests
```

## Roadmap

- Multiple buffers / tabs
- Syntax highlighting via `pygments`
- LSP integration for completions & diagnostics
- Plugin API for custom commands

Happy hacking in the terminal! :sparkles:
