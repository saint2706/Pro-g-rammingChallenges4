# Curses Text Editor – Help & Cheat Sheet

This document powers the in-app help overlay (`F1` or `:help`). It’s safe to read here or from within the editor.

## Modes

- **Command mode**: navigation, high-level actions, command palette.
- **Insert mode**: direct text entry. `Esc` returns to command mode.

## Command Palette (`:`)

Enter `:` in command mode, type a command, and press `Enter`:

| Command | Description |
|---------|-------------|
| `w` | Write buffer to disk. |
| `w filename` | Save-as using `filename`. |
| `wa` | Write and keep autosave file. |
| `q` | Quit (fails if buffer dirty). |
| `q!` | Quit discarding changes. |
| `wq` | Write and quit. |
| `open path/to/file` | Open another file. |
| `set key=value` | Override option (e.g., `set autosave_interval=5`). |
| `search term` | Search forward for `term`. |
| `s/old/new/` | Replace first occurrence on current line. |
| `s/old/new/g` | Replace all occurrences in file. |
| `help` | Display this cheat sheet. |

Commands are case insensitive.

## Keybinding Reference

Default bindings are listed below. Customize them in `config.py` or via JSON config.

### Universal

- `Ctrl+S`: Write buffer
- `Ctrl+Q`: Quit (asks to save if dirty)
- `Ctrl+L`: Redraw screen
- `F1`: Help overlay

### Command Mode

- Navigation: `h`, `j`, `k`, `l`, arrow keys
- Word navigation: `w`, `b`
- Start/end of line: `0`, `$`, `Ctrl+A`, `Ctrl+E`
- File start/end: `gg`, `G`, `Ctrl+Home`, `Ctrl+End`
- Delete: `x`, `dd`, `Ctrl+K`
- Yank/Paste: `yy` (yank line), `p`, `Ctrl+Y`
- Search: `/` (forward), `?` (backward), `n` and `Shift+N`
- Mode switching: `i`, `a`, `o`, `O`

### Insert Mode

- Insert text: any printable char
- Delete left: `Backspace`
- Delete right: `Ctrl+D`
- New line: `Enter`
- Exit insert: `Esc`

## Search Workflow

1. Press `/` (or `?` for reverse) and type your query. Live matches highlight.
2. Hit `Enter` to jump to the next match.
3. Press `n`/`Shift+N` to repeat search forward/backward.

Use `:s/old/new/` for a one-off replacement, or append `g` to replace all matches.

## Configuration Notes

- Configuration file precedence: `--config PATH` > `./ctedit.json` > `~/.ctedit.json` > defaults.
- Key names follow `config.HUMAN_READABLE_KEYS`. Example: `Ctrl+S`, `Alt+X`, `F5`.
- Options available:
  - `autosave_interval` (seconds, default 10)
  - `autosave_suffix` (default `.autosave`)
  - `tab_width` (spaces inserted when pressing `Tab`, default 4)
  - `expand_tab` (`true` to insert spaces instead of `\t`)

## Tips

- `Ctrl+G` shows file statistics (lines, words, characters).
- Autosave kicks in when the buffer is dirty and idle for the configured interval.
- Recovery: launch with `--file my.txt`; if `my.txt.autosave` is newer you’ll be prompted to load it.

Enjoy editing!
