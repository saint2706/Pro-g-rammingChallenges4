# PracticalWM – Lightweight X11 Tiling Window Manager

## 1. Scope & Design Plan

**Goal:** build a concise tiling window manager for X11 that demonstrates
core concepts (event loop, tiling layout, focus control, keybindings) and
touches essential ICCCM/EWMH compliance. The project targets educational
value over feature breadth.

**Key capabilities**

- Master/stack tiling layout with configurable gap/border sizes.
- Keyboard-driven control (focus cycling, swapping, promote to master,
  spawn terminal, close focused window).
- Focus tracking, border highlighting, `_NET_ACTIVE_WINDOW` updates.
- ICCCM/EWMH basics: `_NET_SUPPORTING_WM_CHECK`, `_NET_SUPPORTED`,
  UTF-8 WM name, graceful WM_DELETE_WINDOW handling.
- Declarative configuration for keybindings and style.

**Out of scope (for now)**

- Floating windows, workspaces/tags, mouse-driven focus.
- Drag-resize/rearrange, complex EWMH hints beyond essentials.
- Wayland support – strictly X11 via `python-xlib`.

## 2. Build & Runtime Requirements

1. **System packages** (Debian/Ubuntu example):
   ```bash
   sudo apt install python3 python3-pip python3-xlib xterm x11-xserver-utils
   ```
   > `python3-xlib` provides the `python-xlib` bindings; adjust package
   > names for other distributions (e.g., `pip install python-xlib`).

2. **Optional virtual environment** (recommended if running from repo root):
   ```bash
   python -m venv .venv
   source .venv/bin/activate
   pip install -r ../requirements.txt  # includes python-xlib pin once added
   ```

3. **Running inside a nested X server (safe development workflow):**
   ```bash
   Xephyr :1 -screen 1280x800 &
   DISPLAY=:1 python wm.py
   ```
   Launch test clients (e.g., `DISPLAY=:1 xterm`, `DISPLAY=:1 xclock`) to see
   tiling behavior without replacing your main desktop session.

## 3. Usage & Controls

Default modifier is `Mod4` (the Super/Windows key). Commands:

| Keybinding         | Action                     |
|--------------------|----------------------------|
| `Mod4 + Return`    | Spawn configured terminal  |
| `Mod4 + j/k`       | Focus next/previous window |
| `Mod4 + Shift + j/k` | Swap window with neighbor |
| `Mod4 + Space`     | Promote focused window to master slot |
| `Mod4 + q`         | Gracefully close focused client |

Configuration lives in [`config.py`](config.py); adjust terminal command,
colors, gaps, or bindings as needed.

## 4. Manual Validation Checklist

1. Run `python wm.py` within a nested display (`Xephyr`) to start the WM.
2. Launch two or more X clients; verify master/stack tiling layout updates.
3. Use `Mod4 + j/k` to move focus and confirm border color updates and
   `_NET_ACTIVE_WINDOW` changes (use `xprop -root _NET_ACTIVE_WINDOW`).
4. Swap windows with `Mod4 + Shift + j/k` and promote to master with
   `Mod4 + Space`; ensure layout retiled appropriately.
5. Close a client using `Mod4 + q` and confirm focus falls back to remaining
   window(s) with tiling recomputed.

## 5. Implementation Notes

- The main event loop listens for `MapRequest`, `ConfigureRequest`,
  `DestroyNotify`, and `KeyPress` events.
- Layout is recalculated on every relevant event to keep geometry
  deterministic.
- ICCCM/EWMH support is minimal but enough for standard utilities to
  interact (e.g., `wmctrl`, `xprop`). Extend `_setup_ewmh` for more hints if
  needed.

## 6. Completion Status

✅ Challenge complete – core window manager, configuration, documentation,
   and validation steps are in place.
