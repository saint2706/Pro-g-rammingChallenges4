"""A minimal tiling window manager built on python-xlib.

The implementation focuses on a master/stack tiling layout, ICCCM/EWMH
basics, and keyboard-driven control. It is intentionally compact but
well-documented to serve as a practical reference implementation.
"""
from __future__ import annotations

import logging
import signal
import subprocess
import sys
from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Optional

from Xlib import X, XK, Xatom, display, error, protocol

from config import DEFAULT_CONFIG, KeyBinding, WMConfig

logging.basicConfig(level=logging.INFO, format="[%(levelname)s] %(message)s")


MODIFIER_MASKS: Dict[str, int] = {
    "Shift": X.ShiftMask,
    "Lock": X.LockMask,
    "Control": X.ControlMask,
    "Mod1": X.Mod1Mask,
    "Mod2": X.Mod2Mask,
    "Mod3": X.Mod3Mask,
    "Mod4": X.Mod4Mask,
    "Mod5": X.Mod5Mask,
}


@dataclass
class ManagedWindow:
    """Book-keeping for each client window we manage."""

    window: "display.Window"
    is_master: bool = False


class WindowManager:
    """Simple master-stack tiling WM with ICCCM/EWMH niceties."""

    def __init__(self, config: WMConfig = DEFAULT_CONFIG) -> None:
        self.config = config
        self.disp = display.Display()
        self.root = self.disp.screen().root
        self.clients: List[ManagedWindow] = []
        self.focus_index: Optional[int] = None
        self.actions: Dict[str, Callable[[], None]] = {
            "spawn_terminal": self.spawn_terminal,
            "focus_next": lambda: self.cycle_focus(+1),
            "focus_prev": lambda: self.cycle_focus(-1),
            "shift_next": lambda: self.shift_client(+1),
            "shift_prev": lambda: self.shift_client(-1),
            "close_focused": self.close_focused,
            "rotate_master": self.rotate_master,
        }
        self.supporting_window = None
        self._color_cache: Dict[str, int] = {}
        self._ignored_modifier_mask = X.LockMask | X.Mod2Mask

    # ------------------------------------------------------------------
    # Initialization & teardown
    # ------------------------------------------------------------------
    def setup(self) -> None:
        logging.info("Initializing window manager")
        self._acquire_wm_ownership()
        self._setup_ewmh()
        self._grab_keys(self.config.keybindings)
        signal.signal(signal.SIGINT, lambda *_: self.shutdown())
        signal.signal(signal.SIGTERM, lambda *_: self.shutdown())

    def _acquire_wm_ownership(self) -> None:
        event_mask = (
            X.SubstructureRedirectMask
            | X.SubstructureNotifyMask
            | X.PropertyChangeMask
            | X.EnterWindowMask
            | X.LeaveWindowMask
            | X.KeyPressMask
        )
        try:
            self.root.change_attributes(event_mask=event_mask)
        except error.BadAccess:
            logging.error("Another window manager is running. Aborting.")
            sys.exit(1)

    def _setup_ewmh(self) -> None:
        logging.debug("Setting EWMH hints")
        self.supporting_window = self.root.create_window(
            0,
            0,
            1,
            1,
            0,
            self.disp.screen().root_depth,
            X.InputOutput,
            X.CopyFromParent,
            background_pixel=0,
        )
        wm_check = self.disp.intern_atom("_NET_SUPPORTING_WM_CHECK")
        wm_name = self.disp.intern_atom("_NET_WM_NAME")
        utf8 = self.disp.intern_atom("UTF8_STRING")
        supported = self.disp.intern_atom("_NET_SUPPORTED")
        active = self.disp.intern_atom("_NET_ACTIVE_WINDOW")
        protocols = self.disp.intern_atom("WM_PROTOCOLS")
        delete_window = self.disp.intern_atom("WM_DELETE_WINDOW")

        self.root.change_property(
            wm_check, Xatom.WINDOW, 32, [self.supporting_window.id]
        )
        self.supporting_window.change_property(
            wm_check, Xatom.WINDOW, 32, [self.supporting_window.id]
        )
        self.supporting_window.change_property(
            wm_name,
            utf8,
            8,
            b'PracticalWM'
        )
        self.supporting_window.set_wm_name("PracticalWM")
        self.supporting_window.map()
        self.root.change_property(
            supported,
            Xatom.ATOM,
            32,
            [active, wm_check, wm_name, protocols, delete_window]
        )
        self.root.set_wm_name("PracticalWM")
        self.disp.flush()

    def _grab_keys(self, keybindings: Iterable[KeyBinding]) -> None:
        logging.debug("Registering keybindings")
        ignored = self._ignored_modifier_mask
        variants = [0, X.LockMask, X.Mod2Mask, X.LockMask | X.Mod2Mask]
        for binding in keybindings:
            keycode = self._keysym_to_keycode(binding.key)
            modifiers = self._modifiers_to_mask(binding.modifiers)
            for variant in variants:
                self.root.grab_key(keycode, modifiers | (variant & ignored), True, X.GrabModeAsync, X.GrabModeAsync)
        self.disp.sync()

    def shutdown(self) -> None:
        logging.info("Shutting down window manager")
        if self.supporting_window:
            self.supporting_window.destroy()
        self.disp.close()
        sys.exit(0)

    # ------------------------------------------------------------------
    # Event loop & handlers
    # ------------------------------------------------------------------
    def run(self) -> None:
        self.setup()
        logging.info("Entering event loop")
        while True:
            event = self.disp.next_event()
            handler_name = f"on_{event.__class__.__name__}"
            handler = getattr(self, handler_name, None)
            if handler:
                handler(event)

    def on_MapRequest(self, event: protocol.event.MapRequest) -> None:
        window = event.window
        logging.debug("Managing new window %s", window)
        self._manage_window(window)

    def on_DestroyNotify(self, event: protocol.event.DestroyNotify) -> None:
        self._unmanage_window(event.window)

    def on_UnmapNotify(self, event: protocol.event.UnmapNotify) -> None:
        self._unmanage_window(event.window)

    def on_ConfigureRequest(self, event: protocol.event.ConfigureRequest) -> None:
        window = event.window
        window.configure(
            x=event.x,
            y=event.y,
            width=event.width,
            height=event.height,
            border_width=event.border_width,
            stack_mode=event.detail,
            sibling=event.above,
        )
        self.tile_windows()

    def on_ClientMessage(self, event: protocol.event.ClientMessage) -> None:
        atom = event.client_type
        net_active = self.disp.intern_atom("_NET_ACTIVE_WINDOW")
        net_close = self.disp.intern_atom("_NET_CLOSE_WINDOW")
        data32 = getattr(event.data, "data32", event.data)
        if atom == net_active:
            target_id = event.window.id
            if target_id == self.root.id and data32:
                fallback = next((value for value in data32 if value), None)
                target_id = fallback if fallback is not None else target_id
            if target_id and target_id != self.root.id:
                try:
                    window = self.disp.create_resource_object("window", target_id)
                except error.XError:
                    return
                self.focus_window(window)
        elif atom == net_close:
            target = event.window if event.window != self.root else None
            if target is None and data32:
                target_id = data32[0]
                if target_id:
                    try:
                        target = self.disp.create_resource_object("window", target_id)
                    except error.XError:
                        target = None
            if target is not None:
                self._close_window(target)

    def on_KeyPress(self, event: protocol.event.KeyPress) -> None:
        keycode = event.detail
        state = event.state & ~self._ignored_modifier_mask
        for binding in self.config.keybindings:
            expected_keycode = self._keysym_to_keycode(binding.key)
            expected_state = self._modifiers_to_mask(binding.modifiers)
            if keycode == expected_keycode and state == expected_state:
                action = self.actions.get(binding.action)
                if action:
                    logging.info("Executing action: %s", binding.action)
                    action()
                break

    # ------------------------------------------------------------------
    # Window management helpers
    # ------------------------------------------------------------------
    def _manage_window(self, window: "display.Window") -> None:
        try:
            attrs = window.get_attributes()
        except error.XError:
            return
        if attrs.override_redirect:
            window.map()
            return
        window.change_attributes(event_mask=X.EnterWindowMask | X.FocusChangeMask)
        if not self.clients:
            managed = ManagedWindow(window=window, is_master=True)
            self.clients.append(managed)
            self.focus_index = 0
        else:
            managed = ManagedWindow(window=window, is_master=False)
            self.clients.append(managed)
            self.focus_index = len(self.clients) - 1
        window.map()
        self.tile_windows()
        self.focus_window(managed.window)

    def _unmanage_window(self, window: "display.Window") -> None:
        for idx, managed in enumerate(self.clients):
            if managed.window == window:
                logging.debug("Unmanaging window %s", window)
                del self.clients[idx]
                if self.focus_index is not None:
                    if idx <= self.focus_index:
                        self.focus_index = max(0, self.focus_index - 1)
                    if not self.clients:
                        self.focus_index = None
                break
        self.tile_windows()
        self.focus_current()

    def tile_windows(self) -> None:
        if not self.clients:
            return
        geom = self.root.get_geometry()
        gap = self.config.gap_px
        master_ratio = self.config.master_ratio

        master = next((c for c in self.clients if c.is_master), None)
        if not master:
            self.clients[0].is_master = True
            master = self.clients[0]

        stack = [c for c in self.clients if c is not master]
        width = geom.width
        height = geom.height

        if stack:
            master_width = int(width * master_ratio)
            stack_width = width - master_width
        else:
            master_width = width
            stack_width = 0

        self._configure_window(master.window, gap, gap, master_width - 2 * gap, height - 2 * gap)

        if stack:
            stack_height = height - 2 * gap
            y = gap
            remaining = stack_height
            for index, client in enumerate(stack):
                slots_left = len(stack) - index
                allocation = remaining // slots_left
                remaining -= allocation
                self._configure_window(
                    client.window,
                    master_width + gap,
                    y,
                    stack_width - 2 * gap,
                    allocation - gap,
                )
                y += allocation

        self.disp.sync()

    def _configure_window(self, window: "display.Window", x: int, y: int, width: int, height: int) -> None:
        window.configure(x=x, y=y, width=max(width, 1), height=max(height, 1), border_width=self.config.border_width)
        window.map()

    # ------------------------------------------------------------------
    # Focus management
    # ------------------------------------------------------------------
    def focus_window(self, window: "display.Window") -> None:
        for idx, client in enumerate(self.clients):
            if client.window == window:
                self.focus_index = idx
                break
        active_atom = self.disp.intern_atom("_NET_ACTIVE_WINDOW")
        self.root.change_property(active_atom, Xatom.WINDOW, 32, [window.id])
        self.disp.set_input_focus(window, X.RevertToPointerRoot, X.CurrentTime)
        self._set_border_colors(window)
        self.disp.flush()

    def focus_current(self) -> None:
        if self.focus_index is None or not self.clients:
            return
        window = self.clients[self.focus_index].window
        self.focus_window(window)

    def cycle_focus(self, delta: int) -> None:
        if not self.clients:
            return
        if self.focus_index is None:
            self.focus_index = 0
        else:
            self.focus_index = (self.focus_index + delta) % len(self.clients)
        self.focus_current()

    def shift_client(self, delta: int) -> None:
        if self.focus_index is None or not self.clients:
            return
        new_index = (self.focus_index + delta) % len(self.clients)
        self.clients[self.focus_index], self.clients[new_index] = (
            self.clients[new_index],
            self.clients[self.focus_index],
        )
        self.focus_index = new_index
        self.tile_windows()
        self.focus_current()

    def rotate_master(self) -> None:
        if not self.clients:
            return
        for client in self.clients:
            client.is_master = False
        if self.focus_index is None:
            self.focus_index = 0
        entry = self.clients.pop(self.focus_index)
        self.clients.insert(0, entry)
        for idx, client in enumerate(self.clients):
            client.is_master = idx == 0
        self.focus_index = 0
        self.tile_windows()
        self.focus_current()

    def close_focused(self) -> None:
        if self.focus_index is None:
            return
        window = self.clients[self.focus_index].window
        self._close_window(window)

    def _close_window(self, window: "display.Window") -> None:
        wm_protocols = self.disp.intern_atom("WM_PROTOCOLS")
        delete_atom = self.disp.intern_atom("WM_DELETE_WINDOW")
        try:
            protocols = window.get_wm_protocols()
        except error.XError:
            protocols = None
        if protocols and delete_atom in protocols:
            event = protocol.event.ClientMessage(
                window=window,
                client_type=wm_protocols,
                data=(32, [delete_atom, X.CurrentTime, 0, 0, 0]),
            )
            window.send_event(event, event_mask=X.NoEventMask, propagate=False)
        else:
            window.kill_client()
        self.disp.flush()

    def _set_border_colors(self, focused_window: "display.Window") -> None:
        focus_pixel = self._color_pixel(self.config.focused_border_color)
        unfocus_pixel = self._color_pixel(self.config.unfocused_border_color)
        for client in self.clients:
            pixel = focus_pixel if client.window == focused_window else unfocus_pixel
            client.window.change_attributes(border_pixel=pixel)
        self.disp.flush()

    # ------------------------------------------------------------------
    # Actions
    # ------------------------------------------------------------------
    def spawn_terminal(self) -> None:
        try:
            subprocess.Popen(self.config.terminal_command)
        except FileNotFoundError:
            logging.error("Terminal command %s not found", self.config.terminal_command)

    # ------------------------------------------------------------------
    # Utility helpers
    # ------------------------------------------------------------------
    def _keysym_to_keycode(self, key: str) -> int:
        keysym = XK.string_to_keysym(key)
        if keysym == 0:
            raise ValueError(f"Unknown key: {key}")
        return self.disp.keysym_to_keycode(keysym)

    def _modifiers_to_mask(self, modifiers: Iterable[str]) -> int:
        mask = 0
        for mod in modifiers:
            mask |= MODIFIER_MASKS.get(mod, 0)
        return mask



    def _color_pixel(self, color: str) -> int:
        if color not in self._color_cache:
            colormap = self.disp.screen().default_colormap
            parsed = colormap.alloc_named_color(color)
            self._color_cache[color] = parsed.pixel
        return self._color_cache[color]


def main() -> None:
    wm = WindowManager()
    wm.run()


if __name__ == "__main__":
    main()
