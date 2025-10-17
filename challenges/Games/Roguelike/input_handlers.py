"""Input handling for the roguelike."""

from __future__ import annotations

from typing import Dict, Tuple

import tcod.event

MOVE_KEYS: Dict[int, Tuple[int, int]] = {
    tcod.event.K_UP: (0, -1),
    tcod.event.K_DOWN: (0, 1),
    tcod.event.K_LEFT: (-1, 0),
    tcod.event.K_RIGHT: (1, 0),
    tcod.event.K_HOME: (-1, -1),
    tcod.event.K_END: (-1, 1),
    tcod.event.K_PAGEUP: (1, -1),
    tcod.event.K_PAGEDOWN: (1, 1),
    tcod.event.K_KP_1: (-1, 1),
    tcod.event.K_KP_2: (0, 1),
    tcod.event.K_KP_3: (1, 1),
    tcod.event.K_KP_4: (-1, 0),
    tcod.event.K_KP_5: (0, 0),
    tcod.event.K_KP_6: (1, 0),
    tcod.event.K_KP_7: (-1, -1),
    tcod.event.K_KP_8: (0, -1),
    tcod.event.K_KP_9: (1, -1),
}


class EventHandler(tcod.event.EventDispatch[None]):
    def __init__(self, engine: "Engine") -> None:
        super().__init__()
        self.engine = engine

    def ev_keydown(self, event: tcod.event.KeyDown) -> None:
        if event.sym in MOVE_KEYS:
            dx, dy = MOVE_KEYS[event.sym]
            self.engine.handle_player_movement(dx, dy)
            return
        if event.sym == tcod.event.K_PERIOD:
            self.engine.message_log.add("You wait for a moment.")
            self.engine.handle_enemy_turns()
            return
        if event.sym == tcod.event.K_g:
            self.engine.handle_pickup()
            return
        if event.sym == tcod.event.K_d:
            self.engine.handle_drop()
            return
        if event.sym == tcod.event.K_i:
            self.engine.message_log.add(self.engine.describe_inventory())
            return
        if event.sym == tcod.event.K_u:
            self.engine.handle_use()
            return
        if event.sym in (tcod.event.K_ESCAPE,):
            raise SystemExit()

    def ev_quit(self, event: tcod.event.Quit) -> None:
        raise SystemExit()
