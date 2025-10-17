"""Save and load support for the RPG engine."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Dict, Optional

from .constants import SAVE_PATH
from .entity import Entity


def save_game(player: Entity, world_state: Dict[str, object]) -> None:
    save_data = {
        "player": player.to_dict(),
        "world": world_state,
    }
    Path(SAVE_PATH).parent.mkdir(parents=True, exist_ok=True)
    Path(SAVE_PATH).write_text(json.dumps(save_data, indent=2))


def load_game() -> Optional[Dict[str, object]]:
    save_file = Path(SAVE_PATH)
    if not save_file.exists():
        return None
    return json.loads(save_file.read_text())
