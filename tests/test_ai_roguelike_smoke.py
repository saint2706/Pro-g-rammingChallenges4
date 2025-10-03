"""Smoke tests for the AI Roguelike agent."""

from __future__ import annotations

from pathlib import Path
import sys
import types

import numpy as np

import pytest

# Ensure the ai_roguelike package (stored under Artificial Intelligence) is importable.
PACKAGE_DIR = (
    Path(__file__).resolve().parents[1]
    / "Artificial Intelligence"
    / "AI Roguelike"
)
if str(PACKAGE_DIR) not in sys.path:
    sys.path.insert(0, str(PACKAGE_DIR))


def _install_tcod_stub() -> None:
    """Install a minimal ``tcod`` stub so tests can run without the real library."""

    if "tcod" in sys.modules:
        return

    tcod = types.ModuleType("tcod")

    # tcod.map
    tcod_map = types.ModuleType("tcod.map")

    def compute_fov(transparency, pov, radius, algorithm=None):
        width, height = transparency.shape
        visible = np.zeros((width, height), dtype=bool)
        px, py = pov
        for x in range(width):
            for y in range(height):
                if abs(x - px) + abs(y - py) <= radius:
                    visible[x, y] = True
        return visible

    tcod_map.compute_fov = compute_fov

    # tcod.console
    tcod_console = types.ModuleType("tcod.console")

    class Console:
        def __init__(self, width, height, order="F") -> None:  # pragma: no cover - stub
            self.width = width
            self.height = height

    tcod_console.Console = Console

    # tcod.context
    tcod_context = types.ModuleType("tcod.context")

    class _DummyContext:
        def __enter__(self):  # pragma: no cover - stub
            return self

        def __exit__(self, exc_type, exc, tb) -> None:  # pragma: no cover - stub
            return None

        def convert_event(self, event) -> None:  # pragma: no cover - stub
            return None

        def present(self, console) -> None:  # pragma: no cover - stub
            return None

    def new(*args, **kwargs):  # pragma: no cover - stub
        return _DummyContext()

    tcod_context.new = new

    # tcod.event
    tcod_event = types.ModuleType("tcod.event")

    class Event:  # pragma: no cover - stub
        pass

    class KeyDown(Event):  # pragma: no cover - stub
        def __init__(self, sym: int) -> None:
            self.sym = sym

    class Quit(Event):  # pragma: no cover - stub
        pass

    class EventDispatch:  # pragma: no cover - stub
        def __class_getitem__(cls, item):  # pragma: no cover - stub
            return cls

        def dispatch(self, event):
            method_name = f"ev_{event.__class__.__name__.lower()}"
            handler = getattr(self, method_name, None)
            if handler:
                return handler(event)
            return None

    def wait():  # pragma: no cover - stub
        return []

    for code, name in enumerate(
        [
            "K_UP",
            "K_DOWN",
            "K_LEFT",
            "K_RIGHT",
            "K_HOME",
            "K_END",
            "K_PAGEUP",
            "K_PAGEDOWN",
            "K_KP_1",
            "K_KP_2",
            "K_KP_3",
            "K_KP_4",
            "K_KP_5",
            "K_KP_6",
            "K_KP_7",
            "K_KP_8",
            "K_KP_9",
            "K_PERIOD",
            "K_g",
            "K_d",
            "K_i",
            "K_u",
            "K_ESCAPE",
        ]
    ):
        setattr(tcod_event, name, code)

    tcod_event.Event = Event
    tcod_event.KeyDown = KeyDown
    tcod_event.Quit = Quit
    tcod_event.EventDispatch = EventDispatch
    tcod_event.wait = wait

    # tcod.tileset
    tcod_tileset = types.ModuleType("tcod.tileset")

    def load_truetype_font(*args, **kwargs):  # pragma: no cover - stub
        class _TileSet:
            pass

        return _TileSet()

    tcod_tileset.load_truetype_font = load_truetype_font

    # tcod.path
    tcod_path = types.ModuleType("tcod.path")

    class SimpleGraph:  # pragma: no cover - stub
        def __init__(self, cost, cardinal=2, diagonal=3) -> None:
            self.cost = cost

    class Pathfinder:  # pragma: no cover - stub
        def __init__(self, graph: SimpleGraph) -> None:
            self._root = (0, 0)

        def add_root(self, root) -> None:
            self._root = root

        def path_to(self, target):
            return np.array([self._root, target], dtype=object)

    tcod_path.SimpleGraph = SimpleGraph
    tcod_path.Pathfinder = Pathfinder

    # Register modules
    tcod.console = tcod_console
    tcod.context = tcod_context
    tcod.event = tcod_event
    tcod.tileset = tcod_tileset
    tcod.map = tcod_map
    tcod.path = tcod_path
    tcod.FOV_RESTRICTIVE = 0

    sys.modules["tcod"] = tcod
    sys.modules["tcod.console"] = tcod_console
    sys.modules["tcod.context"] = tcod_context
    sys.modules["tcod.event"] = tcod_event
    sys.modules["tcod.tileset"] = tcod_tileset
    sys.modules["tcod.map"] = tcod_map
    sys.modules["tcod.path"] = tcod_path


_install_tcod_stub()

from ai_roguelike.environment import create_sample_environment, render_ascii
from ai_roguelike.mcts import MCTSAgent


@pytest.mark.parametrize("seed", [7, 21])
def test_agent_advances_through_sample_dungeon(seed: int) -> None:
    env = create_sample_environment(seed=seed)
    agent = MCTSAgent(iterations=32, rollout_depth=3, exploration_weight=1.1)

    max_turns = 6
    for _ in range(max_turns):
        if env.is_terminal:
            break
        action, _ = agent.choose_action(env)
        reward, done = env.apply_action(action)
        # The heuristic reward is bounded and should not be NaN/inf.
        assert reward == reward  # NaN guard
        assert abs(reward) < 1_000
        if done:
            break

    summary = env.summary()
    assert summary["turn"] <= max_turns
    # Either the player is still alive or the run terminated due to success/failure.
    assert env.engine.player.is_alive or env.is_terminal


def test_ascii_renderer_contains_player_glyph() -> None:
    env = create_sample_environment(seed=11)
    ascii_map = render_ascii(env.engine, reveal=True)
    assert "@" in ascii_map
