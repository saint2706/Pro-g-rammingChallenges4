import importlib.util
import sys
from pathlib import Path

import pytest

MODULE_PATH = Path(__file__).resolve().parents[1] / "key_press_bot.py"
spec = importlib.util.spec_from_file_location("key_press_bot", MODULE_PATH)
module = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = module
spec.loader.exec_module(module)  # type: ignore[attr-defined]

SequenceStep = module.SequenceStep
Script = module.Script
RecordedEvent = module.RecordedEvent
KeyPressBot = module.KeyPressBot
parse_key = module.parse_key
save_recording = module.save_recording
load_script = module.load_script
_trim_stop_combination = module._trim_stop_combination
_normalise_hotkey = module._normalise_hotkey
DEFAULT_PLAY_STOP = module.DEFAULT_PLAY_STOP
keyboard_module = getattr(module, "keyboard", None)

if keyboard_module is None:  # pragma: no cover - depends on CI environment
    pytest.skip("pynput keyboard backend unavailable", allow_module_level=True)


class DummyController:
    def __init__(self) -> None:
        self.pressed = []
        self.released = []

    def press(self, key) -> None:  # pragma: no cover - simple recording
        self.pressed.append(key)

    def release(self, key) -> None:  # pragma: no cover - simple recording
        self.released.append(key)


class DummyTypist:
    def __init__(self) -> None:
        self.written = []

    def write(self, text: str) -> None:
        self.written.append(text)


def test_sequence_step_from_dict_defaults() -> None:
    step = SequenceStep.from_dict({"action": "press", "keys": ["ctrl", "c"]})
    assert step.action == "press"
    assert step.keys == ["ctrl", "c"]
    assert pytest.approx(step.hold, rel=1e-5) == 0.1


def test_parse_key_recognises_special_keys() -> None:
    key = parse_key("enter")
    from pynput import keyboard

    assert key == keyboard.Key.enter


def test_key_press_bot_runs_sequence_without_real_typing() -> None:
    controller = DummyController()
    typist = DummyTypist()
    script = Script(
        mode="sequence",
        name="demo",
        metadata={},
        sequence_steps=[
            SequenceStep(action="press", keys=["ctrl", "a"], hold=0),
            SequenceStep(action="type_text", text="hello"),
        ],
    )

    bot = KeyPressBot(controller=controller, typist=typist)
    bot.play(script)

    assert len(controller.pressed) == 2
    assert len(controller.released) == 2
    assert typist.written == ["hello"]


def test_load_and_save_recording_roundtrip(tmp_path: Path) -> None:
    events = [
        RecordedEvent(timestamp=0.0, event="press", key="ctrl"),
        RecordedEvent(timestamp=0.1, event="release", key="ctrl"),
    ]
    out_path = tmp_path / "recording.json"
    save_recording(events, out_path, metadata={"name": "test"})

    loaded = load_script(out_path)
    assert loaded.is_recording
    assert loaded.recorded_events[0].event == "press"


def test_trim_stop_combination_removes_stop_keys() -> None:
    events = [
        RecordedEvent(timestamp=0.0, event="press", key="ctrl"),
        RecordedEvent(timestamp=0.2, event="release", key="ctrl"),
        RecordedEvent(timestamp=0.3, event="press", key="ctrl"),
        RecordedEvent(timestamp=0.31, event="press", key="alt"),
        RecordedEvent(timestamp=0.32, event="press", key="."),
    ]
    trimmed = _trim_stop_combination(events, DEFAULT_PLAY_STOP)
    assert len(trimmed) == 3


def test_normalise_hotkey_handles_brackets() -> None:
    assert _normalise_hotkey("<ctrl>+<alt>+.") == ["ctrl", "alt", "."]
