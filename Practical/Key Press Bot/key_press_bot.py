"""CLI entry point for the Key Press Bot automation toolkit."""
from __future__ import annotations

import argparse
import json
import sys
import threading
import time
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, Iterable, List, Optional

try:
    from pynput import keyboard
except Exception as exc:  # pragma: no cover - import guard
    keyboard = None  # type: ignore[assignment]
    _KEYBOARD_IMPORT_ERROR = exc
else:  # pragma: no cover - exercised in functional environments
    _KEYBOARD_IMPORT_ERROR = None

try:
    import pyautogui
except Exception as exc:  # pragma: no cover - import guard
    pyautogui = None  # type: ignore[assignment]
    _PYAUTOGUI_IMPORT_ERROR = exc
else:  # pragma: no cover - exercised in functional environments
    _PYAUTOGUI_IMPORT_ERROR = None
    pyautogui.FAILSAFE = False
    pyautogui.PAUSE = 0


DEFAULT_PLAY_START = "<ctrl>+<alt>+p"
DEFAULT_PLAY_STOP = "<ctrl>+<alt>+."
DEFAULT_RECORD_STOP = "<ctrl>+<alt>+q"


@dataclass
class SequenceStep:
    """High-level instructions for scripted playback."""

    action: str
    keys: List[str] = field(default_factory=list)
    text: Optional[str] = None
    seconds: float = 0.0
    hold: float = 0.1
    delay_after: float = 0.0

    @classmethod
    def from_dict(cls, payload: Dict[str, Any]) -> "SequenceStep":
        action = payload.get("action")
        if not action:
            raise ValueError("Script step is missing required 'action' field")
        keys = [str(key).lower() for key in payload.get("keys", [])]
        text = payload.get("text")
        seconds = float(payload.get("seconds", payload.get("delay", 0.0)))
        hold = float(payload.get("hold", 0.1))
        delay_after = float(payload.get("delay_after", payload.get("after", 0.0)))
        return cls(action=action, keys=keys, text=text, seconds=seconds, hold=hold, delay_after=delay_after)

    def to_dict(self) -> Dict[str, Any]:
        data: Dict[str, Any] = {
            "action": self.action,
            "keys": self.keys,
            "hold": self.hold,
        }
        if self.text is not None:
            data["text"] = self.text
        if self.seconds:
            data["seconds"] = self.seconds
        if self.delay_after:
            data["delay_after"] = self.delay_after
        return data


@dataclass
class RecordedEvent:
    """Low-level key press/release information captured by the recorder."""

    timestamp: float
    event: str
    key: str

    @classmethod
    def from_dict(cls, payload: Dict[str, Any]) -> "RecordedEvent":
        return cls(timestamp=float(payload["timestamp"]), event=str(payload["event"]), key=str(payload["key"]))

    def to_dict(self) -> Dict[str, Any]:
        return {"timestamp": self.timestamp, "event": self.event, "key": self.key}


@dataclass
class Script:
    """Container that encapsulates the two supported automation formats."""

    mode: str
    name: str
    metadata: Dict[str, Any]
    sequence_steps: List[SequenceStep] = field(default_factory=list)
    recorded_events: List[RecordedEvent] = field(default_factory=list)

    @property
    def is_recording(self) -> bool:
        return self.mode == "recording"


def load_script(path: Path) -> Script:
    data = json.loads(path.read_text(encoding="utf-8"))
    mode = data.get("mode", "sequence")
    metadata = data.get("meta", {})
    name = metadata.get("name") or path.stem

    if mode == "sequence":
        steps = [SequenceStep.from_dict(item) for item in data.get("steps", [])]
        return Script(mode=mode, name=name, metadata=metadata, sequence_steps=steps)

    if mode == "recording":
        events = [RecordedEvent.from_dict(item) for item in data.get("events", data.get("steps", []))]
        return Script(mode=mode, name=name, metadata=metadata, recorded_events=events)

    raise ValueError(f"Unsupported script mode: {mode}")


def _require_keyboard() -> Any:
    if keyboard is None:
        message = "pynput keyboard backend is unavailable."
        if _KEYBOARD_IMPORT_ERROR is not None:
            message += f" ({_KEYBOARD_IMPORT_ERROR})"
        raise RuntimeError(message)
    return keyboard


def _require_pyautogui() -> Any:
    if pyautogui is None:
        message = "pyautogui automation backend is unavailable."
        if _PYAUTOGUI_IMPORT_ERROR is not None:
            message += f" ({_PYAUTOGUI_IMPORT_ERROR})"
        raise RuntimeError(message)
    return pyautogui


if keyboard is not None:
    _KEY_NAME_MAP: Dict[str, Any] = {
        "alt": keyboard.Key.alt,
        "alt_l": keyboard.Key.alt_l,
        "alt_r": keyboard.Key.alt_r,
        "backspace": keyboard.Key.backspace,
        "caps_lock": keyboard.Key.caps_lock,
        "cmd": keyboard.Key.cmd,
        "command": keyboard.Key.cmd,
        "ctrl": keyboard.Key.ctrl,
        "control": keyboard.Key.ctrl,
        "ctrl_l": keyboard.Key.ctrl_l,
        "ctrl_r": keyboard.Key.ctrl_r,
        "delete": keyboard.Key.delete,
        "down": keyboard.Key.down,
        "end": keyboard.Key.end,
        "enter": keyboard.Key.enter,
        "esc": keyboard.Key.esc,
        "escape": keyboard.Key.esc,
        "f1": keyboard.Key.f1,
        "f2": keyboard.Key.f2,
        "f3": keyboard.Key.f3,
        "f4": keyboard.Key.f4,
        "f5": keyboard.Key.f5,
        "f6": keyboard.Key.f6,
        "f7": keyboard.Key.f7,
        "f8": keyboard.Key.f8,
        "f9": keyboard.Key.f9,
        "f10": keyboard.Key.f10,
        "f11": keyboard.Key.f11,
        "f12": keyboard.Key.f12,
        "home": keyboard.Key.home,
        "insert": keyboard.Key.insert,
        "left": keyboard.Key.left,
        "menu": keyboard.Key.menu,
        "page_down": keyboard.Key.page_down,
        "page_up": keyboard.Key.page_up,
        "pause": keyboard.Key.pause,
        "print_screen": keyboard.Key.print_screen,
        "right": keyboard.Key.right,
        "shift": keyboard.Key.shift,
        "shift_l": keyboard.Key.shift_l,
        "shift_r": keyboard.Key.shift_r,
        "space": keyboard.Key.space,
        "tab": keyboard.Key.tab,
        "up": keyboard.Key.up,
        "win": keyboard.Key.cmd,
    }
else:  # pragma: no cover - occurs only in headless CI / unsupported envs
    _KEY_NAME_MAP = {}


def parse_key(name: str) -> Any:
    kb = _require_keyboard()
    key_name = name.lower()
    if key_name in _KEY_NAME_MAP:
        return _KEY_NAME_MAP[key_name]
    if key_name.startswith("f") and key_name[1:].isdigit():
        attr = getattr(kb.Key, key_name, None)
        if attr is not None:
            return attr
    if len(key_name) == 1:
        return kb.KeyCode.from_char(key_name)
    if key_name.startswith("vk_"):
        code = int(key_name[3:], 16)
        return kb.KeyCode.from_vk(code)
    raise ValueError(f"Unsupported key identifier: {name}")


def serialise_key(key: Any) -> str:
    kb = keyboard
    if kb is not None and isinstance(key, kb.KeyCode):
        if key.char:
            return key.char.lower()
        if key.vk:
            return f"vk_{key.vk:02x}"
    if kb is not None and isinstance(key, kb.Key):
        return key.name
    return str(key)


class KeyPressBot:
    """Execute scripted or recorded keyboard sequences."""

    def __init__(
        self,
        controller: Optional[keyboard.Controller] = None,
        typist: Optional[Any] = None,
    ) -> None:
        kb = _require_keyboard()
        self.controller = controller or kb.Controller()
        self.typist = typist or _require_pyautogui()

    def play(self, script: Script, stop_event: Optional[threading.Event] = None) -> None:
        if script.is_recording:
            self._play_recorded(script.recorded_events, stop_event)
        else:
            self._play_sequence(script.sequence_steps, stop_event)

    def _play_sequence(self, steps: Iterable[SequenceStep], stop_event: Optional[threading.Event]) -> None:
        for step in steps:
            if _should_stop(stop_event):
                break
            action = step.action.lower()
            if action == "sleep":
                _sleep(step.seconds, stop_event)
            elif action in {"delay", "wait"}:
                _sleep(step.seconds or step.delay_after, stop_event)
            elif action in {"press", "hotkey"}:
                self._press_combo(step.keys, step.hold)
            elif action in {"type", "type_text", "text"}:
                if step.text is None:
                    raise ValueError("Type action requires 'text'")
                self.typist.write(step.text)
            elif action == "key_down":
                for key_name in step.keys:
                    self.controller.press(parse_key(key_name))
            elif action == "key_up":
                for key_name in step.keys:
                    self.controller.release(parse_key(key_name))
            else:
                raise ValueError(f"Unknown action '{step.action}' in script")

            if step.delay_after and not _should_stop(stop_event):
                _sleep(step.delay_after, stop_event)

    def _play_recorded(self, events: Iterable[RecordedEvent], stop_event: Optional[threading.Event]) -> None:
        last_timestamp = 0.0
        for event in events:
            if _should_stop(stop_event):
                break
            wait = max(0.0, event.timestamp - last_timestamp)
            if wait:
                _sleep(wait, stop_event)
            key = parse_key(event.key)
            if event.event == "press":
                self.controller.press(key)
            elif event.event == "release":
                self.controller.release(key)
            else:
                raise ValueError(f"Unknown recorded event '{event.event}'")
            last_timestamp = event.timestamp

    def _press_combo(self, keys: Iterable[str], hold: float) -> None:
        parsed_keys = [parse_key(name) for name in keys]
        for key in parsed_keys:
            self.controller.press(key)
        if hold > 0:
            time.sleep(hold)
        for key in reversed(parsed_keys):
            self.controller.release(key)


def _sleep(duration: float, stop_event: Optional[threading.Event]) -> None:
    target = time.time() + max(0.0, duration)
    while time.time() < target:
        if _should_stop(stop_event):
            break
        time.sleep(0.01)


def _should_stop(stop_event: Optional[threading.Event]) -> bool:
    return bool(stop_event and stop_event.is_set())


class KeyRecorder:
    """Capture live keyboard input and convert it to a replayable recording."""

    def __init__(self, stop_hotkey: str = DEFAULT_RECORD_STOP) -> None:
        self.stop_hotkey = stop_hotkey
        self._stop_event = threading.Event()

    def record(self) -> List[RecordedEvent]:
        self._stop_event.clear()
        events: List[RecordedEvent] = []
        start_time: Optional[float] = None
        kb = _require_keyboard()

        def on_press(key: Any) -> None:
            nonlocal start_time
            if self._stop_event.is_set():
                return
            now = time.time()
            if start_time is None:
                start_time = now
            events.append(RecordedEvent(timestamp=now - start_time, event="press", key=serialise_key(key)))

        def on_release(key: Any) -> None:
            nonlocal start_time
            if self._stop_event.is_set():
                return
            if start_time is None:
                return
            now = time.time()
            events.append(RecordedEvent(timestamp=now - start_time, event="release", key=serialise_key(key)))

        def stop_listener() -> None:
            self._stop_event.set()

        with kb.Listener(on_press=on_press, on_release=on_release) as listener, kb.GlobalHotKeys(
            {self.stop_hotkey: stop_listener}
        ) as hotkeys:
            while not self._stop_event.is_set():
                time.sleep(0.05)

        listener.stop()
        hotkeys.stop()

        filtered = _trim_stop_combination(events, self.stop_hotkey)
        return filtered


def _trim_stop_combination(events: List[RecordedEvent], combo: str) -> List[RecordedEvent]:
    combo_keys = set(_normalise_hotkey(combo))
    trimmed = list(events)
    while trimmed and trimmed[-1].key in combo_keys:
        trimmed.pop()
    return trimmed


def _normalise_hotkey(hotkey: str) -> List[str]:
    parts = [segment.strip() for segment in hotkey.split("+")]
    normalised: List[str] = []
    for part in parts:
        if part.startswith("<") and part.endswith(">"):
            part = part[1:-1]
        normalised.append(part.lower())
    return normalised


class HotkeyPlayback:
    """Global hotkey controller to trigger playback of a script."""

    def __init__(
        self,
        bot: KeyPressBot,
        script: Script,
        start_hotkey: str = DEFAULT_PLAY_START,
        stop_hotkey: str = DEFAULT_PLAY_STOP,
    ) -> None:
        self.bot = bot
        self.script = script
        self.start_hotkey = start_hotkey
        self.stop_hotkey = stop_hotkey
        self._stop_event = threading.Event()
        self._thread: Optional[threading.Thread] = None
        self._keyboard = _require_keyboard()

    def run(self) -> None:
        with self._keyboard.GlobalHotKeys(
            {
                self.start_hotkey: self._start_playback,
                self.stop_hotkey: self._signal_stop,
            }
        ) as hotkeys:
            hotkeys.join()

    def _start_playback(self) -> None:
        if self._thread and self._thread.is_alive():
            return
        self._stop_event.clear()
        self._thread = threading.Thread(target=self._playback, name="KeyPressBotPlayback", daemon=True)
        self._thread.start()

    def _playback(self) -> None:
        try:
            self.bot.play(self.script, stop_event=self._stop_event)
        finally:
            self._stop_event.clear()

    def _signal_stop(self) -> None:
        self._stop_event.set()


def save_recording(events: Iterable[RecordedEvent], path: Path, metadata: Optional[Dict[str, Any]] = None) -> None:
    payload = {
        "mode": "recording",
        "meta": metadata or {},
        "events": [event.to_dict() for event in events],
    }
    path.write_text(json.dumps(payload, indent=2), encoding="utf-8")


def build_arg_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Keyboard automation toolkit with recording and scripting support.")
    parser.add_argument("--script", type=Path, help="Path to a JSON script to execute", default=None)
    parser.add_argument("--record", type=Path, help="Record keystrokes to this JSON file", default=None)
    parser.add_argument("--play", type=Path, help="Play back a recording JSON file", default=None)
    parser.add_argument("--watch-hotkeys", action="store_true", help="Listen for global hotkeys to start/stop playback")
    parser.add_argument("--start-hotkey", default=DEFAULT_PLAY_START, help="Override the playback start hotkey")
    parser.add_argument("--stop-hotkey", default=DEFAULT_PLAY_STOP, help="Override the playback stop hotkey")
    parser.add_argument("--record-stop-hotkey", default=DEFAULT_RECORD_STOP, help="Hotkey to stop a recording session")
    return parser


def main(argv: Optional[List[str]] = None) -> None:
    parser = build_arg_parser()
    args = parser.parse_args(argv)

    if args.record:
        try:
            recorder = KeyRecorder(stop_hotkey=args.record_stop_hotkey)
        except RuntimeError as err:
            print(f"Error: {err}", file=sys.stderr)
            return
        print(f"Recording... Press {args.record_stop_hotkey} to stop.")
        events = recorder.record()
        save_recording(events, args.record, metadata={"name": args.record.stem})
        print(f"Saved {len(events)} events to {args.record}")
        return

    try:
        bot = KeyPressBot()
    except RuntimeError as err:
        print(f"Error: {err}", file=sys.stderr)
        return

    if args.play:
        script = load_script(args.play)
        if args.watch_hotkeys:
            runner = HotkeyPlayback(bot, script, start_hotkey=args.start_hotkey, stop_hotkey=args.stop_hotkey)
            print("Listening for hotkeys. Press Ctrl+C to exit.")
            try:
                runner.run()
            except KeyboardInterrupt:
                pass
        else:
            bot.play(script)
        return

    if args.script:
        script = load_script(args.script)
        bot.play(script)
        return

    parser.print_help()


if __name__ == "__main__":  # pragma: no cover - manual invocation
    main()
