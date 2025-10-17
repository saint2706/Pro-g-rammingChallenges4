from __future__ import annotations

import importlib.util
import sys
from pathlib import Path
from typing import Callable


def load_aste_module():
    module_path = (
        Path(__file__).resolve().parents[2]
        / "challenges"
        / "Practical"
        / "AutoSave Text Editor"
        / "aste.py"
    )
    spec = importlib.util.spec_from_file_location("aste_module", module_path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class FakeMaster:
    def __init__(self) -> None:
        self._next_id = 0
        self.after_calls: list[tuple[str, int, Callable[[], None]]] = []
        self.cancelled: list[str] = []

    def after(self, delay_ms: int, func):
        self._next_id += 1
        handle = f"after-{self._next_id}"
        self.after_calls.append((handle, delay_ms, func))
        return handle

    def after_cancel(self, handle: str) -> None:
        self.cancelled.append(handle)


class FakeStatusBar:
    def __init__(self) -> None:
        self.text = ""

    def config(self, *, text: str) -> None:
        self.text = text


def test_set_status_cancels_previous_timeout():
    module = load_aste_module()
    AutosaveTextEditor = module.AutosaveTextEditor

    editor = AutosaveTextEditor.__new__(AutosaveTextEditor)
    editor.master = FakeMaster()
    editor.status_bar = FakeStatusBar()
    editor._status_after_id = None

    editor._set_status("First message")
    first_id = editor._status_after_id

    assert editor.status_bar.text == "First message"
    assert editor.master.after_calls[-1][1] == 5000

    editor._set_status("Second message")
    second_id = editor._status_after_id

    assert editor.status_bar.text == "Second message"
    assert editor.master.cancelled == [first_id]
    assert first_id != second_id

    # Simulate the timeout firing for the second message.
    callbacks = {handle: func for handle, _, func in editor.master.after_calls}
    callbacks[second_id]()

    assert editor.status_bar.text == ""
    assert editor._status_after_id is None
