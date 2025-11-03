from __future__ import annotations

import importlib.util
import sys
import threading
from pathlib import Path


def load_pc_module():
    """Load the producer-consumer challenge module for testing."""

    module_name = "producer_consumer_pc"
    module_path = (
        Path(__file__).resolve().parents[3]
        / "challenges"
        / "Practical"
        / "Producer Consumer"
        / "pc.py"
    )
    spec = importlib.util.spec_from_file_location(module_name, module_path)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[module_name] = module
    spec.loader.exec_module(module)
    return module


def test_keyboard_interrupt_shuts_down_threads(monkeypatch):
    pc = load_pc_module()
    pc.STOP_EVENT.clear()

    cfg = pc.Config(
        capacity=2,
        items_per_producer=10,
        producers=1,
        consumers=1,
        prod_delay=0.0,
        cons_delay=0.0,
        quiet=True,
        timestamps=False,
    )

    monkeypatch.setattr(pc, "parse_args", lambda argv=None: cfg)

    created_threads: list[threading.Thread] = []
    join_calls = {"count": 0}
    original_thread_cls = threading.Thread

    class RecordingThread(original_thread_cls):
        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            created_threads.append(self)

        def join(self, timeout=None):  # type: ignore[override]
            join_calls["count"] += 1
            if join_calls["count"] == 1:
                raise KeyboardInterrupt
            return original_thread_cls.join(self, timeout)

    monkeypatch.setattr(threading, "Thread", RecordingThread)
    monkeypatch.setattr(pc.threading, "Thread", RecordingThread)

    try:
        result = pc.main([])
    finally:
        # Ensure we give spawned threads a chance to terminate during cleanup.
        for thread in created_threads:
            if thread.is_alive():
                original_thread_cls.join(thread)
        sys.modules.pop("producer_consumer_pc", None)

    assert result == 130
    assert join_calls["count"] >= 1
    assert all(not thread.is_alive() for thread in created_threads)
    assert not pc.STOP_EVENT.is_set()
