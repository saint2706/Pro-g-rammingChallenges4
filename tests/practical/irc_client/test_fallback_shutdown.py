from __future__ import annotations

import asyncio
import importlib.util
import logging
import sys
import threading
from pathlib import Path
from types import ModuleType


def load_module() -> ModuleType:
    module_path = (
        Path(__file__).resolve().parents[3]
        / "challenges"
        / "Practical"
        / "IRC Client"
        / "irc_client.py"
    )
    spec = importlib.util.spec_from_file_location("irc_client_module", module_path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class BlockingStdin:
    def __init__(self) -> None:
        self._event = threading.Event()
        self._value: str = ""

    def readline(self) -> str:
        self._event.wait()
        return self._value

    def release(self, value: str = "") -> None:
        self._value = value
        self._event.set()


def test_fallback_loop_exits_promptly(monkeypatch):
    module = load_module()
    IRCConfig = module.IRCConfig
    IRCClient = module.IRCClient

    config = IRCConfig(
        server="example.com",
        port=6667,
        nickname="tester",
        username="tester",
        realname="Tester",
        password=None,
        channels=[],
    )
    client = IRCClient(config, logging.getLogger("irc-test"))

    blocking_stdin = BlockingStdin()

    monkeypatch.setattr(module.sys, "stdin", blocking_stdin)

    async def runner() -> None:
        client._stdin_fallback = True
        task = asyncio.create_task(client._fallback_input_loop())
        await asyncio.sleep(0)
        await client.quit("bye")
        await asyncio.wait_for(task, timeout=0.5)

    try:
        asyncio.run(runner())
    finally:
        blocking_stdin.release()

    assert client.stop_requested.is_set()
