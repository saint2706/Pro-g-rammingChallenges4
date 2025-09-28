from __future__ import annotations

import asyncio
import contextlib
import logging
import sys
from pathlib import Path
from typing import Optional

from .config import TcpClientConfig
from .logging_utils import setup_rotating_log
from . import protocols


class TcpChatClient:
    def __init__(self, config: TcpClientConfig, user: str):
        self.config = config
        self.user = user
        self.room = config.room
        self.logger = setup_rotating_log(Path(config.log_path))
        self._writer: Optional[asyncio.StreamWriter] = None
        self._reader: Optional[asyncio.StreamReader] = None

    async def connect(self) -> None:
        while True:
            try:
                self._reader, self._writer = await asyncio.open_connection(
                    self.config.host, self.config.port
                )
                break
            except OSError as exc:
                logging.getLogger(__name__).warning("Connection failed: %s", exc)
                await asyncio.sleep(self.config.reconnect_delay)
        await self._send_join(self.room)

    async def run(self) -> None:
        await self.connect()
        assert self._reader and self._writer
        loop = asyncio.get_running_loop()
        receiver = asyncio.create_task(self._receive_loop())
        sender = asyncio.create_task(self._stdin_loop(loop))
        await asyncio.wait({receiver, sender}, return_when=asyncio.FIRST_COMPLETED)
        receiver.cancel()
        sender.cancel()
        await asyncio.gather(receiver, sender, return_exceptions=True)
        await self.close()

    async def _stdin_loop(self, loop: asyncio.AbstractEventLoop) -> None:
        print("Type /help for commands.")
        while True:
            line = await loop.run_in_executor(None, sys.stdin.readline)
            if not line:
                return
            line = line.rstrip("\n")
            if not line:
                continue
            if line.startswith(protocols.COMMAND_PREFIX):
                await self._handle_command(line)
            else:
                await self._send_message(line)

    async def _handle_command(self, line: str) -> None:
        parts = line.split()
        command = parts[0][1:].lower()
        if command == "help":
            print("Commands: /join ROOM, /rooms, /quit")
        elif command == "join" and len(parts) >= 2:
            await self._send_join(parts[1])
        elif command == "quit":
            await self.close()
            raise asyncio.CancelledError
        elif command == "rooms":
            await self._send({"type": "rooms"})
        else:
            print(f"Unknown command: {line}")

    async def _receive_loop(self) -> None:
        assert self._reader
        while not self._reader.at_eof():
            raw = await self._reader.readline()
            if not raw:
                break
            try:
                payload = protocols.decode_json(raw)
            except protocols.ProtocolError as exc:
                logging.getLogger(__name__).error("Protocol error: %s", exc)
                continue
            self._display(payload)
        print("Disconnected from server")

    def _display(self, payload: dict) -> None:
        msg_type = payload.get("type")
        if msg_type in {"message", "system"}:
            room = payload.get("room", "?")
            user = payload.get("user", "?")
            text = payload.get("text", "")
            line = f"[{room}] {user}: {text}"
            print(line)
            self.logger.info(line)
        elif msg_type == "error":
            print(f"! {payload.get('text')}")
        elif msg_type == "rooms":
            rooms = ", ".join(payload.get("rooms", [])) or "(none)"
            print(f"Active rooms: {rooms}")

    async def _send(self, payload: dict) -> None:
        assert self._writer
        self._writer.write(protocols.encode_json(payload))
        await self._writer.drain()

    async def _send_join(self, room: str) -> None:
        self.room = room
        await self._send({"type": "join", "room": room, "user": self.user})

    async def _send_message(self, text: str) -> None:
        await self._send({"type": "message", "text": text, "room": self.room})

    async def close(self) -> None:
        if self._writer:
            try:
                await self._send({"type": "leave"})
            except Exception:
                pass
            self._writer.close()
            with contextlib.suppress(Exception):
                await self._writer.wait_closed()


async def main(config_path: str, user: str) -> None:
    config = load_client_config(config_path)
    client = TcpChatClient(config, user)
    await client.run()


def load_client_config(path: str) -> TcpClientConfig:
    from .config import load_config

    return load_config(path, TcpClientConfig)


__all__ = ["TcpChatClient", "main", "load_client_config"]
