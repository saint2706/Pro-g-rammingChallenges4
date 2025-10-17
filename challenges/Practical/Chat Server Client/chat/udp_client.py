from __future__ import annotations

import asyncio
import contextlib
import logging
import sys
from pathlib import Path
from typing import Dict, Optional

from .config import UdpConfig
from .logging_utils import setup_rotating_log
from . import protocols


class _AckProtocol(asyncio.DatagramProtocol):
    def __init__(self) -> None:
        self.transport: Optional[asyncio.DatagramTransport] = None
        self.loop = asyncio.get_running_loop()
        self.ack_waiters: Dict[int, asyncio.Future[None]] = {}
        self.incoming: asyncio.Queue[dict] = asyncio.Queue()

    def connection_made(
        self, transport: asyncio.BaseTransport
    ) -> None:  # pragma: no cover
        self.transport = transport  # type: ignore[assignment]

    def datagram_received(self, data: bytes, addr) -> None:  # pragma: no cover
        try:
            payload = protocols.decode_json(data)
        except protocols.ProtocolError as exc:
            logging.getLogger(__name__).error("UDP client decode error: %s", exc)
            return
        if payload.get("type") == "ack":
            message_id = payload.get("id")
            future = self.ack_waiters.pop(message_id, None)
            if future and not future.done():
                future.set_result(None)
        else:
            self.incoming.put_nowait(payload)

    def error_received(self, exc: Exception) -> None:  # pragma: no cover
        logging.getLogger(__name__).error("UDP client transport error: %s", exc)

    def connection_lost(self, exc: Optional[Exception]) -> None:  # pragma: no cover
        if exc:
            logging.getLogger(__name__).error("UDP client connection lost: %s", exc)

    def send(self, payload: dict) -> None:
        if not self.transport:
            raise RuntimeError("Transport not ready")
        self.transport.sendto(protocols.encode_json(payload))


class UdpChatClient:
    def __init__(self, config: UdpConfig, user: str):
        self.config = config
        self.user = user
        self.room = "lobby"
        self.logger = setup_rotating_log(Path(config.log_path))
        self.protocol = _AckProtocol()
        self._message_id = 0

    def _next_id(self) -> int:
        self._message_id += 1
        return self._message_id

    async def run(self) -> None:
        loop = asyncio.get_running_loop()
        transport, _ = await loop.create_datagram_endpoint(
            lambda: self.protocol,
            remote_addr=(self.config.host, self.config.port),
        )
        try:
            await self._send_join(self.room)
            receiver = asyncio.create_task(self._receive_loop())
            stdin = asyncio.create_task(self._stdin_loop(loop))
            await asyncio.wait({receiver, stdin}, return_when=asyncio.FIRST_COMPLETED)
            receiver.cancel()
            stdin.cancel()
            await asyncio.gather(receiver, stdin, return_exceptions=True)
        finally:
            with contextlib.suppress(Exception):
                await self._send_leave()
            transport.close()

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
            print("Commands: /join ROOM, /quit")
        elif command == "join" and len(parts) >= 2:
            await self._send_join(parts[1])
        elif command == "quit":
            await self._send_leave()
            raise asyncio.CancelledError
        else:
            print(f"Unknown command: {line}")

    async def _receive_loop(self) -> None:
        while True:
            payload = await self.protocol.incoming.get()
            self._display(payload)

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

    async def _send_with_retry(self, payload: dict) -> None:
        message_id = payload.get("id")
        if message_id is None:
            self.protocol.send(payload)
            return
        delay = self.config.base_delay
        future = asyncio.get_running_loop().create_future()
        self.protocol.ack_waiters[message_id] = future
        for attempt in range(self.config.max_retries + 1):
            self.protocol.send(payload)
            try:
                await asyncio.wait_for(future, timeout=delay)
                return
            except asyncio.TimeoutError:
                delay *= 2
        if not future.done():
            future.cancel()
            print("! Message delivery uncertain (no ACK)")
        self.protocol.ack_waiters.pop(message_id, None)

    async def _send_join(self, room: str) -> None:
        self.room = room
        payload = {
            "type": "join",
            "room": room,
            "user": self.user,
            "id": self._next_id(),
        }
        await self._send_with_retry(payload)

    async def _send_message(self, text: str) -> None:
        payload = {"type": "message", "text": text, "id": self._next_id()}
        await self._send_with_retry(payload)

    async def _send_leave(self) -> None:
        payload = {"type": "leave", "id": self._next_id()}
        await self._send_with_retry(payload)


async def main(config_path: str, user: str) -> None:
    config = load_udp_config(config_path)
    client = UdpChatClient(config, user)
    await client.run()


def load_udp_config(path: str) -> UdpConfig:
    from .config import load_config

    return load_config(path, UdpConfig)


__all__ = ["UdpChatClient", "main", "load_udp_config"]
