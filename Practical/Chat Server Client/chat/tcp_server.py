from __future__ import annotations

import asyncio
import contextlib
import logging
from collections import defaultdict, deque
from dataclasses import dataclass, field
from pathlib import Path
from typing import Deque, Dict, Optional, Set, Tuple

from .config import TcpServerConfig
from .logging_utils import setup_rotating_log
from . import protocols


@dataclass(eq=False)
class ClientSession:
    reader: asyncio.StreamReader
    writer: asyncio.StreamWriter
    address: Tuple[str, int]
    user: str = field(default_factory=str)
    room: Optional[str] = None

    def transport_id(self) -> str:
        return f"{self.address[0]}:{self.address[1]}"

    def __hash__(self) -> int:  # allow storage in sets
        return hash(id(self))


class ChatRoomServer:
    def __init__(self, config: TcpServerConfig):
        self.config = config
        self.rooms: Dict[str, Set[ClientSession]] = defaultdict(set)
        self.clients: Set[ClientSession] = set()
        self.logger = setup_rotating_log(Path(config.log_path))
        self.sequence = 0
        self.history: Dict[str, Deque[dict]] = defaultdict(lambda: deque(maxlen=config.history_size))
        self._lock = asyncio.Lock()

    async def handle_client(self, reader: asyncio.StreamReader, writer: asyncio.StreamWriter) -> None:
        address = writer.get_extra_info("peername")
        session = ClientSession(reader=reader, writer=writer, address=address)
        if len(self.clients) >= self.config.max_connections:
            await self._send_payload(session, {
                "type": "error",
                "text": "Server busy, try again later",
                "seq": self.sequence,
            })
            writer.close()
            await writer.wait_closed()
            return

        self.clients.add(session)
        try:
            await self._send_greeting(session)
            while not reader.at_eof():
                raw = await reader.readline()
                if not raw:
                    break
                try:
                    payload = protocols.decode_json(raw)
                except protocols.ProtocolError as exc:
                    await self._send_error(session, str(exc))
                    continue
                await self._dispatch(session, payload)
        finally:
            await self._disconnect(session)

    async def _send_greeting(self, session: ClientSession) -> None:
        message = (
            "Welcome! Send {\"type\": \"join\", \"room\": \"lobby\", \"user\": \"alice\"} to start."
        )
        await self._send_payload(session, {
            "type": "system",
            "text": message,
            "room": "system",
            "seq": self.sequence,
        })

    async def _dispatch(self, session: ClientSession, payload: dict) -> None:
        msg_type = payload.get("type")
        if msg_type == "join":
            await self._handle_join(session, payload)
        elif msg_type == "message":
            await self._handle_message(session, payload)
        elif msg_type == "leave":
            await self._disconnect(session)
        elif msg_type == "rooms":
            await self._handle_rooms_request(session)
        else:
            await self._send_error(session, f"Unknown message type: {msg_type}")

    async def _handle_join(self, session: ClientSession, payload: dict) -> None:
        protocols.ensure_fields(payload, ["room", "user"])
        new_room = payload["room"]
        new_user = payload["user"]
        old_room = session.room
        old_user = session.user
        session.user = new_user
        if old_room == new_room:
            await self._send_error(session, "Already in that room")
            return

        async with self._lock:
            if old_room and session in self.rooms.get(old_room, set()):
                self.rooms[old_room].remove(session)
                await self._broadcast(protocols.system_payload(f"{old_user or session.transport_id()} left", old_room, self._next_seq()))
            session.room = new_room
            self.rooms[new_room].add(session)
            await self._send_recent_history(session, new_room)
            await self._broadcast(protocols.system_payload(f"{session.user} joined", new_room, self._next_seq()))

    async def _handle_message(self, session: ClientSession, payload: dict) -> None:
        if not session.room:
            await self._send_error(session, "Join a room first")
            return
        protocols.ensure_fields(payload, ["text"])
        text = payload["text"]
        broadcast = protocols.Broadcast(
            room=session.room,
            user=session.user or session.transport_id(),
            text=text,
            seq=self._next_seq(),
        ).to_payload()
        await self._broadcast(broadcast)

    async def _handle_rooms_request(self, session: ClientSession) -> None:
        rooms = sorted(self.rooms.keys())
        await self._send_payload(session, {
            "type": "rooms",
            "rooms": rooms,
            "seq": self.sequence,
        })

    async def _disconnect(self, session: ClientSession) -> None:
        if session in self.clients:
            self.clients.remove(session)
        room = session.room
        if room and session in self.rooms.get(room, set()):
            self.rooms[room].remove(session)
            await self._broadcast(protocols.system_payload(f"{session.user or session.transport_id()} left", room, self._next_seq()))
        with contextlib.suppress(Exception):
            session.writer.close()
            await session.writer.wait_closed()

    async def _send_payload(self, session: ClientSession, payload: dict) -> None:
        session.writer.write(protocols.encode_json(payload))
        await session.writer.drain()

    async def _broadcast(self, payload: dict) -> None:
        room = payload.get("room", "system")
        self.history[room].append(payload)
        self.logger.info("%s %s: %s", room, payload.get("user"), payload.get("text"))
        coros = [self._safe_send(client, payload) for client in list(self.rooms.get(room, set()))]
        if coros:
            await asyncio.gather(*coros, return_exceptions=True)

    async def _safe_send(self, session: ClientSession, payload: dict) -> None:
        try:
            await self._send_payload(session, payload)
        except ConnectionResetError:
            await self._disconnect(session)

    def _next_seq(self) -> int:
        self.sequence += 1
        return self.sequence

    async def _send_error(self, session: ClientSession, message: str) -> None:
        await self._send_payload(session, {
            "type": "error",
            "text": message,
            "seq": self.sequence,
        })

    async def _send_recent_history(self, session: ClientSession, room: str) -> None:
        if history := list(self.history.get(room, [])):
            for payload in history:
                await self._send_payload(session, payload)


async def serve(config: TcpServerConfig) -> None:
    server = ChatRoomServer(config)
    server_coroutine = await asyncio.start_server(
        server.handle_client, config.host, config.port
    )
    sockets = ", ".join(str(sock.getsockname()) for sock in server_coroutine.sockets or [])
    logging.getLogger(__name__).info("TCP chat listening on %s", sockets)
    async with server_coroutine:
        await server_coroutine.serve_forever()


async def main(config_path: str) -> None:
    config = load_server_config(config_path)
    await serve(config)


def load_server_config(path: str) -> TcpServerConfig:
    from .config import load_config

    return load_config(path, TcpServerConfig)


__all__ = ["serve", "ChatRoomServer", "load_server_config"]
