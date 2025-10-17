from __future__ import annotations

import asyncio
import logging
from collections import defaultdict, deque
from pathlib import Path
from typing import Deque, Dict, Optional, Set, Tuple

from .config import UdpConfig
from .logging_utils import setup_rotating_log
from . import protocols

Address = Tuple[str, int]


class UdpChatProtocol(asyncio.DatagramProtocol):
    def __init__(self, config: UdpConfig):
        self.config = config
        self.transport: Optional[asyncio.DatagramTransport] = None
        self.logger = setup_rotating_log(Path(config.log_path))
        self.loop = asyncio.get_running_loop()
        self.sequence = 0
        self.rooms: Dict[str, Set[Address]] = defaultdict(set)
        self.peers: Dict[Address, Dict[str, str]] = {}
        self.history: Dict[str, Deque[dict]] = defaultdict(
            lambda: deque(maxlen=config.history_size)
        )

    def connection_made(
        self, transport: asyncio.BaseTransport
    ) -> None:  # pragma: no cover - callback
        self.transport = transport  # type: ignore[assignment]
        sock = transport.get_extra_info("sockname")
        logging.getLogger(__name__).info("UDP chat listening on %s", sock)

    def datagram_received(
        self, data: bytes, addr: Address
    ) -> None:  # pragma: no cover - callback
        try:
            payload = protocols.decode_json(data)
        except protocols.ProtocolError as exc:
            self._send_error(addr, str(exc))
            return
        msg_type = payload.get("type")
        if msg_type == "join":
            self._handle_join(addr, payload)
        elif msg_type == "message":
            self._handle_message(addr, payload)
        elif msg_type == "leave":
            self._handle_leave(addr, payload)
        else:
            self._send_error(addr, f"Unknown message type: {msg_type}")

    def connection_lost(
        self, exc: Optional[Exception]
    ) -> None:  # pragma: no cover - callback
        if exc:
            logging.getLogger(__name__).error("UDP server error: %s", exc)

    def _handle_join(self, addr: Address, payload: dict) -> None:
        protocols.ensure_fields(payload, ["room", "user", "id"])
        room = payload["room"]
        user = payload["user"]
        message_id = payload["id"]
        ack = protocols.ack_payload(message_id)
        self._send(addr, ack)

        info = self.peers.get(addr)
        if info and info.get("room") == room and info.get("user") == user:
            return
        if info and info.get("room"):
            self._broadcast(
                protocols.system_payload(
                    f"{info['user']} left", info["room"], self._next_seq()
                )
            )
            self.rooms[info["room"]].discard(addr)
        self.peers[addr] = {"room": room, "user": user}
        self.rooms[room].add(addr)
        self._send_history(addr, room)
        self._broadcast(
            protocols.system_payload(f"{user} joined", room, self._next_seq())
        )

    def _handle_message(self, addr: Address, payload: dict) -> None:
        protocols.ensure_fields(payload, ["text", "id"])
        info = self.peers.get(addr)
        if not info:
            self._send_error(addr, "Join first")
            return
        text = payload["text"]
        message_id = payload["id"]
        self._send(addr, protocols.ack_payload(message_id))
        broadcast = protocols.Broadcast(
            room=info["room"],
            user=info["user"],
            text=text,
            seq=self._next_seq(),
        ).to_payload()
        self._broadcast(broadcast)

    def _handle_leave(self, addr: Address, payload: dict) -> None:
        message_id = payload.get("id")
        if message_id is not None:
            self._send(addr, protocols.ack_payload(message_id))
        info = self.peers.pop(addr, None)
        if info:
            room = info.get("room")
            if room:
                self.rooms[room].discard(addr)
                self._broadcast(
                    protocols.system_payload(
                        f"{info['user']} left", room, self._next_seq()
                    )
                )

    def _broadcast(self, payload: dict) -> None:
        room = payload.get("room", "system")
        self.history[room].append(payload)
        self.logger.info("%s %s: %s", room, payload.get("user"), payload.get("text"))
        for addr in list(self.rooms.get(room, set())):
            self._send(addr, payload)

    def _send_history(self, addr: Address, room: str) -> None:
        for payload in self.history.get(room, []):
            self._send(addr, payload)

    def _send(self, addr: Address, payload: dict) -> None:
        if not self.transport:
            return
        self.transport.sendto(protocols.encode_json(payload), addr)

    def _send_error(self, addr: Address, message: str) -> None:
        self._send(addr, {"type": "error", "text": message, "seq": self.sequence})

    def _next_seq(self) -> int:
        self.sequence += 1
        return self.sequence


async def serve(config: UdpConfig) -> None:
    loop = asyncio.get_running_loop()
    transport, _ = await loop.create_datagram_endpoint(
        lambda: UdpChatProtocol(config), local_addr=(config.host, config.port)
    )
    try:
        await asyncio.Event().wait()
    finally:
        transport.close()


async def main(config_path: str) -> None:
    config = load_udp_config(config_path)
    await serve(config)


def load_udp_config(path: str) -> UdpConfig:
    from .config import load_config

    return load_config(path, UdpConfig)


__all__ = ["serve", "UdpChatProtocol", "load_udp_config"]
