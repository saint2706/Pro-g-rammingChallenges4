"""Peer wire protocol primitives (handshake, messages, piece manager)."""

from __future__ import annotations

import logging
import socket
import struct
import time
from dataclasses import dataclass, field
from typing import Dict, Iterable, List, Optional, Tuple, TYPE_CHECKING

if TYPE_CHECKING:  # pragma: no cover
    from .storage import FileStorage

LOGGER = logging.getLogger(__name__)

PROTOCOL_STRING = b"BitTorrent protocol"
HANDSHAKE_RESERVED = bytes(8)

# Message identifiers
CHOKE = 0
UNCHOKE = 1
INTERESTED = 2
NOT_INTERESTED = 3
HAVE = 4
BITFIELD = 5
REQUEST = 6
PIECE = 7
CANCEL = 8
PORT = 9


def build_handshake(info_hash: bytes, peer_id: bytes) -> bytes:
    if len(info_hash) != 20 or len(peer_id) != 20:
        raise ValueError("info_hash and peer_id must be 20 bytes")
    pstrlen = len(PROTOCOL_STRING)
    return (
        struct.pack("!B", pstrlen)
        + PROTOCOL_STRING
        + HANDSHAKE_RESERVED
        + info_hash
        + peer_id
    )


def parse_handshake(payload: bytes) -> Tuple[bytes, bytes]:
    if len(payload) < 68:
        raise ValueError("Handshake payload too short")
    pstrlen = payload[0]
    expected = 1 + pstrlen + 8 + 20 + 20
    if len(payload) < expected:
        raise ValueError("Incomplete handshake payload")
    protocol = payload[1 : 1 + pstrlen]
    if protocol != PROTOCOL_STRING:
        raise ValueError("Unexpected protocol string")
    info_hash = payload[1 + pstrlen + 8 : 1 + pstrlen + 8 + 20]
    peer_id = payload[1 + pstrlen + 8 + 20 : expected]
    return info_hash, peer_id


@dataclass
class PieceWork:
    index: int
    begin: int
    length: int


@dataclass
class PieceManager:
    """Track which pieces are needed and which are in-flight."""

    total_pieces: int
    piece_length: int
    total_size: int
    completed: List[bool] = field(default_factory=list)
    in_progress: Dict[int, float] = field(default_factory=dict)

    def __post_init__(self) -> None:
        if not self.completed:
            self.completed = [False] * self.total_pieces

    @classmethod
    def from_storage(
        cls, piece_hashes: Iterable[bytes], piece_length: int, storage: "FileStorage"
    ) -> "PieceManager":
        pieces = list(piece_hashes)
        completed = [
            storage.has_piece(i, piece_hash) for i, piece_hash in enumerate(pieces)
        ]
        total_size = storage.total_size
        return cls(
            total_pieces=len(pieces),
            piece_length=piece_length,
            total_size=total_size,
            completed=completed,
        )

    def next_request(self) -> Optional[PieceWork]:
        for index, done in enumerate(self.completed):
            if done or index in self.in_progress:
                continue
            self.in_progress[index] = time.monotonic()
            begin = 0
            length = self._piece_length(index)
            return PieceWork(index=index, begin=begin, length=length)
        return None

    def _piece_length(self, index: int) -> int:
        if index < self.total_pieces - 1:
            return self.piece_length
        return self.total_size - (self.total_pieces - 1) * self.piece_length

    def mark_complete(self, index: int) -> None:
        self.completed[index] = True
        self.in_progress.pop(index, None)

    def mark_failed(self, index: int) -> None:
        self.in_progress.pop(index, None)

    def finished(self) -> bool:
        return all(self.completed)

    @property
    def completed_count(self) -> int:
        return sum(self.completed)


class PeerConnection:
    """Blocking TCP connection to a peer that can request pieces sequentially."""

    def __init__(
        self,
        host: str,
        port: int,
        info_hash: bytes,
        peer_id: bytes,
        timeout: float = 10.0,
    ) -> None:
        self.host = host
        self.port = port
        self.info_hash = info_hash
        self.peer_id = peer_id
        self.timeout = timeout
        self.socket: Optional[socket.socket] = None
        self.choked = True

    def __enter__(self) -> "PeerConnection":  # pragma: no cover - used in CLI
        self.connect()
        return self

    def __exit__(self, exc_type, exc, tb) -> None:  # pragma: no cover - used in CLI
        self.close()

    def connect(self) -> None:
        LOGGER.debug("Connecting to %s:%s", self.host, self.port)
        sock = socket.create_connection((self.host, self.port), timeout=self.timeout)
        sock.settimeout(self.timeout)
        handshake = build_handshake(self.info_hash, self.peer_id)
        sock.sendall(handshake)
        response = self._recv_exact(sock, 68)
        peer_info_hash, _ = parse_handshake(response)
        if peer_info_hash != self.info_hash:
            sock.close()
            raise RuntimeError("Peer responded with mismatched info hash")
        self.socket = sock
        self._send_message(INTERESTED)
        LOGGER.debug("Handshake complete with %s:%s", self.host, self.port)

    def _recv_exact(self, sock: socket.socket, size: int) -> bytes:
        chunks = []
        remaining = size
        while remaining > 0:
            chunk = sock.recv(remaining)
            if not chunk:
                raise ConnectionError("Connection closed while receiving data")
            chunks.append(chunk)
            remaining -= len(chunk)
        return b"".join(chunks)

    def close(self) -> None:
        if self.socket:
            try:
                self.socket.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            self.socket.close()
            self.socket = None

    def _send_message(self, message_id: Optional[int], payload: bytes = b"") -> None:
        if not self.socket:
            raise RuntimeError("Not connected")
        if message_id is None:
            length_prefix = struct.pack("!I", 0)
            self.socket.sendall(length_prefix)
            return
        length = len(payload) + 1
        packet = struct.pack("!IB", length, message_id) + payload
        self.socket.sendall(packet)

    def _read_message(self) -> Tuple[int, bytes]:
        if not self.socket:
            raise RuntimeError("Not connected")
        length_prefix = self._recv_exact(self.socket, 4)
        (length,) = struct.unpack("!I", length_prefix)
        if length == 0:
            return -1, b""
        message_id_data = self._recv_exact(self.socket, 1)
        message_id = message_id_data[0]
        payload = self._recv_exact(self.socket, length - 1) if length > 1 else b""
        return message_id, payload

    def _wait_for_unchoke(self, deadline: float) -> None:
        while time.monotonic() < deadline:
            message_id, payload = self._read_message()
            if message_id == CHOKE:
                self.choked = True
            elif message_id == UNCHOKE:
                self.choked = False
                return
            elif message_id == BITFIELD:
                # ignore for now
                continue
            elif message_id == HAVE:
                continue
            elif message_id == -1:
                continue
            elif message_id == PORT:
                continue
            elif message_id == PIECE:
                # unexpected but consume
                continue
        raise TimeoutError("Timed out waiting for unchoke")

    def download_piece(self, piece: PieceWork) -> bytes:
        if not self.socket:
            raise RuntimeError("Not connected")
        if self.choked:
            self._wait_for_unchoke(time.monotonic() + self.timeout)
        request_payload = struct.pack("!III", piece.index, piece.begin, piece.length)
        self._send_message(REQUEST, request_payload)
        while True:
            message_id, payload = self._read_message()
            if message_id == PIECE:
                index, begin = struct.unpack("!II", payload[:8])
                if index != piece.index or begin != piece.begin:
                    continue
                return payload[8:]
            if message_id == CHOKE:
                self.choked = True
                self._wait_for_unchoke(time.monotonic() + self.timeout)
            elif message_id in {HAVE, BITFIELD, UNCHOKE, PORT, -1}:
                if message_id == UNCHOKE:
                    self.choked = False
                continue
            else:
                LOGGER.debug("Ignoring message %s", message_id)


__all__ = [
    "PeerConnection",
    "PieceManager",
    "PieceWork",
    "build_handshake",
    "parse_handshake",
]
