from __future__ import annotations

import json
from dataclasses import dataclass
from typing import Any, Dict, Iterable

COMMAND_PREFIX = "/"


class ProtocolError(RuntimeError):
    """Raised when an incoming payload is malformed."""


@dataclass
class Broadcast:
    room: str
    user: str
    text: str
    seq: int
    type: str = "message"

    def to_payload(self) -> Dict[str, Any]:
        return {
            "type": self.type,
            "room": self.room,
            "user": self.user,
            "text": self.text,
            "seq": self.seq,
        }


SYSTEM_USER = "system"


def encode_json(payload: Dict[str, Any]) -> bytes:
    return (json.dumps(payload, ensure_ascii=False) + "\n").encode("utf-8")


def decode_json(line: bytes) -> Dict[str, Any]:
    try:
        data = json.loads(line.decode("utf-8"))
    except json.JSONDecodeError as exc:  # pragma: no cover - clarity
        raise ProtocolError(f"Invalid JSON: {exc}") from exc
    if not isinstance(data, dict):
        raise ProtocolError("Payload must be a JSON object")
    return data


def ensure_fields(data: Dict[str, Any], required: Iterable[str]) -> None:
    missing = [field for field in required if field not in data]
    if missing:
        raise ProtocolError(f"Missing fields: {', '.join(missing)}")


def system_payload(message: str, room: str, seq: int) -> Dict[str, Any]:
    return Broadcast(
        room=room, user=SYSTEM_USER, text=message, seq=seq, type="system"
    ).to_payload()


def ack_payload(message_id: int) -> Dict[str, Any]:
    return {"type": "ack", "id": message_id}
