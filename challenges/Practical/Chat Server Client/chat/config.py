from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Type, TypeVar

try:  # Python 3.11+
    import tomllib  # type: ignore
except ModuleNotFoundError:  # pragma: no cover - fallback for 3.10
    import tomli as tomllib  # type: ignore

import json


@dataclass
class TcpServerConfig:
    host: str = "127.0.0.1"
    port: int = 8765
    log_path: Path = Path("challenges/Practical/Chat Server Client/logs/tcp-chat.log")
    history_size: int = 100
    max_connections: int = 128


@dataclass
class TcpClientConfig:
    host: str = "127.0.0.1"
    port: int = 8765
    room: str = "lobby"
    log_path: Path = Path("challenges/Practical/Chat Server Client/logs/tcp-client.log")
    reconnect_delay: float = 3.0


@dataclass
class UdpConfig:
    host: str = "127.0.0.1"
    port: int = 8766
    log_path: Path = Path("challenges/Practical/Chat Server Client/logs/udp-chat.log")
    base_delay: float = 0.5
    max_retries: int = 5
    history_size: int = 50


T = TypeVar("T")


def _load_raw_config(path: Path) -> Dict[str, Any]:
    if not path.exists():
        raise FileNotFoundError(path)
    text = path.read_text(encoding="utf-8")
    if path.suffix in {".toml", ".tml"}:
        return tomllib.loads(text)
    if path.suffix in {".json"}:
        return json.loads(text)
    raise ValueError(f"Unsupported config format for {path}")


def load_config(path: str | Path, schema: Type[T]) -> T:
    """Load a configuration dataclass from TOML or JSON."""

    raw = _load_raw_config(Path(path))
    data = {
        field: raw.get(field, getattr(schema(), field))
        for field in schema.__dataclass_fields__
    }
    instance = schema(**data)
    _normalize_paths(instance)
    return instance


def _normalize_paths(config_obj: Any) -> None:
    for field in getattr(config_obj, "__dataclass_fields__", {}):
        value = getattr(config_obj, field)
        if isinstance(value, (str, Path)) and "path" in field:
            setattr(config_obj, field, Path(value).expanduser())


__all__ = [
    "TcpServerConfig",
    "TcpClientConfig",
    "UdpConfig",
    "load_config",
]
