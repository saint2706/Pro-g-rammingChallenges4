"""HTTP tracker communication helpers."""
from __future__ import annotations

import ipaddress
import logging
import random
import socket
from dataclasses import dataclass
from typing import List, Optional
from urllib.parse import urlencode, urlparse

import requests

LOGGER = logging.getLogger(__name__)


@dataclass
class Peer:
    """A peer advertisement returned by a tracker."""

    host: str
    port: int

    def __str__(self) -> str:  # pragma: no cover - convenience only
        return f"{self.host}:{self.port}"


@dataclass
class TrackerResponse:
    """Structured tracker response."""

    interval: int
    peers: List[Peer]
    warning_message: Optional[str] = None


class TrackerClient:
    """Talk to a BitTorrent tracker over HTTP/HTTPS."""

    def __init__(self, announce_url: str, peer_id: Optional[bytes] = None) -> None:
        self.announce_url = announce_url
        self.peer_id = peer_id or self._generate_peer_id()

    @staticmethod
    def _generate_peer_id() -> bytes:
        prefix = b"PC4-"  # Pro-g-rammingChallenges4 signature
        suffix = bytes(random.randint(0, 255) for _ in range(20 - len(prefix)))
        return prefix + suffix

    def announce(
        self,
        info_hash: bytes,
        port: int,
        downloaded: int,
        left: int,
        uploaded: int = 0,
        event: Optional[str] = None,
        numwant: int = 30,
        timeout: int = 15,
        headers: Optional[dict] = None,
    ) -> TrackerResponse:
        """Perform a tracker announce request.

        Parameters mirror the BitTorrent specification.
        """

        params = {
            "info_hash": info_hash,
            "peer_id": self.peer_id,
            "port": port,
            "uploaded": uploaded,
            "downloaded": downloaded,
            "left": left,
            "compact": 1,
            "numwant": numwant,
        }
        if event:
            params["event"] = event

        parsed = urlparse(self.announce_url)
        if parsed.scheme not in {"http", "https"}:
            raise ValueError("Only HTTP/HTTPS trackers are supported")

        LOGGER.debug("Announcing to %s", self.announce_url)
        response = requests.get(
            self.announce_url,
            params=params,
            timeout=timeout,
            headers=headers,
        )
        response.raise_for_status()
        payload = response.content
        data, _ = bdecode(payload)
        if not isinstance(data, dict):
            raise ValueError("Tracker response must be a dictionary")

        failure = data.get(b"failure reason")
        if isinstance(failure, bytes):
            raise RuntimeError(f"Tracker failure: {failure.decode('utf-8', 'ignore')}")

        warning = data.get(b"warning message")
        interval = data.get(b"interval", 1800)
        peers_blob = data.get(b"peers")
        peers = _parse_peers(peers_blob)
        warning_message = warning.decode("utf-8", "ignore") if isinstance(warning, bytes) else None
        return TrackerResponse(interval=int(interval), peers=peers, warning_message=warning_message)


def _parse_peers(peers_blob: Optional[bytes]) -> List[Peer]:
    if not peers_blob:
        return []
    if isinstance(peers_blob, list):  # dictionary model
        peers: List[Peer] = []
        for entry in peers_blob:
            if not isinstance(entry, dict):
                continue
            ip = entry.get(b"ip")
            port = entry.get(b"port")
            if isinstance(ip, bytes) and isinstance(port, int):
                peers.append(Peer(ip.decode("utf-8", "ignore"), port))
        return peers
    if not isinstance(peers_blob, (bytes, bytearray)):
        raise ValueError("Unsupported peer list format")
    if len(peers_blob) % 6:
        raise ValueError("Invalid compact peer list length")
    peers = []
    for i in range(0, len(peers_blob), 6):
        ip_bytes = peers_blob[i : i + 4]
        port_bytes = peers_blob[i + 4 : i + 6]
        ip_addr = socket.inet_ntoa(ip_bytes)
        port = int.from_bytes(port_bytes, "big")
        # filter private/invalid addresses for nicer logs
        try:
            ipaddress.ip_address(ip_addr)
        except ValueError:  # pragma: no cover - defensive
            continue
        peers.append(Peer(ip_addr, port))
    return peers


# Late import to avoid cycle
from .metainfo import bdecode  # noqa: E402

__all__ = ["TrackerClient", "TrackerResponse", "Peer"]
