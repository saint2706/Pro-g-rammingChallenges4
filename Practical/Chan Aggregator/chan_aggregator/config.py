"""Configuration helpers for the Chan Aggregator."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional


@dataclass
class BoardConfig:
    """Describe a board/endpoint combination."""

    board_id: str
    title: str
    catalog_url: str
    thread_url: str
    rate_limit_seconds: float = 2.0
    cache_ttl: int = 120
    archive: Optional[Dict[str, str]] = None
    source: str = ""

    def catalog_endpoint(self) -> str:
        return self.catalog_url.format(board=self.board_id)

    def thread_endpoint(self, thread_id: int) -> str:
        return self.thread_url.format(board=self.board_id, thread=thread_id)


DEFAULT_CONFIG = {
    "sources": [
        {
            "name": "4chan",
            "boards": [
                {
                    "board_id": "g",
                    "title": "Technology",
                    "catalog_url": "https://a.4cdn.org/{board}/catalog.json",
                    "thread_url": "https://a.4cdn.org/{board}/thread/{thread}.json",
                },
                {
                    "board_id": "his",
                    "title": "History & Humanities",
                    "catalog_url": "https://a.4cdn.org/{board}/catalog.json",
                    "thread_url": "https://a.4cdn.org/{board}/thread/{thread}.json",
                },
                {
                    "board_id": "co",
                    "title": "Comics & Cartoons",
                    "catalog_url": "https://a.4cdn.org/{board}/catalog.json",
                    "thread_url": "https://a.4cdn.org/{board}/thread/{thread}.json",
                },
            ],
        },
        {
            "name": "4plebs",
            "boards": [
                {
                    "board_id": "g",
                    "title": "Technology (Archive)",
                    "catalog_url": "https://archive.4plebs.org/{board}/catalog.json",
                    "thread_url": "https://archive.4plebs.org/_/api/chan/thread/?board={board}&num={thread}",
                    "rate_limit_seconds": 4,
                    "cache_ttl": 300,
                }
            ],
        },
    ]
}


def _normalise_board(entry: Dict[str, object], source_name: str) -> BoardConfig:
    return BoardConfig(
        board_id=str(entry["board_id"]),
        title=str(entry.get("title", entry["board_id"])),
        catalog_url=str(entry["catalog_url"]),
        thread_url=str(entry["thread_url"]),
        rate_limit_seconds=float(entry.get("rate_limit_seconds", 2.0)),
        cache_ttl=int(entry.get("cache_ttl", 120)),
        archive=entry.get("archive"),
        source=source_name,
    )


def load_board_configs(paths: Optional[Iterable[Path]] = None) -> List[BoardConfig]:
    """Load board configurations from JSON files.

    Args:
        paths: Optional iterable of file paths. If omitted or empty, fall back to
            a default preset defined in :data:`DEFAULT_CONFIG`.
    """

    configs: List[BoardConfig] = []
    if paths:
        for path in paths:
            if not path:
                continue
            if not path.exists():
                continue
            data = json.loads(path.read_text(encoding="utf-8"))
            configs.extend(_extract_from_payload(data))
    if not configs:
        configs.extend(_extract_from_payload(DEFAULT_CONFIG))
    return configs


def _extract_from_payload(payload: Dict[str, object]) -> List[BoardConfig]:
    configs: List[BoardConfig] = []
    for source in payload.get("sources", []):
        source_name = str(source.get("name", ""))
        for board_entry in source.get("boards", []):
            configs.append(_normalise_board(board_entry, source_name))
    return configs


__all__ = ["BoardConfig", "load_board_configs", "DEFAULT_CONFIG"]
