"""Core aggregation logic."""

from __future__ import annotations

import html
import re
import threading
import time
from dataclasses import dataclass
from datetime import datetime, timezone
from typing import Dict, Iterable, List, Optional, Sequence

import requests

from .config import BoardConfig


@dataclass
class AggregatedThread:
    board: str
    source: str
    thread_id: int
    title: str
    excerpt: str
    replies: int
    last_modified: datetime

    def as_dict(self) -> Dict[str, object]:
        return {
            "board": self.board,
            "source": self.source,
            "thread_id": self.thread_id,
            "title": self.title,
            "excerpt": self.excerpt,
            "replies": self.replies,
            "last_modified": self.last_modified.isoformat(),
        }


class TTLCache:
    def __init__(self) -> None:
        self._lock = threading.Lock()
        self._values: Dict[str, tuple[float, object, float]] = {}

    def get(self, key: str, now: Optional[float] = None) -> Optional[object]:
        if now is None:
            now = time.monotonic()
        with self._lock:
            entry = self._values.get(key)
            if not entry:
                return None
            timestamp, value, ttl = entry
            if now - timestamp > ttl:
                del self._values[key]
                return None
            return value

    def set(self, key: str, value: object, ttl: float, now: Optional[float] = None) -> None:
        if now is None:
            now = time.monotonic()
        with self._lock:
            self._values[key] = (now, value, ttl)


class RateLimiter:
    def __init__(self, interval: float) -> None:
        self.interval = interval
        self._lock = threading.Lock()
        self._last_call = 0.0

    def wait(self, now: Optional[float] = None) -> None:
        if now is None:
            now = time.monotonic()
        with self._lock:
            elapsed = now - self._last_call
            remaining = self.interval - elapsed
            if remaining > 0:
                time.sleep(remaining)
                now = time.monotonic()
            self._last_call = now


class ChanAggregator:
    """Fetch and merge threads from multiple boards."""

    def __init__(
        self,
        boards: Sequence[BoardConfig],
        session: Optional[requests.Session] = None,
    ) -> None:
        self.boards = list(boards)
        self.session = session or requests.Session()
        self._cache = TTLCache()
        self._rate_limiters: Dict[str, RateLimiter] = {}

    # ------------------------------- Requests helpers ---------------------
    def _get_rate_limiter(self, board: BoardConfig) -> RateLimiter:
        key = f"{board.source}:{board.board_id}" if board.source else board.board_id
        limiter = self._rate_limiters.get(key)
        if not limiter:
            limiter = RateLimiter(board.rate_limit_seconds)
            self._rate_limiters[key] = limiter
        return limiter

    def _request_json(self, url: str, board: BoardConfig) -> object:
        prefix = f"{board.source}:{board.board_id}" if board.source else board.board_id
        cache_key = f"{prefix}:{url}"
        cached = self._cache.get(cache_key)
        if cached is not None:
            return cached
        self._get_rate_limiter(board).wait()
        response = self.session.get(url, timeout=10)
        response.raise_for_status()
        data = response.json()
        ttl = max(1, board.cache_ttl)
        self._cache.set(cache_key, data, ttl)
        return data

    # ------------------------------- Public API ---------------------------
    def fetch_catalog(self, board: BoardConfig) -> List[Dict[str, object]]:
        data = self._request_json(board.catalog_endpoint(), board)
        if isinstance(data, list):
            return data
        if isinstance(data, dict) and "threads" in data:
            return [data]
        raise ValueError(f"Unexpected catalog payload for {board.board_id}: {type(data)!r}")

    def fetch_thread(self, board: BoardConfig, thread_id: int) -> Dict[str, object]:
        data = self._request_json(board.thread_endpoint(thread_id), board)
        if isinstance(data, dict):
            return data
        raise ValueError(f"Unexpected thread payload for {board.board_id}: {type(data)!r}")

    def aggregate_threads(
        self,
        board_ids: Optional[Iterable[str]] = None,
        search: Optional[str] = None,
        limit: Optional[int] = None,
    ) -> List[AggregatedThread]:
        selected = self._select_boards(board_ids)
        results: List[AggregatedThread] = []
        for board in selected:
            try:
                catalog_pages = self.fetch_catalog(board)
            except requests.RequestException:
                continue
            except ValueError:
                continue
            for page in catalog_pages:
                for thread in page.get("threads", []):
                    normalized = self._normalise_thread(board, thread)
                    if not normalized:
                        continue
                    results.append(normalized)
        if search:
            needle = search.lower()
            results = [
                t
                for t in results
                if needle in t.title.lower()
                or needle in t.excerpt.lower()
                or needle in board_title(selected, t.board).lower()
            ]
        results.sort(key=lambda t: t.last_modified, reverse=True)
        if limit is not None:
            results = results[:limit]
        return results

    # ------------------------------- Internals ----------------------------
    def _select_boards(self, board_ids: Optional[Iterable[str]]) -> List[BoardConfig]:
        if not board_ids:
            return list(self.boards)
        requested = {b.strip() for b in board_ids if b.strip()}
        return [board for board in self.boards if board.board_id in requested]

    def _normalise_thread(self, board: BoardConfig, thread: Dict[str, object]) -> Optional[AggregatedThread]:
        try:
            thread_id = int(thread.get("no") or thread.get("num"))
        except (TypeError, ValueError):
            return None
        title = str(
            thread.get("sub")
            or thread.get("subject")
            or thread.get("title")
            or "(no subject)"
        )
        excerpt = str(
            thread.get("com")
            or thread.get("comment")
            or thread.get("teaser")
            or ""
        )
        replies = _safe_int(
            thread.get("replies")
            or thread.get("nreplies")
            or thread.get("omitted_posts")
            or 0
        )
        last_modified = _parse_timestamp(
            thread.get("last_modified")
            or thread.get("lastreply")
            or thread.get("last_reply")
            or thread.get("modified")
            or thread.get("time")
        )
        return AggregatedThread(
            board=board.board_id,
            source=board.source,
            thread_id=thread_id,
            title=html.unescape(title),
            excerpt=html.unescape(strip_html(excerpt)),
            replies=replies,
            last_modified=last_modified,
        )


_TAG_RE = re.compile(r"<[^>]+>")


def strip_html(raw: str) -> str:
    """Strip tags and normalise line breaks for comments."""

    no_breaks = raw.replace("<br>", "\n").replace("<br />", "\n")
    return _TAG_RE.sub("", no_breaks)


def _safe_int(value: object) -> int:
    try:
        return int(value)
    except (TypeError, ValueError):
        return 0


def _parse_timestamp(value: object) -> datetime:
    try:
        timestamp = int(value)
    except (TypeError, ValueError):
        return datetime.fromtimestamp(0, tz=timezone.utc)
    return datetime.fromtimestamp(timestamp, tz=timezone.utc)


def board_title(boards: Sequence[BoardConfig], board_id: str) -> str:
    for board in boards:
        if board.board_id == board_id:
            return board.title
    return board_id


__all__ = [
    "AggregatedThread",
    "ChanAggregator",
]
