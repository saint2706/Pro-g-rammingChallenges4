"""Tests for the Booru client using mocked responses."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict, List, Optional

import pytest

import sys

sys.path.append(str(Path(__file__).resolve().parents[1]))

from booru_client import BooruClient, CacheManager, Post


class FakeResponse:
    def __init__(self, payload: Any = None, *, content: bytes | None = None):
        self._payload = payload
        self.content = content or b""

    # --- context manager for downloads ---
    def __enter__(self) -> "FakeResponse":
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        return None

    # --- HTTP API ---
    def raise_for_status(self) -> None:
        return None

    def json(self) -> Any:
        return self._payload

    def iter_content(self, chunk_size: int = 8192):
        yield self.content


class RecordingSession:
    def __init__(self, payload: Any):
        self.payload = payload
        self.calls: List[Dict[str, Any]] = []
        self.download_content = b"test-bytes"

    def request(
        self,
        method: str,
        url: str,
        params: Optional[Dict[str, Any]] = None,
        timeout: int = 0,
    ):
        self.calls.append({"method": method, "url": url, "params": dict(params or {})})
        return FakeResponse(self.payload)

    def get(self, url: str, stream: bool = False, timeout: int = 0):
        self.calls.append(
            {"method": "GET", "url": url, "stream": stream, "timeout": timeout}
        )
        return FakeResponse(content=self.download_content)


@pytest.fixture()
def cache_dir(tmp_path: Path) -> CacheManager:
    return CacheManager(tmp_path, ttl=3600)


def make_post_payload(post_id: int = 1) -> Dict[str, Any]:
    return [
        {
            "id": post_id,
            "rating": "safe",
            "tag_string": "cat sky",
            "file_url": "https://example.test/full.jpg",
            "preview_file_url": "https://example.test/thumb.jpg",
            "source": "https://example.test/post",
        }
    ]


def test_search_posts_uses_cache(cache_dir: CacheManager) -> None:
    session = RecordingSession(make_post_payload())
    client = BooruClient("danbooru", session=session, cache=cache_dir)

    first = client.search_posts(tags=["cat"], rating="safe", limit=2, page=1)
    assert isinstance(first[0], Post)
    assert session.calls[0]["params"]["tags"] == "cat rating:safe"

    # Mutate payload to ensure cache is used
    session.payload = make_post_payload(post_id=999)
    second = client.search_posts(tags=["cat"], rating="safe", limit=2, page=1)
    assert [post.id for post in second] == [post.id for post in first]
    # Only one API call should have been recorded for search (download call not triggered)
    assert len(session.calls) == 1


def test_get_post_fetches_single_post(cache_dir: CacheManager) -> None:
    session = RecordingSession(make_post_payload(post_id=42))
    client = BooruClient("danbooru", session=session, cache=cache_dir)

    post = client.get_post(42, use_cache=False)
    assert post is not None
    assert post.id == 42
    assert session.calls[0]["params"]["id"] == "42"


def test_download_post_writes_metadata_and_custom_tags(
    tmp_path: Path, cache_dir: CacheManager
) -> None:
    session = RecordingSession(make_post_payload())
    client = BooruClient(
        "danbooru", session=session, cache=cache_dir, download_dir=tmp_path
    )
    post_data = make_post_payload()[0]
    post = Post(
        id=post_data["id"],
        rating=post_data["rating"],
        tags=post_data["tag_string"].split(),
        file_url=post_data["file_url"],
        preview_url=post_data["preview_file_url"],
        source=post_data["source"],
    )
    downloaded = client.download_post(post)

    assert downloaded.exists()
    meta = json.loads(downloaded.with_suffix(downloaded.suffix + ".json").read_text())
    assert meta["id"] == post.id
    assert meta["tags"] == post.tags

    tagged_path = client.tag_image(downloaded, ["favorite", "wallpaper"])
    tagged = json.loads(tagged_path.read_text())
    assert "custom_tags" in tagged
    assert set(tagged["custom_tags"]) == {"favorite", "wallpaper"}

    # Ensure the download used streaming GET
    assert any(call.get("stream") for call in session.calls if call["method"] == "GET")
