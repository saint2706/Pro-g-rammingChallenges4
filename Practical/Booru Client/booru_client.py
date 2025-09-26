"""Unified Booru API client with caching and rate limiting."""
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional
import hashlib
import json
import logging
import threading
import time

import requests


logger = logging.getLogger(__name__)


@dataclass
class Post:
    """Normalised representation of a Booru post."""

    id: int
    rating: str
    tags: List[str]
    file_url: str
    preview_url: Optional[str]
    source: str

    @property
    def filename(self) -> str:
        return Path(self.file_url).name


class RateLimiter:
    """Simple token bucket limiter (requests per second)."""

    def __init__(self, rate_per_sec: float) -> None:
        if rate_per_sec <= 0:
            raise ValueError("rate_per_sec must be positive")
        self._interval = 1.0 / rate_per_sec
        self._lock = threading.Lock()
        self._next_available = 0.0

    def wait(self) -> None:
        with self._lock:
            now = time.monotonic()
            wait_time = self._next_available - now
            if wait_time > 0:
                time.sleep(wait_time)
                now = time.monotonic()
            self._next_available = max(now, self._next_available) + self._interval


class CacheManager:
    """Lightweight JSON cache for API responses."""

    def __init__(self, cache_dir: Path, ttl: int = 900) -> None:
        self.cache_dir = cache_dir
        self.ttl = ttl
        self.cache_dir.mkdir(parents=True, exist_ok=True)

    def _path_for_key(self, key: str) -> Path:
        digest = hashlib.sha1(key.encode("utf-8")).hexdigest()
        return self.cache_dir / f"{digest}.json"

    def get(self, key: str) -> Optional[Dict]:
        path = self._path_for_key(key)
        if not path.exists():
            return None
        if self.ttl:
            age = time.time() - path.stat().st_mtime
            if age > self.ttl:
                return None
        try:
            with path.open("r", encoding="utf-8") as fp:
                return json.load(fp)
        except json.JSONDecodeError:
            logger.warning("Cache entry %s was corrupted", path)
            return None

    def set(self, key: str, value: Dict) -> None:
        path = self._path_for_key(key)
        with path.open("w", encoding="utf-8") as fp:
            json.dump(value, fp)


class BooruClient:
    """High level API client for Booru-compatible services."""

    _CONFIGS: Dict[str, Dict] = {
        "danbooru": {
            "base_url": "https://danbooru.donmai.us",
            "search_endpoint": "/posts.json",
            "post_endpoint": "/posts/{id}.json",
            "rate_limit": 1.0,
            "max_page_size": 20,
            "query_params": {
                "tags": "tags",
                "page": "page",
                "limit": "limit",
            },
            "rating_in_tags": True,
            "page_offset": 0,
            "page_min": 1,
        },
        "gelbooru": {
            "base_url": "https://gelbooru.com",
            "search_endpoint": "/index.php",
            "post_endpoint": "/index.php",
            "rate_limit": 0.5,
            "max_page_size": 100,
            "query_params": {
                "tags": "tags",
                "page": "pid",
                "limit": "limit",
            },
            "constant_params": {
                "page": "dapi",
                "s": "post",
                "q": "index",
                "json": "1",
            },
            "rating_in_tags": True,
            "page_offset": 1,
            "page_min": 0,
        },
        "safebooru": {
            "base_url": "https://safebooru.org",
            "search_endpoint": "/index.php",
            "post_endpoint": "/index.php",
            "rate_limit": 0.5,
            "max_page_size": 40,
            "query_params": {
                "tags": "tags",
                "page": "pid",
                "limit": "limit",
            },
            "constant_params": {
                "page": "dapi",
                "s": "post",
                "q": "index",
                "json": "1",
            },
            "rating_in_tags": True,
            "page_offset": 1,
            "page_min": 0,
        },
    }

    def __init__(
        self,
        booru: str = "danbooru",
        *,
        cache: Optional[CacheManager] = None,
        download_dir: Optional[Path] = None,
        session: Optional[requests.Session] = None,
        rate_limit: Optional[float] = None,
    ) -> None:
        booru_key = booru.lower()
        if booru_key not in self._CONFIGS:
            raise ValueError(f"Unsupported booru '{booru}'. Configure it in BooruClient._CONFIGS")
        self.config = self._CONFIGS[booru_key]
        self.base_url = self.config["base_url"].rstrip("/")
        self.session = session or requests.Session()
        self.cache = cache or CacheManager(Path("cache"))
        self.download_dir = (download_dir or Path("downloads")).expanduser()
        self.download_dir.mkdir(parents=True, exist_ok=True)
        rl_rate = rate_limit or self.config.get("rate_limit", 1.0)
        self.rate_limiter = RateLimiter(rl_rate)

    # ------------------------------ HTTP helpers ---------------------------
    def _build_url(self, endpoint: str) -> str:
        if endpoint.startswith("http"):
            return endpoint
        return f"{self.base_url}{endpoint}"

    def _request_json(self, method: str, url: str, params: Dict) -> Dict:
        self.rate_limiter.wait()
        response = self.session.request(method, url, params=params, timeout=30)
        response.raise_for_status()
        return response.json()

    # ------------------------------- Search --------------------------------
    def search_posts(
        self,
        *,
        tags: Optional[Iterable[str]] = None,
        rating: Optional[str] = None,
        limit: int = 20,
        page: int = 1,
        extra_filters: Optional[Dict[str, str]] = None,
        use_cache: bool = True,
    ) -> List[Post]:
        """Search posts with pagination and optional rating filter."""

        if limit <= 0:
            raise ValueError("limit must be positive")

        tags_list = list(tags or [])
        if rating:
            tags_list.append(f"rating:{rating}")
        query_tags = " ".join(tags_list).strip()

        cache_key = json.dumps(
            {
                "action": "search",
                "booru": self.base_url,
                "tags": query_tags,
                "page": page,
                "limit": limit,
                "extra": extra_filters or {},
            },
            sort_keys=True,
        )
        if use_cache and (cached := self.cache.get(cache_key)) is not None:
            return [Post(**item) for item in cached["posts"]]

        params = self._build_query_params(query_tags, limit, page, extra_filters)
        data = self._request_json("GET", self._build_url(self.config["search_endpoint"]), params)
        posts = self._normalise_posts(data)

        if use_cache:
            self.cache.set(cache_key, {"posts": [post.__dict__ for post in posts]})
        return posts

    def _build_query_params(
        self,
        tags: str,
        limit: int,
        page: int,
        extra_filters: Optional[Dict[str, str]],
    ) -> Dict[str, str]:
        params = dict(self.config.get("constant_params", {}))
        qp = self.config["query_params"]
        params[qp["limit"]] = str(min(limit, self.config.get("max_page_size", limit)))
        offset = self.config.get("page_offset", 0)
        page_min = self.config.get("page_min", 0)
        params[qp["page"]] = str(max(page - offset, page_min))
        if tags:
            params[qp["tags"]] = tags
        if extra_filters:
            params.update({k: str(v) for k, v in extra_filters.items()})
        return params

    def _normalise_posts(self, payload) -> List[Post]:
        if isinstance(payload, dict) and "post" in payload:
            items = payload["post"]
        else:
            items = payload
        posts: List[Post] = []
        for item in items or []:
            if isinstance(item, dict):
                posts.append(
                    Post(
                        id=int(item.get("id")),
                        rating=str(item.get("rating", "unknown")),
                        tags=self._split_tags(item),
                        file_url=item.get("file_url") or item.get("file_url".upper(), ""),
                        preview_url=item.get("preview_file_url")
                        or item.get("preview_url")
                        or item.get("preview_url".upper()),
                        source=item.get("source", self.base_url),
                    )
                )
        return posts

    def _split_tags(self, item: Dict) -> List[str]:
        tags = (
            item.get("tag_string")
            or item.get("tags")
            or item.get("tag_string_general")
            or ""
        )
        if isinstance(tags, list):
            return [str(t) for t in tags]
        return [t for t in str(tags).split() if t]

    # ------------------------------- Posts ---------------------------------
    def get_post(self, post_id: int, use_cache: bool = True) -> Optional[Post]:
        cache_key = json.dumps({"action": "post", "id": post_id, "booru": self.base_url})
        if use_cache and (cached := self.cache.get(cache_key)) is not None:
            return Post(**cached["post"])

        params = dict(self.config.get("constant_params", {}))
        qp = self.config["query_params"]
        params[qp.get("limit", "limit")] = "1"
        params["id"] = str(post_id)
        data = self._request_json("GET", self._build_url(self.config["post_endpoint"]), params)
        posts = self._normalise_posts(data)
        post = posts[0] if posts else None
        if post and use_cache:
            self.cache.set(cache_key, {"post": post.__dict__})
        return post

    # ------------------------------ Download -------------------------------
    def download_post(self, post: Post, *, destination: Optional[Path] = None) -> Path:
        """Download the file referenced by *post* and return its path."""

        dest_dir = (destination or self.download_dir).expanduser()
        dest_dir.mkdir(parents=True, exist_ok=True)
        target = dest_dir / post.filename

        if target.exists():
            logger.info("Skipping download of %s; file already exists", target)
            return target

        self.rate_limiter.wait()
        with self.session.get(post.file_url, stream=True, timeout=60) as response:
            response.raise_for_status()
            with target.open("wb") as fh:
                for chunk in response.iter_content(chunk_size=8192):
                    if chunk:
                        fh.write(chunk)
        self._write_metadata(post, target)
        return target

    def _write_metadata(self, post: Post, image_path: Path) -> None:
        metadata = {
            "id": post.id,
            "rating": post.rating,
            "tags": post.tags,
            "file_url": post.file_url,
            "source": post.source,
            "downloaded_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        }
        meta_path = image_path.with_suffix(image_path.suffix + ".json")
        with meta_path.open("w", encoding="utf-8") as fh:
            json.dump(metadata, fh, indent=2)

    def tag_image(self, image_path: Path, tags: Iterable[str]) -> Path:
        """Append custom tags to an existing metadata file."""

        meta_path = image_path.with_suffix(image_path.suffix + ".json")
        data = {"custom_tags": list(tags)}
        if meta_path.exists():
            with meta_path.open("r", encoding="utf-8") as fh:
                try:
                    existing = json.load(fh)
                except json.JSONDecodeError:
                    existing = {}
            existing.update(data)
            data = existing
        with meta_path.open("w", encoding="utf-8") as fh:
            json.dump(data, fh, indent=2)
        return meta_path


__all__ = ["BooruClient", "Post", "CacheManager", "RateLimiter"]
