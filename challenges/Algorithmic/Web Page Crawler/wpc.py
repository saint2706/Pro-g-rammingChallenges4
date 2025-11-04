"""Lightweight Web Page Crawler.

Modernized features:
  * Dataclass configuration (depth, same-domain constraint, max pages, rate limit)
  * Optional robots.txt compliance (--robots)
  * JSON output summarizing crawl (--json) plus optional export of edge list (--edges)
  * Content-type & size guarding to avoid binary / huge downloads
  * Graceful timeout + error handling with reason classification
  * De-duplication via set; queue uses deque for BFS ordering
  * Extensible design: override `handle_page` for custom processing

Usage examples:
  python wpc.py https://example.com --depth 1 --json
  python wpc.py example.com -d 2 --max-pages 100 --rate 0.5 --robots --edges links.txt
  python wpc.py https://example.com --no-same-domain  # allow external domains

Notes:
  * This is intentionally minimal; for production consider: politeness delays per host,
    retries with backoff, persistent storage, JS rendering support, etc.
"""

from __future__ import annotations

import argparse
import json
import sys
import time
from collections import deque
from dataclasses import dataclass
from pathlib import Path
from typing import Deque, Dict, Iterable, List, Optional, Set, Tuple
from urllib.parse import urljoin, urlparse
from urllib.robotparser import RobotFileParser

import requests
from bs4 import BeautifulSoup

DEFAULT_UA = "MiniCrawler/1.0 (+https://github.com/saintwithataint)"


# ------------------------------ Configuration ------------------------------ #


@dataclass(slots=True)
class CrawlerConfig:
    start_url: str
    max_depth: int = 2
    max_pages: int = 500
    same_domain: bool = True
    rate_limit: float = 0.0  # seconds between requests (simple global spacing)
    per_host_delay: float = 0.0  # politeness spacing per host
    timeout: float = 6.0
    user_agent: str = DEFAULT_UA
    robots: bool = False
    json_out: bool = False
    edges_path: Optional[str] = None
    state_path: Optional[Path] = None

    def normalize(self) -> None:
        if not urlparse(self.start_url).scheme:
            self.start_url = "http://" + self.start_url
        if self.max_depth < 0:
            raise ValueError("max_depth must be >= 0")
        if self.max_pages < 1:
            raise ValueError("max_pages must be >= 1")
        if self.rate_limit < 0:
            raise ValueError("rate_limit must be >= 0")
        if self.per_host_delay < 0:
            raise ValueError("per_host_delay must be >= 0")
        if self.state_path is not None:
            self.state_path = self.state_path.expanduser().resolve()


# ------------------------------ Core Crawler ------------------------------ #


class WebCrawler:
    def __init__(self, cfg: CrawlerConfig):
        self.cfg = cfg
        self.root_domain = urlparse(cfg.start_url).netloc
        self.visited: Set[str] = set()
        self.queue: Deque[Tuple[str, int]] = deque([(cfg.start_url, 0)])
        self.edges: List[Tuple[str, str]] = []
        self.errors: Dict[str, str] = {}
        self.robot_parser: Optional[RobotFileParser] = None
        self._last_global_request = 0.0
        self._last_host_request: Dict[str, float] = {}
        if cfg.robots:
            self._init_robots()
        self._load_state()

    # ---- Robots ---- #
    def _init_robots(self) -> None:
        robots_url = urljoin(self.cfg.start_url, "/robots.txt")
        rp = RobotFileParser()
        try:
            rp.set_url(robots_url)
            rp.read()
            self.robot_parser = rp
        except Exception:
            # Ignore robots failure; treat as no restrictions
            self.robot_parser = None

    def _allowed(self, url: str) -> bool:
        if not self.robot_parser:
            return True
        return self.robot_parser.can_fetch(self.cfg.user_agent, url)

    # ---- Fetch & Parse ---- #
    def fetch(self, url: str) -> Optional[BeautifulSoup]:
        headers = {"User-Agent": self.cfg.user_agent}
        try:
            limit_bytes = 2 * 1024 * 1024
            chunks: List[bytes] = []
            downloaded = 0
            with requests.get(
                url, timeout=self.cfg.timeout, headers=headers, stream=True
            ) as resp:
                resp.raise_for_status()
                ctype = resp.headers.get("Content-Type", "")
                if "text/html" not in ctype.lower():
                    return None
                for chunk in resp.iter_content(chunk_size=16384):
                    if not chunk:
                        continue
                    chunks.append(chunk)
                    downloaded += len(chunk)
                    if downloaded >= limit_bytes:
                        break
            if not chunks:
                return None
            content = b"".join(chunks)
            return BeautifulSoup(content, "html.parser")
        except requests.exceptions.RequestException as e:
            self.errors[url] = type(e).__name__
            return None
        except Exception as e:  # pragma: no cover - unexpected
            self.errors[url] = f"Unexpected:{type(e).__name__}"
            return None

    # ---- Link Extraction ---- #
    def extract_links(self, soup: BeautifulSoup, base_url: str) -> Iterable[str]:
        from bs4.element import Tag  # local import keeps top minimal

        for a in soup.find_all("a", href=True):
            if not isinstance(a, Tag):  # type guard for static analysis
                continue
            href_val = a.get("href")
            if not href_val or not isinstance(href_val, str):
                continue
            absolute = urljoin(base_url, href_val)
            parsed = urlparse(absolute)
            clean = parsed._replace(fragment="").geturl()
            if self._accept_link(clean):
                yield clean

    def _accept_link(self, url: str) -> bool:
        parsed = urlparse(url)
        if parsed.scheme not in ("http", "https"):
            return False
        if self.cfg.same_domain and parsed.netloc != self.root_domain:
            return False
        if self.cfg.robots and not self._allowed(url):
            return False
        return True

    # ---- Crawl Loop ---- #
    def crawl(self) -> None:
        pages_fetched = 0
        while self.queue and pages_fetched < self.cfg.max_pages:
            url, depth = self.queue.popleft()
            if url in self.visited or depth > self.cfg.max_depth:
                continue
            self.visited.add(url)
            print(f"[{depth}] {url}")
            self._wait_for_politeness(url)
            soup = self.fetch(url)
            self._record_request(url)
            if soup is None:
                continue
            for link in self.extract_links(soup, url):
                self.edges.append((url, link))
                if link not in self.visited and all(q[0] != link for q in self.queue):
                    self.queue.append((link, depth + 1))
            pages_fetched += 1
            self.handle_page(url, soup)
            self._persist_state()

    def _wait_for_politeness(self, url: str) -> None:
        sleep_for = 0.0
        now = time.time()
        if self.cfg.rate_limit > 0:
            sleep_for = max(sleep_for, self.cfg.rate_limit - (now - self._last_global_request))
        if self.cfg.per_host_delay > 0:
            host = urlparse(url).netloc
            host_wait = self.cfg.per_host_delay - (now - self._last_host_request.get(host, 0.0))
            sleep_for = max(sleep_for, host_wait)
        if sleep_for > 0:
            time.sleep(max(0.0, sleep_for))

    def _record_request(self, url: str) -> None:
        now = time.time()
        self._last_global_request = now
        if self.cfg.per_host_delay > 0:
            host = urlparse(url).netloc
            self._last_host_request[host] = now

    def _persist_state(self) -> None:
        if not self.cfg.state_path:
            return
        state = {
            "visited": sorted(self.visited),
            "queue": list(self.queue),
        }
        try:
            self.cfg.state_path.parent.mkdir(parents=True, exist_ok=True)
            self.cfg.state_path.write_text(json.dumps(state, indent=2), encoding="utf-8")
        except OSError as exc:
            self.errors.setdefault("__state__", str(exc))

    def _load_state(self) -> None:
        if not self.cfg.state_path or not self.cfg.state_path.exists():
            return
        try:
            data = json.loads(self.cfg.state_path.read_text(encoding="utf-8"))
            visited = data.get("visited", [])
            queue = data.get("queue", [])
            if isinstance(visited, list):
                self.visited.update(v for v in visited if isinstance(v, str))
            restored_queue: List[Tuple[str, int]] = []
            if isinstance(queue, list):
                for item in queue:
                    if (
                        isinstance(item, (list, tuple))
                        and len(item) == 2
                        and isinstance(item[0], str)
                    ):
                        try:
                            restored_queue.append((item[0], int(item[1])))
                        except (TypeError, ValueError):
                            continue
            if restored_queue:
                self.queue = deque(restored_queue)
        except (OSError, json.JSONDecodeError, ValueError) as exc:
            self.errors.setdefault("__state__", f"load_failed:{exc}")

    # Extension point
    def handle_page(
        self, url: str, soup: BeautifulSoup
    ) -> None:  # pragma: no cover - default no-op
        pass

    # Summary for JSON
    def summary(self) -> dict:
        return {
            "start_url": self.cfg.start_url,
            "max_depth": self.cfg.max_depth,
            "max_pages": self.cfg.max_pages,
            "visited_count": len(self.visited),
            "edges_count": len(self.edges),
            "same_domain": self.cfg.same_domain,
            "rate_limit": self.cfg.rate_limit,
            "per_host_delay": self.cfg.per_host_delay,
            "robots": self.cfg.robots,
            "state_path": str(self.cfg.state_path) if self.cfg.state_path else None,
            "errors": self.errors,
        }


# ------------------------------ CLI ------------------------------ #


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(description="Lightweight web crawler")
    p.add_argument("url", help="Starting URL (scheme optional)")
    p.add_argument(
        "-d", "--depth", type=int, default=2, help="Maximum crawl depth (BFS levels)"
    )
    p.add_argument(
        "--max-pages", type=int, default=500, help="Maximum number of pages to fetch"
    )
    p.add_argument(
        "--no-same-domain", action="store_true", help="Allow following external domains"
    )
    p.add_argument(
        "--rate",
        type=float,
        default=0.0,
        help="Seconds between requests (simple rate limit)",
    )
    p.add_argument(
        "--host-delay",
        type=float,
        default=0.0,
        help="Minimum seconds between requests to the same host",
    )
    p.add_argument("--timeout", type=float, default=6.0, help="Request timeout seconds")
    p.add_argument(
        "--ua",
        "--user-agent",
        dest="ua",
        default=DEFAULT_UA,
        help="Custom User-Agent string",
    )
    p.add_argument(
        "--robots", action="store_true", help="Respect robots.txt (best-effort)"
    )
    p.add_argument("--json", action="store_true", help="Emit JSON summary to stdout")
    p.add_argument(
        "--edges", metavar="PATH", help="Write discovered edges (src\tdst) to file"
    )
    p.add_argument(
        "--state",
        type=Path,
        help="Persist crawl state (visited + queue) to this JSON file",
    )
    return p


def main(argv: Optional[List[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    cfg = CrawlerConfig(
        start_url=args.url,
        max_depth=args.depth,
        max_pages=args.max_pages,
        same_domain=not args.no_same_domain,
        rate_limit=args.rate,
        per_host_delay=args.host_delay,
        timeout=args.timeout,
        user_agent=args.ua,
        robots=args.robots,
        json_out=args.json,
        edges_path=args.edges,
        state_path=args.state,
    )
    try:
        cfg.normalize()
    except ValueError as e:
        parser.error(str(e))

    crawler = WebCrawler(cfg)
    try:
        crawler.crawl()
    except KeyboardInterrupt:
        print("\nInterrupted", file=sys.stderr)
    finally:
        if cfg.edges_path:
            try:
                with open(cfg.edges_path, "w", encoding="utf-8") as f:
                    for src, dst in crawler.edges:
                        f.write(f"{src}\t{dst}\n")
            except OSError as e:
                print(f"Failed to write edges file: {e}", file=sys.stderr)
        if cfg.json_out:
            print(json.dumps(crawler.summary(), indent=2))
        else:
            print(
                f"Visited {len(crawler.visited)} pages; edges: {len(crawler.edges)}; errors: {len(crawler.errors)}"
            )
    return 0


if __name__ == "__main__":  # pragma: no cover - CLI entry
    raise SystemExit(main())
