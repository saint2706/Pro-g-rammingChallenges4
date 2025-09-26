"""Command line interface for the Booru client."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Iterable, List

from booru_client import BooruClient, CacheManager


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Search and download posts from Booru-style APIs")
    parser.add_argument("--booru", default="danbooru", help="Target booru (danbooru, gelbooru, safebooru)")
    parser.add_argument("--cache-dir", default="cache", help="Directory for cached responses")
    parser.add_argument("--cache-ttl", type=int, default=900, help="Cache freshness window in seconds")
    parser.add_argument("--download-dir", default="downloads", help="Default download directory")

    subparsers = parser.add_subparsers(dest="command", required=True)

    search = subparsers.add_parser("search", help="Search posts and optionally download them")
    search.add_argument("--tags", default="", help="Space separated tags")
    search.add_argument("--rating", choices=["safe", "questionable", "explicit"], help="Filter by rating")
    search.add_argument("--limit", type=int, default=10, help="Number of posts to fetch")
    search.add_argument("--page", type=int, default=1, help="Page number (1-based)")
    search.add_argument("--extra", nargs="*", help="Additional key=value filters")
    search.add_argument("--download", nargs="?", const=".", help="Download results to optional directory")
    search.add_argument("--json", action="store_true", help="Print full JSON payload")

    download = subparsers.add_parser("download", help="Download a specific post by id")
    download.add_argument("id", type=int, help="Post identifier")
    download.add_argument("--output", default=None, help="Optional download directory")

    tag = subparsers.add_parser("tag", help="Attach custom tags to an existing image")
    tag.add_argument("file", type=Path, help="Downloaded image path")
    tag.add_argument("tags", nargs="+", help="Tags to append to metadata")

    browse = subparsers.add_parser("browse", help="Launch the Tkinter GUI browser")
    browse.add_argument("--page-size", type=int, default=20, help="Results per page in the GUI")

    return parser


def parse_extra(extra: Iterable[str] | None) -> dict:
    result = {}
    if not extra:
        return result
    for item in extra:
        if "=" not in item:
            raise SystemExit(f"Invalid extra filter '{item}'. Use key=value")
        key, value = item.split("=", 1)
        result[key] = value
    return result


def create_client(args: argparse.Namespace) -> BooruClient:
    cache = CacheManager(Path(args.cache_dir), ttl=args.cache_ttl)
    return BooruClient(
        args.booru,
        cache=cache,
        download_dir=Path(args.download_dir),
    )


def handle_search(args: argparse.Namespace) -> None:
    client = create_client(args)
    tags = [tag for tag in args.tags.split() if tag]
    posts = client.search_posts(
        tags=tags,
        rating=args.rating,
        limit=args.limit,
        page=args.page,
        extra_filters=parse_extra(args.extra),
    )

    if args.json:
        print(json.dumps([post.__dict__ for post in posts], indent=2))
    else:
        for post in posts:
            preview = post.preview_url or "(no preview)"
            print(f"#{post.id} rating={post.rating} file={post.file_url} preview={preview}")
            print(f"  tags: {' '.join(post.tags)}")

    if args.download is not None:
        dest = Path(args.download)
        for post in posts:
            client.download_post(post, destination=dest)
        print(f"Downloaded {len(posts)} posts to {dest.resolve()}")


def handle_download(args: argparse.Namespace) -> None:
    client = create_client(args)
    post = client.get_post(args.id)
    if not post:
        raise SystemExit(f"Post {args.id} not found")
    path = client.download_post(post, destination=Path(args.output) if args.output else None)
    print(path.resolve())


def handle_tag(args: argparse.Namespace) -> None:
    client = create_client(args)
    meta_path = client.tag_image(args.file, args.tags)
    print(meta_path.resolve())


def handle_browse(args: argparse.Namespace) -> None:
    from gui import launch_gui

    launch_gui(
        booru=args.booru,
        cache_dir=Path(args.cache_dir),
        download_dir=Path(args.download_dir),
        page_size=args.page_size,
        cache_ttl=args.cache_ttl,
    )


def main(argv: List[str] | None = None) -> None:
    parser = build_parser()
    args = parser.parse_args(argv)

    if args.command == "search":
        handle_search(args)
    elif args.command == "download":
        handle_download(args)
    elif args.command == "tag":
        handle_tag(args)
    elif args.command == "browse":
        handle_browse(args)
    else:
        parser.error("Unknown command")


if __name__ == "__main__":  # pragma: no cover - CLI entry point
    main()
