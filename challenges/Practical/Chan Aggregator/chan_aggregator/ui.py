"""Command line interface for the aggregator."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
from typing import Iterable, List, Optional

from .config import BoardConfig, load_board_configs
from .core import ChanAggregator, AggregatedThread


def parse_args(argv: Optional[Iterable[str]] = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Aggregate threads from multiple boards"
    )
    parser.add_argument("--boards", help="Comma separated board IDs", default="")
    parser.add_argument(
        "--limit", type=int, default=None, help="Maximum number of threads to show"
    )
    parser.add_argument("--search", default=None, help="Case insensitive search filter")
    parser.add_argument(
        "--json", action="store_true", help="Output JSON instead of table"
    )
    parser.add_argument(
        "--config",
        type=Path,
        default=None,
        help="Optional path to a JSON configuration file (defaults to built-in preset)",
    )
    return parser.parse_args(argv)


def load_configs_from_args(args: argparse.Namespace) -> List[BoardConfig]:
    paths: List[Path] = []
    if args.config:
        paths.append(args.config)
    else:
        default_path = Path("boards.json")
        if default_path.exists():
            paths.append(default_path)
    return load_board_configs(paths)


def main(argv: Optional[Iterable[str]] = None) -> int:
    args = parse_args(argv)
    boards = load_configs_from_args(args)
    aggregator = ChanAggregator(boards)
    board_ids = [b.strip() for b in args.boards.split(",") if b.strip()]
    threads = aggregator.aggregate_threads(
        board_ids=board_ids or None,
        search=args.search,
        limit=args.limit,
    )
    if args.json:
        print(json.dumps([thread.as_dict() for thread in threads], indent=2))
    else:
        print(render_table(threads))
    return 0


def render_table(threads: List[AggregatedThread]) -> str:
    if not threads:
        return "No threads found."
    headers = ["Board", "Source", "Thread", "Replies", "Last Activity", "Subject"]
    rows = [
        [
            thread.board,
            thread.source or "-",
            str(thread.thread_id),
            str(thread.replies),
            thread.last_modified.strftime("%Y-%m-%d %H:%M UTC"),
            thread.title,
        ]
        for thread in threads
    ]
    widths = [
        max(len(headers[i]), max(len(row[i]) for row in rows))
        for i in range(len(headers))
    ]
    sep = "  "
    header_line = sep.join(h.ljust(widths[i]) for i, h in enumerate(headers))
    divider = sep.join("-" * widths[i] for i in range(len(headers)))
    body_lines = [
        sep.join(row[i].ljust(widths[i]) for i in range(len(headers))) for row in rows
    ]
    return "\n".join([header_line, divider, *body_lines])


if __name__ == "__main__":  # pragma: no cover - allow module execution
    raise SystemExit(main())
