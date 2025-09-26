"""Command line interface for the Torrent Client."""
from __future__ import annotations

import argparse
import logging
from pathlib import Path

from tqdm import tqdm

from torrent_client import TorrentClient, load_torrent


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Minimal BitTorrent client (single-file)")
    parser.add_argument("torrent", type=Path, help="Path to the .torrent file")
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("downloads"),
        help="Directory where the downloaded file will be placed",
    )
    parser.add_argument("--port", type=int, default=6881, help="Port used for tracker announces")
    parser.add_argument(
        "--no-resume", dest="resume", action="store_false", help="Ignore resume data and re-download"
    )
    parser.add_argument("--verbose", action="store_true", help="Enable debug logging")
    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)

    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.INFO,
        format="%(asctime)s [%(levelname)s] %(name)s: %(message)s",
    )

    metainfo = load_torrent(args.torrent)
    client = TorrentClient(metainfo=metainfo, download_dir=args.output, resume=args.resume, port=args.port)

    total_pieces = client.piece_manager.total_pieces
    initial = client.piece_manager.completed_count
    with tqdm(total=total_pieces, initial=initial, unit="piece", desc=metainfo.name) as bar:
        def update(completed: int, total: int) -> None:
            bar.total = total
            bar.n = completed
            bar.refresh()

        client.download(progress_callback=update)
    return 0


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
