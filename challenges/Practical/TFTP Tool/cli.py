"""Command-line interface for the educational TFTP tool."""

from __future__ import annotations

import argparse
import logging
import sys
import time
from pathlib import Path

from tftp import TFTPClient, TFTPError, TFTPServer

DEFAULT_PORT = 6969


def configure_logging(verbosity: int) -> None:
    level = logging.WARNING
    if verbosity == 1:
        level = logging.INFO
    elif verbosity >= 2:
        level = logging.DEBUG
    logging.basicConfig(
        level=level, format="%(asctime)s %(levelname)s %(name)s: %(message)s"
    )


def run_server(args: argparse.Namespace) -> int:
    server = TFTPServer(
        args.root,
        host=args.host,
        port=args.port,
        timeout=args.timeout,
        retries=args.retries,
    )
    try:
        server.start()
        logging.getLogger(__name__).info(
            "Server ready on %s:%s", args.host, server.server_port
        )
        while True:
            time.sleep(1.0)
    except KeyboardInterrupt:
        logging.getLogger(__name__).info("Shutting down server")
        return 0
    finally:
        server.stop()


def run_get(args: argparse.Namespace) -> int:
    client = TFTPClient(
        args.host,
        port=args.port,
        timeout=args.timeout,
        retries=args.retries,
    )
    try:
        client.download(args.remote, args.local, block_size=args.blksize)
        logging.getLogger(__name__).info("Downloaded %s -> %s", args.remote, args.local)
        return 0
    except (TFTPError, OSError) as exc:
        logging.getLogger(__name__).error("Download failed: %s", exc)
        return 1


def run_put(args: argparse.Namespace) -> int:
    client = TFTPClient(
        args.host,
        port=args.port,
        timeout=args.timeout,
        retries=args.retries,
    )
    try:
        client.upload(args.local, args.remote, block_size=args.blksize)
        logging.getLogger(__name__).info("Uploaded %s -> %s", args.local, args.remote)
        return 0
    except (TFTPError, OSError) as exc:
        logging.getLogger(__name__).error("Upload failed: %s", exc)
        return 1


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Educational TFTP client/server")
    parser.add_argument(
        "--port", type=int, default=DEFAULT_PORT, help="UDP port (default: %(default)s)"
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=3.0,
        help="Socket timeout in seconds (default: %(default)s)",
    )
    parser.add_argument(
        "--retries", type=int, default=5, help="Retransmission attempts before aborting"
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="Increase log verbosity (repeat for debug)",
    )

    subparsers = parser.add_subparsers(dest="command", required=True)

    server_parser = subparsers.add_parser("server", help="Run a TFTP server")
    server_parser.add_argument("root", type=Path, help="Directory to serve files from")
    server_parser.add_argument(
        "--host", default="0.0.0.0", help="Bind address (default: %(default)s)"
    )
    server_parser.set_defaults(func=run_server)

    get_parser = subparsers.add_parser("get", help="Download a file")
    get_parser.add_argument("host", help="Server hostname or IP")
    get_parser.add_argument("remote", help="Remote filename")
    get_parser.add_argument("local", type=Path, help="Destination path")
    get_parser.add_argument(
        "--blksize",
        type=int,
        default=512,
        help="Request block size (default: %(default)s)",
    )
    get_parser.set_defaults(func=run_get)

    put_parser = subparsers.add_parser("put", help="Upload a file")
    put_parser.add_argument("host", help="Server hostname or IP")
    put_parser.add_argument("local", type=Path, help="Local file to send")
    put_parser.add_argument("remote", help="Destination filename on the server")
    put_parser.add_argument(
        "--blksize",
        type=int,
        default=512,
        help="Request block size (default: %(default)s)",
    )
    put_parser.set_defaults(func=run_put)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    configure_logging(args.verbose)
    return args.func(args)


if __name__ == "__main__":
    sys.exit(main())
