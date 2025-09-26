from __future__ import annotations

import argparse
import asyncio
import logging
from pathlib import Path
from typing import Iterable, Optional

from .config import TcpClientConfig, TcpServerConfig, UdpConfig, load_config
from .logging_utils import configure_root_logger
from . import tcp_client, tcp_server, udp_client, udp_server


_PACKAGE_DIR = Path(__file__).resolve().parent
_PROJECT_DIR = _PACKAGE_DIR.parent
DEFAULT_TCP_SERVER_CONFIG = _PROJECT_DIR / "configs" / "tcp_server.toml"
DEFAULT_TCP_CLIENT_CONFIG = _PROJECT_DIR / "configs" / "tcp_client.toml"
DEFAULT_UDP_CONFIG = _PROJECT_DIR / "configs" / "udp.toml"


def _build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Async chat server/client demos")
    parser.add_argument("--log-level", default="INFO", help="Root logging level")
    subparsers = parser.add_subparsers(dest="command", required=True)

    tcp_server_parser = subparsers.add_parser("tcp-server", help="Run the TCP chat server")
    tcp_server_parser.add_argument("--config", default=str(DEFAULT_TCP_SERVER_CONFIG))

    tcp_client_parser = subparsers.add_parser("tcp-client", help="Run the TCP chat client")
    tcp_client_parser.add_argument("--config", default=str(DEFAULT_TCP_CLIENT_CONFIG))
    tcp_client_parser.add_argument("--user", required=True, help="Username to present to the server")

    udp_server_parser = subparsers.add_parser("udp-server", help="Run the UDP chat server")
    udp_server_parser.add_argument("--config", default=str(DEFAULT_UDP_CONFIG))

    udp_client_parser = subparsers.add_parser("udp-client", help="Run the UDP chat client")
    udp_client_parser.add_argument("--config", default=str(DEFAULT_UDP_CONFIG))
    udp_client_parser.add_argument("--user", required=True)

    return parser


async def _dispatch(args: argparse.Namespace) -> None:
    command = args.command
    if command == "tcp-server":
        config = load_config(args.config, TcpServerConfig)
        await tcp_server.serve(config)
    elif command == "tcp-client":
        config = load_config(args.config, TcpClientConfig)
        await tcp_client.TcpChatClient(config, args.user).run()
    elif command == "udp-server":
        config = load_config(args.config, UdpConfig)
        await udp_server.serve(config)
    elif command == "udp-client":
        config = load_config(args.config, UdpConfig)
        await udp_client.UdpChatClient(config, args.user).run()
    else:  # pragma: no cover - safety
        raise ValueError(f"Unknown command: {command}")


def main(argv: Optional[Iterable[str]] = None) -> None:
    parser = _build_parser()
    args = parser.parse_args(argv)
    configure_root_logger(getattr(logging, str(args.log_level).upper(), logging.INFO))
    asyncio.run(_dispatch(args))


if __name__ == "__main__":
    main()
