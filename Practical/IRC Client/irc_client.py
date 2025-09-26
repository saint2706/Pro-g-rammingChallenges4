"""Async terminal IRC client implementation.

This module provides a small asyncio-powered IRC client that can:
* connect to an IRC server (optionally over TLS)
* authenticate with PASS/NICK/USER commands
* join multiple channels
* stream server messages to the terminal while reading user input concurrently
* log every incoming and outgoing line to a timestamped file
* gracefully reconnect with exponential backoff on connection errors

The entrypoint is :func:`main` which parses CLI arguments and runs
:class:`IRCClient`.
"""
from __future__ import annotations

import argparse
import asyncio
import contextlib
import logging
import shlex
import signal
import ssl
import sys
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import List, Optional, Sequence

__all__ = ["IRCClient", "IRCConfig", "main"]


@dataclass
class IRCConfig:
    """Configuration for :class:`IRCClient`."""

    server: str
    port: int
    nickname: str
    username: str
    realname: str
    password: Optional[str]
    channels: List[str] = field(default_factory=list)
    use_tls: bool = False
    log_file: Path = field(default_factory=lambda: Path("irc.log"))
    reconnect: bool = True
    max_retries: Optional[int] = None
    retry_delay: float = 3.0


class IRCClient:
    """A minimal asyncio IRC client with reconnection support."""

    def __init__(self, config: IRCConfig, logger: logging.Logger) -> None:
        self.config = config
        self.logger = logger
        self.reader: Optional[asyncio.StreamReader] = None
        self.writer: Optional[asyncio.StreamWriter] = None
        self.stop_requested = asyncio.Event()
        self.primary_target: Optional[str] = config.channels[0] if config.channels else None

    async def run(self) -> None:
        """Entry point for running the client with reconnection logic."""

        retries = 0
        delay = max(0.1, self.config.retry_delay)

        while not self.stop_requested.is_set():
            try:
                await self._connect()
                retries = 0
                delay = max(0.1, self.config.retry_delay)
                await self._session_loop()
            except asyncio.CancelledError:
                raise
            except Exception as exc:  # noqa: BLE001 - log then retry/exit
                if self.stop_requested.is_set():
                    break
                self._print_status(f"Connection error: {exc!s}")
                self.logger.exception("Connection error")
            finally:
                await self._close_writer()

            if self.stop_requested.is_set():
                break

            if not self.config.reconnect:
                self._print_status("Reconnect disabled; exiting.")
                break

            retries += 1
            if self.config.max_retries is not None and retries > self.config.max_retries:
                self._print_status("Maximum reconnect attempts reached; exiting.")
                break

            self._print_status(f"Reconnecting in {delay:.1f}s (attempt {retries})...")
            try:
                await asyncio.sleep(delay)
            except asyncio.CancelledError:
                break
            delay = min(delay * 2, 60.0)

    async def _connect(self) -> None:
        """Establish the connection and perform initial handshake."""

        self._print_status(f"Connecting to {self.config.server}:{self.config.port}...")
        ssl_context = ssl.create_default_context() if self.config.use_tls else None
        self.reader, self.writer = await asyncio.open_connection(
            host=self.config.server,
            port=self.config.port,
            ssl=ssl_context,
        )
        self._print_status("Connected. Negotiating nickname…")

        if self.config.password:
            await self._send_raw(f"PASS {self.config.password}")

        await self._send_raw(f"NICK {self.config.nickname}")
        user_line = f"USER {self.config.username} 0 * :{self.config.realname}".strip()
        await self._send_raw(user_line)

        for channel in self.config.channels:
            await self.join_channel(channel, announce=False)

    async def _session_loop(self) -> None:
        """Handle reader and input tasks for a single connection."""

        assert self.reader is not None
        assert self.writer is not None

        listener = asyncio.create_task(self._listen_to_server(), name="irc-listener")
        input_task = asyncio.create_task(self._read_user_input(), name="irc-stdin")

        tasks = {listener, input_task}
        done, pending = await asyncio.wait(tasks, return_when=asyncio.FIRST_COMPLETED)

        for task in pending:
            task.cancel()
        for task in pending:
            with contextlib.suppress(asyncio.CancelledError):
                await task

        for task in done:
            exc = task.exception()
            if exc and not isinstance(exc, ConnectionError):
                raise exc

    async def _listen_to_server(self) -> None:
        """Continuously read lines from the server until the connection closes."""

        assert self.reader is not None
        while not self.stop_requested.is_set():
            try:
                raw = await self.reader.readline()
            except asyncio.CancelledError:
                raise
            except Exception as exc:  # noqa: BLE001
                raise ConnectionError(str(exc)) from exc

            if not raw:
                raise ConnectionError("Server closed the connection")

            line = raw.decode(errors="ignore").strip("\r\n")
            self.logger.info("<-- %s", line)
            print(f"\r{line}")

            if line.startswith("PING"):
                payload = line.partition(":")[2] or line.split(" ", 1)[-1]
                await self._send_raw(f"PONG :{payload}")

    async def _read_user_input(self) -> None:
        """Read commands and messages from stdin asynchronously."""

        while not self.stop_requested.is_set():
            try:
                line = await asyncio.to_thread(sys.stdin.readline)
            except Exception as exc:  # noqa: BLE001
                raise ConnectionError(f"stdin error: {exc!s}") from exc

            if line == "":
                await self.quit("EOF on stdin")
                return

            line = line.rstrip("\n")
            if not line:
                continue

            if line.startswith("/"):
                await self._handle_command(line[1:])
            else:
                if not self.primary_target:
                    self._print_status("No default channel set. Use /join or /msg.")
                    continue
                await self.send_message(self.primary_target, line)

    async def _handle_command(self, command_line: str) -> None:
        """Parse and execute a slash command from the user."""

        if not command_line.strip():
            return

        try:
            parts = shlex.split(command_line)
        except ValueError as exc:
            self._print_status(f"Command parse error: {exc}")
            return

        if not parts:
            return

        command = parts[0].lower()
        args = parts[1:]

        match command:
            case "join":
                if not args:
                    self._print_status("Usage: /join #channel")
                    return
                await self.join_channel(args[0])
            case "leave" | "part":
                if not args:
                    self._print_status("Usage: /leave #channel")
                    return
                channel = self._normalize_channel_name(args[0])
                await self._send_raw(f"PART {channel}")
                if channel in self.config.channels:
                    self.config.channels.remove(channel)
                if self.primary_target == channel:
                    self.primary_target = self.config.channels[0] if self.config.channels else None
            case "msg":
                if len(args) < 2:
                    self._print_status("Usage: /msg <target> <message>")
                    return
                target, text = args[0], " ".join(args[1:])
                await self.send_message(target, text)
            case "me":
                if len(args) < 2:
                    self._print_status("Usage: /me <target> <action>")
                    return
                target, text = args[0], " ".join(args[1:])
                await self._send_raw(f"PRIVMSG {target} :\x01ACTION {text}\x01")
                self.logger.info("--> [me %s] %s", target, text)
            case "topic":
                if not args:
                    self._print_status("Usage: /topic <#channel> [new topic]")
                    return
                channel = args[0]
                if len(args) == 1:
                    await self._send_raw(f"TOPIC {channel}")
                else:
                    await self._send_raw(f"TOPIC {channel} :{' '.join(args[1:])}")
            case "names":
                if not args:
                    self._print_status("Usage: /names <#channel>")
                    return
                await self._send_raw(f"NAMES {args[0]}")
            case "whois":
                if not args:
                    self._print_status("Usage: /whois <nick>")
                    return
                await self._send_raw(f"WHOIS {args[0]}")
            case "raw":
                if not args:
                    self._print_status("Usage: /raw <command>")
                    return
                await self._send_raw(" ".join(args))
            case "quit":
                message = " ".join(args) if args else "Client exiting"
                await self.quit(message)
            case _:
                self._print_status(f"Unknown command: {command}")

    async def send_message(self, target: str, text: str) -> None:
        """Send a PRIVMSG to the target."""

        await self._send_raw(f"PRIVMSG {target} :{text}")
        self.logger.info("--> [%s] %s", target, text)

    async def join_channel(self, channel: str, announce: bool = True) -> None:
        """Join an IRC channel, normalizing the name."""

        normalized = self._normalize_channel_name(channel)
        if not normalized:
            return
        await self._send_raw(f"JOIN {normalized}")
        if announce:
            self._print_status(f"Joined {normalized}")
        if normalized not in self.config.channels:
            self.config.channels.append(normalized)
        self.primary_target = normalized

    async def quit(self, message: str) -> None:
        """Send QUIT and stop reconnecting."""

        self.stop_requested.set()
        try:
            await self._send_raw(f"QUIT :{message}")
        except ConnectionError:
            pass
        finally:
            await self._close_writer()

    async def _send_raw(self, line: str) -> None:
        """Send a raw line to the IRC server."""

        if not self.writer:
            raise ConnectionError("Not connected")
        data = f"{line}\r\n".encode()
        self.writer.write(data)
        try:
            await self.writer.drain()
        except Exception as exc:  # noqa: BLE001
            raise ConnectionError(str(exc)) from exc
        self.logger.info("--> %s", line)

    @staticmethod
    def _normalize_channel_name(channel: str) -> str:
        channel = channel.strip()
        if not channel:
            return channel
        if channel[0] in {"#", "&"}:
            return channel
        return f"#{channel}"

    async def _close_writer(self) -> None:
        writer = self.writer
        self.reader = None
        self.writer = None
        if writer is None:
            return
        writer.close()
        with contextlib.suppress(Exception):
            await writer.wait_closed()

    def _print_status(self, message: str) -> None:
        timestamp = datetime.now().strftime("%H:%M:%S")
        print(f"[{timestamp}] {message}")
        self.logger.info("*** %s", message)


def _parse_channels(raw: str) -> List[str]:
    if not raw:
        return []
    return [ch.strip() for ch in raw.split(",") if ch.strip()]


def _default_log_path(custom: Optional[str]) -> Path:
    if custom:
        return Path(custom).expanduser().resolve()
    stamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    base_dir = Path(__file__).resolve().parent / "logs"
    base_dir.mkdir(parents=True, exist_ok=True)
    return base_dir / f"irc_{stamp}.log"


def build_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Async terminal IRC client with optional TLS and reconnection",
    )
    parser.add_argument("--server", required=True, help="IRC server hostname or IP")
    parser.add_argument("--port", type=int, help="IRC server port", default=6667)
    parser.add_argument("-n", "--nickname", required=True, help="Nickname to use")
    parser.add_argument("--username", help="Username (defaults to nickname)")
    parser.add_argument("--realname", help="Real name/gecos (defaults to nickname)")
    parser.add_argument("--password", help="Server password or NickServ password")
    parser.add_argument(
        "--channels",
        help="Comma separated list of channels to join",
        default="",
    )
    tls_group = parser.add_mutually_exclusive_group()
    tls_group.add_argument("--tls", dest="tls", action="store_true", help="Enable TLS")
    tls_group.add_argument("--no-tls", dest="tls", action="store_false", help="Disable TLS")
    parser.set_defaults(tls=False)
    parser.add_argument(
        "--log-file",
        dest="log_file",
        help="Path to log file (default: logs/irc_<timestamp>.log)",
    )
    parser.add_argument(
        "--no-reconnect",
        dest="reconnect",
        action="store_false",
        help="Disable automatic reconnection attempts",
    )
    parser.set_defaults(reconnect=True)
    parser.add_argument(
        "--max-retries",
        type=int,
        help="Maximum reconnect attempts (default: unlimited)",
    )
    parser.add_argument(
        "--retry-delay",
        type=float,
        default=3.0,
        help="Initial reconnect delay in seconds",
    )
    return parser


def configure_logging(log_path: Path) -> logging.Logger:
    logger = logging.getLogger("irc_client")
    logger.setLevel(logging.INFO)
    logger.handlers.clear()

    file_handler = logging.FileHandler(log_path, encoding="utf-8")
    file_handler.setFormatter(
        logging.Formatter("%(asctime)s %(message)s", datefmt="%Y-%m-%d %H:%M:%S")
    )
    logger.addHandler(file_handler)
    return logger


async def _run_client(args: argparse.Namespace) -> None:
    log_path = _default_log_path(args.log_file)
    logger = configure_logging(log_path)
    channels = _parse_channels(args.channels)

    config = IRCConfig(
        server=args.server,
        port=args.port,
        nickname=args.nickname,
        username=args.username or args.nickname,
        realname=args.realname or args.nickname,
        password=args.password,
        channels=channels,
        use_tls=args.tls,
        log_file=log_path,
        reconnect=args.reconnect,
        max_retries=args.max_retries,
        retry_delay=args.retry_delay,
    )

    client = IRCClient(config=config, logger=logger)

    loop = asyncio.get_running_loop()

    def _handle_stop() -> None:
        loop.create_task(client.quit("Interrupted"))

    for sig in (signal.SIGINT, signal.SIGTERM):
        with contextlib.suppress(NotImplementedError):
            loop.add_signal_handler(sig, _handle_stop)

    try:
        await client.run()
    finally:
        for sig in (signal.SIGINT, signal.SIGTERM):
            with contextlib.suppress(NotImplementedError):
                loop.remove_signal_handler(sig)
        await client._close_writer()
        for handler in list(client.logger.handlers):
            handler.close()
            client.logger.removeHandler(handler)


def main(argv: Optional[Sequence[str]] = None) -> int:
    parser = build_argument_parser()
    args = parser.parse_args(argv)

    try:
        asyncio.run(_run_client(args))
    except KeyboardInterrupt:
        return 130
    except Exception as exc:  # noqa: BLE001
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
