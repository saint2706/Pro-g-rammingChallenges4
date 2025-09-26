"""Interactive CLI client for the music streaming server.

Features
--------
* Discovers servers on the local network via UDP broadcast.
* Fetches playlist/metadata from the HTTP API.
* Streams audio using a buffered pipe into ``ffplay`` (FFmpeg) for playback.
* Falls back to saving the streamed media to a temporary file if FFmpeg is absent.
"""
from __future__ import annotations

import argparse
import json
import shutil
import socket
import subprocess
import sys
import tempfile
import textwrap
import time
from collections import deque
from dataclasses import dataclass
from pathlib import Path
from typing import Deque, Dict, List, Optional

import requests

DISCOVERY_MESSAGE = b"MUSIC_STREAM_DISCOVERY"
DEFAULT_DISCOVERY_PORT = 9999
DEFAULT_HTTP_PORT = 8000
CHUNK_SIZE = 64 * 1024
DEFAULT_BUFFER_BYTES = 512 * 1024
API_PREFIX = "/api"


@dataclass
class ServerInfo:
    host: str
    port: int
    tracks: List[Dict[str, object]]

    @property
    def base_url(self) -> str:
        return f"http://{self.host}:{self.port}{API_PREFIX}"


class MusicStreamClient:
    def __init__(self, server: ServerInfo, buffer_bytes: int = DEFAULT_BUFFER_BYTES) -> None:
        self.server = server
        self.session = requests.Session()
        self.buffer_bytes = buffer_bytes
        self.playlist: List[Dict[str, object]] = []
        self.current: Optional[Dict[str, object]] = None

    # --- API helpers -------------------------------------------------
    def refresh_playlist(self) -> None:
        response = self.session.get(f"{self.server.base_url}/playlist", timeout=5)
        response.raise_for_status()
        payload = response.json()
        self.playlist = payload.get("tracks", [])
        self.current = payload.get("current")

    def select_track(self, track_id: str) -> Dict[str, object]:
        response = self.session.post(
            f"{self.server.base_url}/playlist/select",
            json={"id": track_id},
            timeout=5,
        )
        response.raise_for_status()
        selected = response.json().get("selected")
        self.current = selected
        return selected

    def next_track(self) -> Optional[Dict[str, object]]:
        response = self.session.post(f"{self.server.base_url}/playlist/next", timeout=5)
        response.raise_for_status()
        current = response.json().get("current")
        self.current = current
        return current

    def previous_track(self) -> Optional[Dict[str, object]]:
        response = self.session.post(f"{self.server.base_url}/playlist/previous", timeout=5)
        response.raise_for_status()
        current = response.json().get("current")
        self.current = current
        return current

    # --- Streaming ---------------------------------------------------
    def stream(self, track_id: str) -> None:
        """Stream the specified track and play it using FFmpeg (``ffplay``)."""
        stream_url = f"{self.server.base_url}/stream/{track_id}"
        print(f"Requesting {stream_url}")
        response = self.session.get(stream_url, stream=True, timeout=10)
        if response.status_code != 200:
            print(f"Error {response.status_code}: {response.text}")
            return

        filename = _parse_filename(response.headers.get("Content-Disposition", ""))
        meta_text = _format_track_metadata(response.headers, filename)
        print(meta_text)

        ffplay_path = shutil.which("ffplay")
        if ffplay_path:
            self._play_with_ffplay(response, ffplay_path)
        else:
            print("ffplay executable not found. Buffering to a temporary file instead.")
            self._buffer_to_tempfile(response, filename)

    def _play_with_ffplay(self, response: requests.Response, ffplay_path: str) -> None:
        buffer: Deque[bytes] = deque()
        buffered_bytes = 0
        started = False
        start_time = time.time()

        process = subprocess.Popen(
            [ffplay_path, "-autoexit", "-nodisp", "-loglevel", "error", "-"],
            stdin=subprocess.PIPE,
        )
        assert process.stdin is not None  # mypy hint

        try:
            for chunk in response.iter_content(chunk_size=CHUNK_SIZE):
                if not chunk:
                    continue
                buffer.append(chunk)
                buffered_bytes += len(chunk)
                if not started:
                    if buffered_bytes >= self.buffer_bytes:
                        started = True
                        elapsed = time.time() - start_time
                        print(f"Buffered {buffered_bytes/1024:.0f} KiB in {elapsed:.1f}s. Starting playback...")
                    else:
                        print(
                            f"Buffering {buffered_bytes/1024:.0f} KiB / {self.buffer_bytes/1024:.0f} KiB",
                            end="\r",
                            flush=True,
                        )
                        continue
                while buffer:
                    process.stdin.write(buffer.popleft())
        except KeyboardInterrupt:
            print("\nStopping playback...")
        finally:
            # Flush remaining buffer
            while buffer:
                try:
                    process.stdin.write(buffer.popleft())
                except BrokenPipeError:
                    break
            try:
                process.stdin.close()
            except BrokenPipeError:
                pass
            process.wait()
            response.close()

    def _buffer_to_tempfile(self, response: requests.Response, filename: Optional[str]) -> None:
        suffix = Path(filename).suffix if filename else ".bin"
        with tempfile.NamedTemporaryFile(delete=False, suffix=suffix) as handle:
            total = 0
            for chunk in response.iter_content(chunk_size=CHUNK_SIZE):
                if not chunk:
                    continue
                handle.write(chunk)
                total += len(chunk)
                print(f"Downloaded {total/1024:.0f} KiB", end="\r", flush=True)
            temp_path = Path(handle.name)
        print()
        print(
            textwrap.dedent(
                f"""
                Stream saved to {temp_path} ({total/1024:.1f} KiB).
                Install FFmpeg for instant playback, or open this file with your preferred media player.
                """
            ).strip()
        )


# ---------------------------------------------------------------------------
# Utility functions
# ---------------------------------------------------------------------------

def discover_servers(port: int, timeout: float) -> List[ServerInfo]:
    """Broadcasts a discovery packet and collects responses."""
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_BROADCAST, 1)
    sock.settimeout(timeout)
    responses: List[ServerInfo] = []

    try:
        sock.sendto(DISCOVERY_MESSAGE, ("<broadcast>", port))
        start = time.time()
        while True:
            remaining = timeout - (time.time() - start)
            if remaining <= 0:
                break
            sock.settimeout(remaining)
            try:
                payload, addr = sock.recvfrom(32_768)
            except socket.timeout:
                break
            try:
                data = json.loads(payload.decode("utf-8"))
            except json.JSONDecodeError:
                continue
            host = data.get("host") or addr[0]
            port = int(data.get("port", DEFAULT_HTTP_PORT))
            tracks = data.get("tracks", [])
            responses.append(ServerInfo(host=host, port=port, tracks=tracks))
    finally:
        sock.close()

    return responses


def _parse_filename(content_disposition: str) -> Optional[str]:
    if not content_disposition:
        return None
    parts = content_disposition.split(";")
    for part in parts:
        if "=" not in part:
            continue
        key, value = part.strip().split("=", 1)
        if key.lower() == "filename":
            return value.strip('"')
    return None


def _format_track_metadata(headers: Dict[str, str], filename: Optional[str]) -> str:
    title = headers.get("X-Track-Title")
    artist = headers.get("X-Track-Artist")
    album = headers.get("X-Track-Album")
    duration = headers.get("X-Track-Duration")
    pieces = []
    if title:
        pieces.append(f"Title: {title}")
    if artist:
        pieces.append(f"Artist: {artist}")
    if album:
        pieces.append(f"Album: {album}")
    if duration:
        try:
            seconds = float(duration)
            minutes = int(seconds // 60)
            secs = int(seconds % 60)
            pieces.append(f"Duration: {minutes:02d}:{secs:02d}")
        except (TypeError, ValueError):
            pieces.append(f"Duration: {duration} seconds")
    if filename:
        pieces.append(f"Source file: {filename}")
    return " | ".join(pieces) or "Streaming audio"


def _format_duration(duration_seconds: Optional[float]) -> str:
    if duration_seconds is None:
        return "??:??"
    minutes = int(duration_seconds // 60)
    seconds = int(duration_seconds % 60)
    return f"{minutes:02d}:{seconds:02d}"


def _print_playlist(playlist: List[Dict[str, object]]) -> None:
    if not playlist:
        print("Playlist is empty. Add audio files to the server's media directory and refresh.")
        return
    print("Available tracks:")
    for idx, track in enumerate(playlist, start=1):
        title = track.get("title") or track.get("filename")
        artist = track.get("artist")
        album = track.get("album")
        duration = _format_duration(track.get("duration_seconds"))
        extra = ""
        if artist:
            extra += f" by {artist}"
        if album:
            extra += f" ({album})"
        print(f"[{idx:02d}] {title}{extra} — {duration}")


def parse_arguments() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Music streaming client")
    parser.add_argument("--host", help="Skip discovery and connect to this host")
    parser.add_argument("--port", type=int, default=DEFAULT_HTTP_PORT, help="HTTP port of the server")
    parser.add_argument("--discovery-port", type=int, default=DEFAULT_DISCOVERY_PORT, help="UDP discovery port")
    parser.add_argument("--timeout", type=float, default=3.0, help="Discovery timeout in seconds")
    parser.add_argument("--buffer", type=int, default=DEFAULT_BUFFER_BYTES, help="Bytes to buffer before playback")
    return parser.parse_args()


def choose_server(args: argparse.Namespace) -> Optional[ServerInfo]:
    if args.host:
        server = ServerInfo(host=args.host, port=args.port, tracks=[])
        print(f"Connecting directly to {server.host}:{server.port}")
        return server

    print("Broadcasting discovery packet...")
    servers = discover_servers(args.discovery_port, args.timeout)
    if not servers:
        print("No servers discovered. Ensure the server is running and reachable on the network.")
        return None
    if len(servers) == 1:
        server = servers[0]
        print(f"Found server at {server.host}:{server.port}")
        return server

    print("Multiple servers discovered:")
    for index, server in enumerate(servers, start=1):
        print(f"  [{index}] {server.host}:{server.port} ({len(server.tracks)} tracks)")
    while True:
        selection = input("Select server number (or press Enter to cancel): ").strip()
        if not selection:
            return None
        try:
            choice = int(selection)
        except ValueError:
            print("Enter a numeric choice.")
            continue
        if 1 <= choice <= len(servers):
            return servers[choice - 1]
        print("Choice out of range.")


def interactive_loop(client: MusicStreamClient) -> None:
    client.refresh_playlist()
    _print_playlist(client.playlist)
    help_text = textwrap.dedent(
        """
        Commands:
          list              - show playlist
          play <n>          - select and stream track number n
          next / prev       - skip forward/backward using server playlist order
          refresh           - rescan playlist from server
          buffer <bytes>    - change local buffer threshold before playback
          quit              - exit the client
        """
    ).strip()
    print(help_text)

    while True:
        try:
            raw = input("music> ").strip()
        except (KeyboardInterrupt, EOFError):
            print()
            return
        if not raw:
            continue
        parts = raw.split()
        command = parts[0].lower()

        if command == "quit" or command == "exit":
            return
        if command == "help":
            print(help_text)
        elif command == "list":
            _print_playlist(client.playlist)
        elif command == "refresh":
            client.refresh_playlist()
            _print_playlist(client.playlist)
        elif command == "buffer" and len(parts) == 2:
            try:
                client.buffer_bytes = int(parts[1])
                print(f"Buffer threshold set to {client.buffer_bytes} bytes")
            except ValueError:
                print("Provide an integer number of bytes")
        elif command in {"next", "prev", "previous"}:
            next_track = client.next_track() if command == "next" else client.previous_track()
            if not next_track:
                print("Playlist is empty")
                continue
            print(f"Selected: {next_track.get('title') or next_track.get('filename')}")
            client.stream(next_track["id"])  # type: ignore[index]
        elif command == "play" and len(parts) == 2:
            try:
                index = int(parts[1]) - 1
            except ValueError:
                print("Provide a numeric track number")
                continue
            if not (0 <= index < len(client.playlist)):
                print("Track number out of range")
                continue
            track = client.playlist[index]
            client.select_track(track["id"])  # type: ignore[index]
            print(f"Playing {track.get('title') or track.get('filename')}")
            client.stream(track["id"])  # type: ignore[index]
        else:
            print("Unknown command. Type 'help' for options.")


def main() -> None:
    args = parse_arguments()
    server = choose_server(args)
    if not server:
        sys.exit(1)

    client = MusicStreamClient(server, buffer_bytes=args.buffer)
    try:
        interactive_loop(client)
    finally:
        client.session.close()


if __name__ == "__main__":
    main()
