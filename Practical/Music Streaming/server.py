"""Streaming audio server with playlist management and LAN discovery.

Usage::
    python server.py --media ./media --host 0.0.0.0 --port 8000

The server exposes a JSON API for playlist management and streams audio
content using chunked HTTP responses suitable for large media files.
"""

from __future__ import annotations

import argparse
import atexit
import json
import mimetypes
import socket
import threading
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional

from flask import Flask, Response, abort, jsonify, request
from mutagen import File as MutagenFile

CHUNK_SIZE = 64 * 1024
SUPPORTED_EXTENSIONS = {".mp3", ".flac", ".wav", ".ogg", ".m4a", ".aac"}


@dataclass
class Track:
    """Represents a single audio asset on disk."""

    id: str
    path: Path
    title: str
    artist: Optional[str]
    album: Optional[str]
    duration_seconds: Optional[float]

    @property
    def filename(self) -> str:
        return self.path.name

    def to_dict(self) -> Dict[str, Optional[str]]:
        payload = asdict(self)
        payload["filename"] = self.filename
        # Convert Path to string for JSON serialisation
        payload["path"] = str(self.path)
        return payload


class PlaylistManager:
    """Thread-safe access to the playlist and currently selected track."""

    def __init__(self, media_root: Path) -> None:
        self.media_root = media_root
        self._lock = threading.RLock()
        self._tracks: List[Track] = []
        self._current_index: int = 0
        self.refresh()

    def refresh(self) -> None:
        """Rescan the media directory for playable files."""
        with self._lock:
            candidates = sorted(
                (
                    path
                    for ext in SUPPORTED_EXTENSIONS
                    for path in self.media_root.rglob(f"*{ext}")
                ),
                key=lambda p: (p.parent.name.lower(), p.name.lower()),
            )
            tracks: List[Track] = []
            for idx, path in enumerate(candidates):
                metadata = self._extract_metadata(path)
                tracks.append(
                    Track(
                        id=f"track-{idx}",
                        path=path,
                        title=metadata.get("title") or path.stem,
                        artist=metadata.get("artist"),
                        album=metadata.get("album"),
                        duration_seconds=metadata.get("duration"),
                    )
                )
            self._tracks = tracks
            if self._current_index >= len(self._tracks):
                self._current_index = 0 if self._tracks else -1

    def _extract_metadata(self, path: Path) -> Dict[str, Optional[str]]:
        data: Dict[str, Optional[str]] = {
            "title": None,
            "artist": None,
            "album": None,
            "duration": None,
        }
        try:
            audio = MutagenFile(path)
        except Exception:
            return data
        if not audio:
            return data
        info = getattr(audio, "info", None)
        if info and getattr(info, "length", None):
            data["duration"] = float(info.length)
        tags = getattr(audio, "tags", {}) or {}
        for key in ("TIT2", "TITLE"):
            if key in tags:
                data["title"] = self._read_mutagen_tag(tags[key])
                break
        for key in ("TPE1", "ARTIST"):
            if key in tags:
                data["artist"] = self._read_mutagen_tag(tags[key])
                break
        for key in ("TALB", "ALBUM"):
            if key in tags:
                data["album"] = self._read_mutagen_tag(tags[key])
                break
        return data

    @staticmethod
    def _read_mutagen_tag(tag: object) -> Optional[str]:
        try:
            if hasattr(tag, "text"):
                values = tag.text
                if isinstance(values, (list, tuple)):
                    return str(values[0]) if values else None
                return str(values)
            return str(tag)
        except Exception:
            return None

    def all_tracks(self) -> List[Track]:
        with self._lock:
            return list(self._tracks)

    def current_track(self) -> Optional[Track]:
        with self._lock:
            if 0 <= self._current_index < len(self._tracks):
                return self._tracks[self._current_index]
            return None

    def select(self, track_id: str) -> Track:
        with self._lock:
            for idx, track in enumerate(self._tracks):
                if track.id == track_id:
                    self._current_index = idx
                    return track
        raise KeyError(f"Unknown track id: {track_id}")

    def next(self) -> Optional[Track]:
        with self._lock:
            if not self._tracks:
                return None
            self._current_index = (self._current_index + 1) % len(self._tracks)
            return self._tracks[self._current_index]

    def previous(self) -> Optional[Track]:
        with self._lock:
            if not self._tracks:
                return None
            self._current_index = (self._current_index - 1) % len(self._tracks)
            return self._tracks[self._current_index]

    def get(self, track_id: str) -> Track:
        with self._lock:
            for track in self._tracks:
                if track.id == track_id:
                    return track
        raise KeyError(f"Unknown track id: {track_id}")


class DiscoveryResponder:
    """Responds to LAN discovery pings via UDP."""

    def __init__(
        self,
        playlist: PlaylistManager,
        http_host: str,
        http_port: int,
        discovery_port: int,
        advertised_host: Optional[str] = None,
    ) -> None:
        self.playlist = playlist
        self.http_host = http_host
        self.http_port = http_port
        self.discovery_port = discovery_port
        self.advertised_host = advertised_host or self._resolve_host(http_host)
        self._thread: Optional[threading.Thread] = None
        self._stop = threading.Event()

    @staticmethod
    def _resolve_host(host: str) -> str:
        if host and host != "0.0.0.0":
            return host
        probe = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        try:
            probe.connect(("8.8.8.8", 80))
            resolved = probe.getsockname()[0]
        except OSError:
            resolved = "127.0.0.1"
        finally:
            probe.close()
        return resolved

    def start(self) -> None:
        if self._thread and self._thread.is_alive():
            return
        self._thread = threading.Thread(
            target=self._serve, name="discovery", daemon=True
        )
        self._thread.start()

    def stop(self) -> None:
        self._stop.set()
        # poke the socket to unblock
        try:
            poke = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
            poke.sendto(b"", ("127.0.0.1", self.discovery_port))
            poke.close()
        except OSError:
            pass
        if self._thread and self._thread.is_alive():
            self._thread.join(timeout=1.0)

    def _serve(self) -> None:
        sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        sock.bind(("", self.discovery_port))
        with sock:
            while not self._stop.is_set():
                try:
                    data, addr = sock.recvfrom(1024)
                except OSError:
                    break
                if self._stop.is_set():
                    break
                if not data:
                    continue
                if data.strip().upper() != b"MUSIC_STREAM_DISCOVERY":
                    continue
                payload = {
                    "host": self.advertised_host,
                    "port": self.http_port,
                    "tracks": [track.to_dict() for track in self.playlist.all_tracks()],
                }
                try:
                    sock.sendto(json.dumps(payload).encode("utf-8"), addr)
                except OSError:
                    continue


def create_app(playlist: PlaylistManager) -> Flask:
    app = Flask(__name__)

    @app.get("/api/health")
    def health() -> Dict[str, str]:
        return {"status": "ok"}

    @app.get("/api/playlist")
    def get_playlist() -> Response:
        tracks = [track.to_dict() for track in playlist.all_tracks()]
        current = playlist.current_track()
        payload = {
            "tracks": tracks,
            "current": current.to_dict() if current else None,
        }
        return jsonify(payload)

    @app.post("/api/playlist/refresh")
    def refresh_playlist() -> Response:
        playlist.refresh()
        return jsonify({"ok": True, "count": len(playlist.all_tracks())})

    @app.post("/api/playlist/select")
    def select_track() -> Response:
        data = request.get_json(force=True, silent=True) or {}
        track_id = data.get("id")
        if not track_id:
            abort(400, "Missing 'id' in payload")
        try:
            track = playlist.select(track_id)
        except KeyError:
            abort(404, f"Unknown track id: {track_id}")
        return jsonify({"selected": track.to_dict()})

    @app.post("/api/playlist/next")
    def next_track() -> Response:
        track = playlist.next()
        return jsonify({"current": track.to_dict() if track else None})

    @app.post("/api/playlist/previous")
    def previous_track() -> Response:
        track = playlist.previous()
        return jsonify({"current": track.to_dict() if track else None})

    @app.get("/api/stream/<track_id>")
    def stream(track_id: str):
        try:
            track = playlist.get(track_id)
        except KeyError:
            abort(404, "Track not found")

        if not track.path.exists():
            abort(404, "File missing")

        mimetype, _ = mimetypes.guess_type(str(track.path))
        mimetype = mimetype or "application/octet-stream"

        def generate() -> Iterable[bytes]:
            with track.path.open("rb") as handle:
                while True:
                    chunk = handle.read(CHUNK_SIZE)
                    if not chunk:
                        break
                    yield chunk

        headers = {
            "Content-Disposition": f"inline; filename={track.filename}",
            "X-Track-Title": track.title or track.filename,
        }
        if track.artist:
            headers["X-Track-Artist"] = track.artist
        if track.album:
            headers["X-Track-Album"] = track.album
        if track.duration_seconds:
            headers["X-Track-Duration"] = str(track.duration_seconds)

        return Response(generate(), mimetype=mimetype, headers=headers)

    return app


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Audio streaming server")
    parser.add_argument(
        "--media",
        type=Path,
        default=Path("media"),
        help="Directory containing audio files",
    )
    parser.add_argument(
        "--host", default="0.0.0.0", help="Host/interface to bind the HTTP server"
    )
    parser.add_argument(
        "--port", type=int, default=8000, help="Port for the HTTP server"
    )
    parser.add_argument(
        "--discovery-port", type=int, default=9999, help="UDP discovery port"
    )
    parser.add_argument("--debug", action="store_true", help="Enable Flask debug mode")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    media = args.media
    media.mkdir(parents=True, exist_ok=True)

    playlist = PlaylistManager(media)
    app = create_app(playlist)

    discovery = DiscoveryResponder(playlist, args.host, args.port, args.discovery_port)
    discovery.start()
    atexit.register(discovery.stop)

    print(
        f"Audio streaming server ready on http://{discovery.advertised_host}:{args.port}"
    )
    print(f"Serving media from: {media.resolve()}")
    if not playlist.all_tracks():
        print(
            "No audio tracks found yet. Drop MP3/FLAC/WAV files into the media directory."
        )

    app.run(host=args.host, port=args.port, debug=args.debug, threaded=True)


if __name__ == "__main__":
    main()
