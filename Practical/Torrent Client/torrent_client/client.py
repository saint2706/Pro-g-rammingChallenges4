"""High-level torrent client orchestration."""
from __future__ import annotations

import logging
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable, Optional

from .metainfo import TorrentMetaInfo
from .peer_protocol import PeerConnection, PieceManager
from .storage import FileStorage
from .tracker import Peer, TrackerClient

LOGGER = logging.getLogger(__name__)

ProgressCallback = Callable[[int, int], None]


@dataclass
class TorrentClient:
    metainfo: TorrentMetaInfo
    download_dir: Path
    resume: bool = True
    port: int = 6881
    peer_id: Optional[bytes] = None
    timeout: float = 10.0

    def __post_init__(self) -> None:
        self.download_dir = Path(self.download_dir)
        self.download_dir.mkdir(parents=True, exist_ok=True)
        target_path = self.download_dir / self.metainfo.name
        self.storage = FileStorage(
            path=target_path,
            total_size=self.metainfo.length,
            piece_length=self.metainfo.piece_length,
            info_hash=self.metainfo.info_hash,
            resume=self.resume,
        )
        self.tracker = TrackerClient(self.metainfo.announce, peer_id=self.peer_id)
        self.piece_manager = PieceManager.from_storage(
            self.metainfo.pieces, self.metainfo.piece_length, self.storage
        )

    def download(self, progress_callback: Optional[ProgressCallback] = None) -> None:
        if self.piece_manager.finished():
            LOGGER.info("All pieces already present; nothing to do")
            if progress_callback:
                progress_callback(self.piece_manager.completed_count, self.piece_manager.total_pieces)
            return

        left = self.metainfo.length - self.storage.bytes_completed
        downloaded = self.storage.bytes_completed
        response = self.tracker.announce(
            info_hash=self.metainfo.info_hash,
            port=self.port,
            downloaded=downloaded,
            left=left,
            event="started",
        )
        peers = response.peers
        if not peers:
            raise RuntimeError("Tracker returned no peers")
        LOGGER.info("Tracker returned %d peers", len(peers))
        self._download_from_peers(peers, progress_callback)
        if self.piece_manager.finished():
            LOGGER.info("Download complete")
            try:
                self.tracker.announce(
                    info_hash=self.metainfo.info_hash,
                    port=self.port,
                    downloaded=self.metainfo.length,
                    left=0,
                    event="completed",
                )
            except Exception as exc:  # pragma: no cover - tracker completion best effort
                LOGGER.debug("Failed to send completion announce: %s", exc)
        else:
            raise RuntimeError("Unable to complete download from available peers")

    def _download_from_peers(
        self, peers: Iterable[Peer], progress_callback: Optional[ProgressCallback]
    ) -> None:
        for peer in peers:
            if self.piece_manager.finished():
                return
            try:
                self._download_from_peer(peer, progress_callback)
            except Exception as exc:
                LOGGER.warning("Peer %s:%s failed: %s", peer.host, peer.port, exc)
                continue

    def _download_from_peer(self, peer: Peer, progress_callback: Optional[ProgressCallback]) -> None:
        LOGGER.info("Attempting peer %s:%s", peer.host, peer.port)
        with PeerConnection(
            peer.host,
            peer.port,
            info_hash=self.metainfo.info_hash,
            peer_id=self.tracker.peer_id,
            timeout=self.timeout,
        ) as connection:
            while not self.piece_manager.finished():
                work = self.piece_manager.next_request()
                if not work:
                    return
                try:
                    data = connection.download_piece(work)
                except Exception:
                    self.piece_manager.mark_failed(work.index)
                    raise
                expected_hash = self.metainfo.pieces[work.index]
                ok = self.storage.write_piece(work.index, data, expected_hash)
                if not ok:
                    LOGGER.warning("Piece %s failed hash check", work.index)
                    self.piece_manager.mark_failed(work.index)
                    continue
                self.piece_manager.mark_complete(work.index)
                if progress_callback:
                    progress_callback(
                        self.piece_manager.completed_count, self.piece_manager.total_pieces
                    )


def load_torrent(path: Path) -> TorrentMetaInfo:
    return TorrentMetaInfo.from_file(path)


__all__ = ["TorrentClient", "load_torrent"]
