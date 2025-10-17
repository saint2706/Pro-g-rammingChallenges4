import hashlib
from pathlib import Path

from torrent_client.client import TorrentClient
from torrent_client.metainfo import TorrentMetaInfo, bencode
from torrent_client.tracker import Peer, TrackerResponse


def create_torrent(tmp_path: Path) -> Path:
    data = b"hello world"
    piece_length = 4
    pieces = [
        hashlib.sha1(data[i : i + piece_length]).digest()
        for i in range(0, len(data), piece_length)
    ]
    info = {
        b"piece length": piece_length,
        b"pieces": b"".join(pieces),
        b"name": b"hello.txt",
        b"length": len(data),
    }
    meta = {b"announce": b"http://tracker.example/announce", b"info": info}
    path = tmp_path / "hello.torrent"
    path.write_bytes(bencode(meta))
    return path, data


class FakePeerConnection:
    def __init__(self, host, port, info_hash, peer_id, timeout=10.0):
        self.host = host
        self.port = port
        self.info_hash = info_hash
        self.peer_id = peer_id
        self.timeout = timeout
        self.data = None

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc, tb):
        return False

    def download_piece(self, work):
        assert self.data is not None
        return self.data[work.index]


def test_torrent_client_download(monkeypatch, tmp_path: Path) -> None:
    torrent_path, data = create_torrent(tmp_path)
    metainfo = TorrentMetaInfo.from_file(torrent_path)
    target_dir = tmp_path / "out"
    target_dir.mkdir()

    pieces = [
        data[i : i + metainfo.piece_length]
        for i in range(0, len(data), metainfo.piece_length)
    ]

    def fake_announce(self, **kwargs):
        return TrackerResponse(interval=1, peers=[Peer("127.0.0.1", 6881)])

    def connection_factory(host, port, info_hash, peer_id, timeout=10.0):
        conn = FakePeerConnection(host, port, info_hash, peer_id, timeout)
        conn.data = pieces
        return conn

    monkeypatch.setattr("torrent_client.client.TrackerClient.announce", fake_announce)
    monkeypatch.setattr("torrent_client.client.PeerConnection", connection_factory)

    client = TorrentClient(metainfo=metainfo, download_dir=target_dir, resume=True)
    client.download()

    output_file = target_dir / metainfo.name
    assert output_file.read_bytes() == data
