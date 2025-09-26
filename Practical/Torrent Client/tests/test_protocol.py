import hashlib
from pathlib import Path

from torrent_client.metainfo import bencode, TorrentMetaInfo
from torrent_client.peer_protocol import build_handshake, parse_handshake


def create_test_torrent(tmp_path: Path) -> Path:
    data = b"hello world"
    piece_length = 4
    pieces = [hashlib.sha1(data[i : i + piece_length]).digest() for i in range(0, len(data), piece_length)]
    info = {
        b"piece length": piece_length,
        b"pieces": b"".join(pieces),
        b"name": b"hello.txt",
        b"length": len(data),
    }
    meta = {b"announce": b"http://tracker.example/announce", b"info": info}
    path = tmp_path / "hello.torrent"
    path.write_bytes(bencode(meta))
    return path


def test_handshake_roundtrip(tmp_path: Path) -> None:
    torrent_path = create_test_torrent(tmp_path)
    metainfo = TorrentMetaInfo.from_file(torrent_path)
    peer_id = b"-PC4TEST-1234567890!"
    handshake = build_handshake(metainfo.info_hash, peer_id)
    parsed_hash, parsed_peer = parse_handshake(handshake)
    assert parsed_hash == metainfo.info_hash
    assert parsed_peer == peer_id
