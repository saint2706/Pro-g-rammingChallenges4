import hashlib
from pathlib import Path

import requests

from torrent_client.metainfo import bencode
from torrent_client.storage import FileStorage
from torrent_client.tracker import TrackerClient


def test_file_storage_resume(tmp_path: Path) -> None:
    data = b"hello world"
    piece_length = 4
    pieces = [hashlib.sha1(data[i : i + piece_length]).digest() for i in range(0, len(data), piece_length)]
    target = tmp_path / "hello.txt"
    storage = FileStorage(path=target, total_size=len(data), piece_length=piece_length, info_hash=b"hash" * 5)

    assert storage.bytes_completed == 0
    assert not storage.has_piece(0, pieces[0])

    assert storage.write_piece(0, data[0:4], pieces[0])
    assert storage.write_piece(1, data[4:8], pieces[1])
    assert storage.write_piece(2, data[8:], pieces[2])

    assert storage.bytes_completed == len(data)

    # Re-open with resume to ensure bitfield persisted
    storage2 = FileStorage(
        path=target,
        total_size=len(data),
        piece_length=piece_length,
        info_hash=b"hash" * 5,
    )
    assert storage2.bytes_completed == len(data)
    assert storage2.bitfield == [True, True, True]


def test_tracker_compact_response(monkeypatch) -> None:
    payload = {
        b"interval": 30,
        b"peers": b"".join([
            b"\x7f\x00\x00\x01" + (6881).to_bytes(2, "big"),
            b"\x7f\x00\x00\x02" + (51413).to_bytes(2, "big"),
        ]),
    }

    response = requests.Response()
    response.status_code = 200
    response._content = bencode(payload)

    def fake_get(*args, **kwargs):
        return response

    monkeypatch.setattr("torrent_client.tracker.requests.get", fake_get)

    client = TrackerClient("http://example.com/announce", peer_id=b"-PC4TEST-1234567890")
    result = client.announce(info_hash=b"abc" * 7, port=6881, downloaded=0, left=1)
    assert result.interval == 30
    assert len(result.peers) == 2
    assert (result.peers[0].host, result.peers[0].port) == ("127.0.0.1", 6881)
