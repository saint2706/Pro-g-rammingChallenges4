"""Integration tests for the TFTP client/server pair."""

from __future__ import annotations

import os
from pathlib import Path
import sys

sys.path.append(str(Path(__file__).resolve().parents[1]))

import pytest

from tftp import TFTPClient, TFTPServer


@pytest.fixture()
def server_root(tmp_path: Path) -> Path:
    root = tmp_path / "server"
    root.mkdir()
    return root


@pytest.fixture()
def client_dir(tmp_path: Path) -> Path:
    dest = tmp_path / "client"
    dest.mkdir()
    return dest


def test_download_roundtrip(server_root: Path, client_dir: Path) -> None:
    original = server_root / "hello.txt"
    original.write_text("Hello over TFTP!\n" * 8)

    with TFTPServer(
        server_root, host="127.0.0.1", port=0, timeout=1.0, retries=5
    ) as server:
        client = TFTPClient(
            "127.0.0.1", port=server.server_port, timeout=1.0, retries=5
        )
        destination = client_dir / "hello_copy.txt"
        client.download("hello.txt", destination, block_size=1024)
        assert destination.read_bytes() == original.read_bytes()


def test_upload_with_blocksize_negotiation(server_root: Path, client_dir: Path) -> None:
    payload = ("RFC1350" * 700).encode("ascii")
    local_file = client_dir / "upload.bin"
    local_file.write_bytes(payload)

    with TFTPServer(
        server_root, host="127.0.0.1", port=0, timeout=1.0, retries=5
    ) as server:
        client = TFTPClient(
            "127.0.0.1", port=server.server_port, timeout=1.0, retries=5
        )
        client.upload(local_file, "remote.bin", block_size=2048)

    remote = server_root / "remote.bin"
    assert remote.read_bytes() == payload


def test_upload_then_download(server_root: Path, client_dir: Path) -> None:
    local_file = client_dir / "data.bin"
    local_file.write_bytes(os.urandom(4096))

    with TFTPServer(
        server_root, host="127.0.0.1", port=0, timeout=1.0, retries=5
    ) as server:
        client = TFTPClient(
            "127.0.0.1", port=server.server_port, timeout=1.0, retries=5
        )
        client.upload(local_file, "data.bin", block_size=512)
        downloaded = client_dir / "downloaded.bin"
        client.download("data.bin", downloaded, block_size=512)

    assert downloaded.read_bytes() == local_file.read_bytes()
