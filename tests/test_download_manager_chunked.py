import importlib.util
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

import pytest


MODULE_PATH = Path(__file__).resolve().parents[1] / "Practical" / "Download Manager" / "dManager.py"


@pytest.fixture(scope="module")
def dmanager():
    import sys
    import types

    if "tqdm" not in sys.modules:
        tqdm_module = types.ModuleType("tqdm")

        class _SilentTqdm:
            def __init__(self, *args, **kwargs):
                pass

            def __enter__(self):
                return self

            def __exit__(self, exc_type, exc, tb):
                return False

            def update(self, *_args, **_kwargs):
                pass

        tqdm_module.tqdm = _SilentTqdm
        sys.modules["tqdm"] = tqdm_module

    spec = importlib.util.spec_from_file_location("dmanager", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules.setdefault(spec.name, module)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


@pytest.fixture
def chunked_server():
    data = b"chunked-response-data" * 5

    class Handler(BaseHTTPRequestHandler):
        def do_HEAD(self):  # noqa: N802 (capitalization by protocol)
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.end_headers()

        def do_GET(self):  # noqa: N802
            self.send_response(200)
            self.send_header("Content-Type", "application/octet-stream")
            self.send_header("Transfer-Encoding", "chunked")
            self.end_headers()
            chunk_size = 7
            for idx in range(0, len(data), chunk_size):
                chunk = data[idx : idx + chunk_size]
                self.wfile.write(f"{len(chunk):X}\r\n".encode("ascii"))
                self.wfile.write(chunk + b"\r\n")
            self.wfile.write(b"0\r\n\r\n")
            self.close_connection = True

        def log_message(self, format, *args):  # noqa: A003
            return

    server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield f"http://127.0.0.1:{server.server_address[1]}/file", data
    finally:
        server.shutdown()
        thread.join()


def test_chunked_download_records_size_and_bytes(dmanager, chunked_server, tmp_path):
    url, expected = chunked_server
    output = tmp_path / "chunked.bin"
    cfg = dmanager.Config(
        url=url,
        threads=4,
        output=output,
        retries=1,
        timeout=5.0,
        backoff=0.1,
        resume=False,
        chunk_size=4096,
        expected_sha256=None,
        json_path=None,
        verify_tls=True,
        user_agent=dmanager.DEFAULT_UA,
    )

    result = dmanager.download(cfg)

    assert result.error is None
    assert result.size == -1
    assert result.threads == 1
    assert result.downloaded == len(expected)
    assert output.read_bytes() == expected
