import argparse
import base64
import hmac
import json
import threading
from http.server import BaseHTTPRequestHandler, HTTPServer
from pathlib import Path

import boto3
import pytest
import requests
from moto import mock_aws

MODULE_PATH = Path(__file__).resolve().parents[1] / "encrypted_upload.py"


def load_module():
    import importlib.util
    import sys

    spec = importlib.util.spec_from_file_location("encrypted_upload", MODULE_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def test_encrypt_roundtrip_password(tmp_path):
    module = load_module()
    secret = "classified".encode()
    sample_file = tmp_path / "sample.txt"
    sample_file.write_bytes(secret)

    key, salt = module.derive_key(password="correct horse", key_hex=None)
    result = module.encrypt_file(sample_file, key, salt, b"sample.txt")

    recovered = module.decrypt_payload(result.payload, key, b"sample.txt")
    assert recovered == secret
    assert result.sha256 == module.hashlib.sha256(result.payload).hexdigest()


class MultipartCaptureHandler(BaseHTTPRequestHandler):
    stored_payload = None
    stored_key = None

    def do_POST(self):  # noqa: N802 - http handler naming
        import cgi

        form = cgi.FieldStorage(
            fp=self.rfile,
            headers=self.headers,
            environ={
                "REQUEST_METHOD": "POST",
                "CONTENT_TYPE": self.headers.get("Content-Type"),
            },
        )
        file_item = form["file"]
        self.__class__.stored_payload = file_item.file.read()
        self.__class__.stored_key = form["object_key"].value
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps({"url": "https://example.test/download"}).encode())

    def log_message(self, *_):  # silence noisy handler
        return


@pytest.fixture
def http_server():
    server = HTTPServer(("127.0.0.1", 0), MultipartCaptureHandler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield server
    finally:
        server.shutdown()
        thread.join()


def test_http_upload_flow(tmp_path, http_server, monkeypatch):
    module = load_module()
    MultipartCaptureHandler.stored_payload = None
    MultipartCaptureHandler.stored_key = None
    real_post = requests.post

    # Avoid making real HTTP requests by forcing requests to talk to our server
    def fake_post(url, data, files, headers, timeout):
        response = real_post(
            f"http://127.0.0.1:{http_server.server_address[1]}/",
            data=data,
            files=files,
            headers=headers,
            timeout=timeout,
        )
        return response

    monkeypatch.setattr(module.requests, "post", fake_post)

    sample_file = tmp_path / "document.txt"
    sample_file.write_text("payload")

    config = {
        "endpoints": [
            {
                "name": "local-http",
                "type": "http",
                "url": "http://example.invalid/upload",
                "method": "POST",
                "response_url_field": "url",
            }
        ]
    }
    config_path = tmp_path / "config.json"
    config_path.write_text(json.dumps(config))
    manifest_path = tmp_path / "manifest.json"

    args = argparse.Namespace(
        file=str(sample_file),
        config=str(config_path),
        endpoint="local-http",
        password=None,
        key="0f" * 32,
        manifest=str(manifest_path),
        signing_key=None,
        content_type=None,
    )

    assert module.handle_upload(args) == 0
    assert MultipartCaptureHandler.stored_payload is not None
    salt, nonce, ciphertext = module.parse_payload(MultipartCaptureHandler.stored_payload)
    assert len(nonce) == 12
    manifest = json.loads(manifest_path.read_text())
    assert base64.b64encode(salt).decode() == manifest["encryption"]["salt"]
    assert MultipartCaptureHandler.stored_key == manifest["upload"]["object_key"]


@mock_aws
def test_s3_upload_and_manifest(tmp_path):
    module = load_module()
    sample_file = tmp_path / "topsecret.txt"
    sample_file.write_text("classified")

    client = boto3.client("s3", region_name="us-east-1")
    client.create_bucket(Bucket="test-bucket")

    config = {
        "endpoints": [
            {
                "name": "secure-s3",
                "type": "s3",
                "bucket": "test-bucket",
                "region": "us-east-1",
                "key_prefix": "uploads/",
                "expires_in": 600,
            }
        ]
    }
    config_path = tmp_path / "config.json"
    config_path.write_text(json.dumps(config))
    manifest_path = tmp_path / "manifest.json"

    args = argparse.Namespace(
        file=str(sample_file),
        config=str(config_path),
        endpoint="secure-s3",
        password="swordfish",
        key=None,
        manifest=str(manifest_path),
        signing_key="aa" * 32,
        content_type=None,
    )

    assert module.handle_upload(args) == 0

    manifest = json.loads(manifest_path.read_text())
    assert manifest["upload"]["endpoint"] == "secure-s3"
    assert manifest["encryption"]["algorithm"].startswith("AES-")

    # Validate signature
    signature = manifest.pop("signature")
    serialized = json.dumps(manifest, sort_keys=True).encode()
    expected_sig = hmac.new(bytes.fromhex(args.signing_key), serialized, module.hashlib.sha256).digest()
    assert base64.b64encode(expected_sig).decode() == signature["value"]

    obj = client.get_object(Bucket="test-bucket", Key=manifest["upload"]["object_key"])
    stored_payload = obj["Body"].read()
    assert stored_payload.startswith(module.MAGIC)
