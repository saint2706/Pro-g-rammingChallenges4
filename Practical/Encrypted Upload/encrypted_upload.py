"""AES-GCM encrypted upload CLI supporting S3 and generic HTTP targets."""
from __future__ import annotations

import argparse
import base64
import hashlib
import hmac
import json
import os
from dataclasses import dataclass
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, Optional, Tuple

import requests
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.ciphers.aead import AESGCM
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC

try:  # boto3 is optional until an S3 endpoint is used
    import boto3
except ImportError:  # pragma: no cover - boto3 always available in tests
    boto3 = None

MAGIC = b"EU01"
DEFAULT_CONTENT_TYPE = "application/octet-stream"


class ConfigurationError(RuntimeError):
    """Raised when the configuration file is invalid."""


def load_config(config_path: Path) -> Dict[str, Any]:
    try:
        data = json.loads(config_path.read_text(encoding="utf-8"))
    except FileNotFoundError as exc:  # pragma: no cover - defensive
        raise ConfigurationError(f"Configuration file not found: {config_path}") from exc
    except json.JSONDecodeError as exc:
        raise ConfigurationError(f"Invalid JSON in configuration file: {config_path}") from exc

    if "endpoints" not in data or not isinstance(data["endpoints"], list):
        raise ConfigurationError("Configuration must contain an 'endpoints' list")
    return data


def find_endpoint(config: Dict[str, Any], name: str) -> Dict[str, Any]:
    for endpoint in config.get("endpoints", []):
        if endpoint.get("name") == name:
            return endpoint
    raise ConfigurationError(f"Endpoint '{name}' not found in configuration")


@dataclass
class EncryptionResult:
    payload: bytes
    nonce: bytes
    salt: bytes
    sha256: str
    filesize: int
    key_bits: int


def derive_key(password: Optional[str], key_hex: Optional[str]) -> Tuple[bytes, bytes]:
    if password and key_hex:
        raise ValueError("Provide either a password or a hex key, not both")
    if password:
        salt = os.urandom(16)
        kdf = PBKDF2HMAC(
            algorithm=hashes.SHA256(),
            length=32,
            salt=salt,
            iterations=200_000,
        )
        key = kdf.derive(password.encode("utf-8"))
        return key, salt
    if key_hex:
        try:
            key = bytes.fromhex(key_hex)
        except ValueError as exc:
            raise ValueError("Key must be a hexadecimal string") from exc
        if len(key) not in (16, 24, 32):
            raise ValueError("Key must decode to 16, 24, or 32 bytes for AES")
        return key, b""
    raise ValueError("A password or hex key is required")


def encrypt_file(path: Path, key: bytes, salt: bytes, associated_data: bytes) -> EncryptionResult:
    data = path.read_bytes()
    nonce = os.urandom(12)
    aesgcm = AESGCM(key)
    ciphertext = aesgcm.encrypt(nonce, data, associated_data)

    if len(salt) > 255:
        raise ValueError("Salt length cannot exceed 255 bytes")
    if len(nonce) > 255:
        raise ValueError("Nonce length cannot exceed 255 bytes")

    header = MAGIC + bytes((len(salt), len(nonce)))
    payload = header + salt + nonce + ciphertext
    sha256 = hashlib.sha256(payload).hexdigest()
    return EncryptionResult(
        payload=payload,
        nonce=nonce,
        salt=salt,
        sha256=sha256,
        filesize=len(data),
        key_bits=len(key) * 8,
    )


def parse_payload(payload: bytes) -> Tuple[bytes, bytes, bytes]:
    if not payload.startswith(MAGIC):
        raise ValueError("Payload missing magic header")
    if len(payload) < 6:
        raise ValueError("Payload too short")
    salt_len = payload[4]
    nonce_len = payload[5]
    expected_len = 6 + salt_len + nonce_len
    if len(payload) < expected_len:
        raise ValueError("Payload truncated")
    salt = payload[6 : 6 + salt_len]
    nonce = payload[6 + salt_len : 6 + salt_len + nonce_len]
    ciphertext = payload[6 + salt_len + nonce_len :]
    return salt, nonce, ciphertext


def decrypt_payload(payload: bytes, key: bytes, associated_data: bytes) -> bytes:
    salt, nonce, ciphertext = parse_payload(payload)
    aesgcm = AESGCM(key)
    return aesgcm.decrypt(nonce, ciphertext, associated_data)


def ensure_boto3():
    if boto3 is None:
        raise RuntimeError("boto3 is required for S3 uploads but is not installed")
    return boto3


def generate_object_key(filename: str, prefix: str = "") -> str:
    safe_name = filename.replace("/", "_").replace(" ", "_")
    if prefix and not prefix.endswith("/"):
        prefix = f"{prefix}/"
    timestamp = datetime.now(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    return f"{prefix}{timestamp}_{safe_name}.enc"


def upload_to_s3(payload: bytes, endpoint: Dict[str, Any], object_key: str, content_type: str) -> str:
    boto3_mod = ensure_boto3()
    region = endpoint.get("region")
    bucket = endpoint.get("bucket")
    if not bucket or not region:
        raise ConfigurationError("S3 endpoint requires 'bucket' and 'region'")

    profile = endpoint.get("profile")
    session = boto3_mod.Session(profile_name=profile) if profile else boto3_mod.Session()
    client = session.client("s3", region_name=region)
    client.put_object(Bucket=bucket, Key=object_key, Body=payload, ContentType=content_type)

    expires_in = int(endpoint.get("expires_in", 3600))
    return client.generate_presigned_url(
        "get_object",
        Params={"Bucket": bucket, "Key": object_key},
        ExpiresIn=expires_in,
    )


def upload_to_http(payload: bytes, endpoint: Dict[str, Any], object_key: str, content_type: str, filename: str) -> str:
    url = endpoint.get("url")
    if not url:
        raise ConfigurationError("HTTP endpoint requires a 'url'")
    method = endpoint.get("method", "POST").upper()
    headers = endpoint.get("headers", {}) or {}
    response_url_field = endpoint.get("response_url_field", "url")

    if method == "POST":
        field_name = endpoint.get("field_name", "file")
        data = {"object_key": object_key}
        files = {field_name: (filename + ".enc", payload, content_type)}
        response = requests.post(url, data=data, files=files, headers=headers, timeout=endpoint.get("timeout", 30))
    elif method == "PUT":
        headers = {**headers, "Content-Type": content_type, "X-Object-Key": object_key}
        response = requests.put(url, data=payload, headers=headers, timeout=endpoint.get("timeout", 30))
    else:
        raise ConfigurationError(f"Unsupported HTTP method '{method}'")

    response.raise_for_status()
    if response.headers.get("Content-Type", "").startswith("application/json"):
        json_body = response.json()
        if response_url_field not in json_body:
            raise RuntimeError(f"HTTP response missing '{response_url_field}' field")
        return json_body[response_url_field]
    # Fallback: accept Location header or the URL itself
    return response.headers.get("Location", url)


def build_manifest(
    result: EncryptionResult,
    endpoint_name: str,
    object_key: str,
    share_url: str,
    filename: str,
    associated_data: bytes,
    signing_key_hex: Optional[str],
) -> Dict[str, Any]:
    manifest: Dict[str, Any] = {
        "original_filename": filename,
        "filesize": result.filesize,
        "sha256": result.sha256,
        "encryption": {
            "algorithm": f"AES-{result.key_bits}-GCM",
            "nonce": base64.b64encode(result.nonce).decode("ascii"),
            "salt": base64.b64encode(result.salt).decode("ascii"),
            "associated_data": associated_data.decode("utf-8", errors="ignore"),
        },
        "upload": {
            "endpoint": endpoint_name,
            "object_key": object_key,
            "timestamp": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
            "link": share_url,
        },
    }
    if signing_key_hex:
        try:
            signing_key = bytes.fromhex(signing_key_hex)
        except ValueError as exc:
            raise ValueError("Signing key must be hexadecimal") from exc
        serialized = json.dumps(manifest, sort_keys=True).encode("utf-8")
        signature = hmac.new(signing_key, serialized, hashlib.sha256).digest()
        manifest["signature"] = {
            "algorithm": "HMAC-SHA256",
            "value": base64.b64encode(signature).decode("ascii"),
        }
    return manifest


def save_manifest(manifest: Dict[str, Any], manifest_path: Path) -> None:
    manifest_path.write_text(json.dumps(manifest, indent=2), encoding="utf-8")


def handle_upload(args: argparse.Namespace) -> int:
    file_path = Path(args.file)
    if not file_path.is_file():
        raise FileNotFoundError(f"Input file not found: {file_path}")

    config = load_config(Path(args.config))
    endpoint = find_endpoint(config, args.endpoint)

    key, salt = derive_key(args.password, args.key)
    associated_data = file_path.name.encode("utf-8")
    result = encrypt_file(file_path, key, salt, associated_data)

    object_key = generate_object_key(file_path.name, endpoint.get("key_prefix", ""))
    content_type = args.content_type or DEFAULT_CONTENT_TYPE

    if endpoint.get("type") == "s3":
        share_url = upload_to_s3(result.payload, endpoint, object_key, content_type)
    elif endpoint.get("type") == "http":
        share_url = upload_to_http(result.payload, endpoint, object_key, content_type, file_path.name)
    else:
        raise ConfigurationError(f"Unsupported endpoint type '{endpoint.get('type')}'")

    manifest = build_manifest(
        result=result,
        endpoint_name=endpoint["name"],
        object_key=object_key,
        share_url=share_url,
        filename=file_path.name,
        associated_data=associated_data,
        signing_key_hex=args.signing_key,
    )

    if args.manifest:
        save_manifest(manifest, Path(args.manifest))

    print(share_url)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description="Encrypt a file and upload it to a remote endpoint.")
    subparsers = parser.add_subparsers(dest="command")

    upload_parser = subparsers.add_parser("upload", help="Encrypt and upload a file")
    upload_parser.add_argument("--file", required=True, help="Path to the file to encrypt")
    upload_parser.add_argument("--config", required=True, help="Path to the endpoints configuration JSON")
    upload_parser.add_argument("--endpoint", required=True, help="Name of the endpoint to use")
    group = upload_parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--password", help="Password used to derive the AES key")
    group.add_argument("--key", help="Hex-encoded AES key (16/24/32 bytes)")
    upload_parser.add_argument("--manifest", help="Path to write the manifest JSON")
    upload_parser.add_argument("--signing-key", help="Hex-encoded HMAC key for signing the manifest")
    upload_parser.add_argument("--content-type", help="Override the uploaded content type")
    upload_parser.set_defaults(func=handle_upload)

    return parser


def main(argv: Optional[list[str]] = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    if not hasattr(args, "func"):
        parser.print_help()
        return 1
    try:
        return args.func(args)
    except Exception as exc:  # pragma: no cover - CLI protection
        parser.exit(status=1, message=f"error: {exc}\n")


if __name__ == "__main__":  # pragma: no cover
    raise SystemExit(main())
