"""Minimal TFTP client/server implementation with block-size negotiation."""

from __future__ import annotations

import logging
import socket
import struct
import threading
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Optional, Tuple

LOGGER = logging.getLogger(__name__)

# Opcodes
OP_RRQ = 1
OP_WRQ = 2
OP_DATA = 3
OP_ACK = 4
OP_ERROR = 5
OP_OACK = 6

# Error codes (subset)
ERR_NOT_DEFINED = 0
ERR_FILE_NOT_FOUND = 1
ERR_ACCESS_VIOLATION = 2
ERR_DISK_FULL = 3
ERR_ILLEGAL_OPERATION = 4
ERR_UNKNOWN_TID = 5
ERR_FILE_EXISTS = 6


class TFTPError(Exception):
    """Exception raised for protocol errors."""

    def __init__(self, code: int, message: str) -> None:
        super().__init__(message)
        self.code = code
        self.message = message


@dataclass
class TransferOptions:
    """Negotiated transfer parameters."""

    block_size: int = 512
    requested_blksize: Optional[int] = None

    @classmethod
    def from_request(cls, options: Dict[str, str]) -> "TransferOptions":
        block_size = 512
        requested: Optional[int] = None
        if "blksize" in options:
            try:
                requested = int(options["blksize"])
            except (TypeError, ValueError):
                LOGGER.debug("Invalid blksize %r", options["blksize"])
            else:
                if 8 <= requested <= 65464:
                    block_size = requested
                else:
                    LOGGER.debug(
                        "Requested blksize %s out of range; using default", requested
                    )
        return cls(block_size=block_size, requested_blksize=requested)

    def reply_options(self) -> Dict[str, str]:
        if self.requested_blksize is not None:
            return {"blksize": str(self.block_size)}
        return {}


# Packet builders -------------------------------------------------------


def _encode_request(
    opcode: int,
    filename: str,
    mode: str = "octet",
    *,
    options: Optional[Dict[str, str]] = None,
) -> bytes:
    parts = [
        struct.pack("!H", opcode),
        filename.encode("utf-8"),
        b"\0",
        mode.encode("ascii"),
        b"\0",
    ]
    if options:
        for key, value in options.items():
            parts.extend(
                [key.encode("ascii"), b"\0", str(value).encode("ascii"), b"\0"]
            )
    return b"".join(parts)


def build_rrq(filename: str, *, options: Optional[Dict[str, str]] = None) -> bytes:
    return _encode_request(OP_RRQ, filename, options=options)


def build_wrq(filename: str, *, options: Optional[Dict[str, str]] = None) -> bytes:
    return _encode_request(OP_WRQ, filename, options=options)


def build_data(block: int, payload: bytes) -> bytes:
    return struct.pack("!HH", OP_DATA, block) + payload


def build_ack(block: int) -> bytes:
    return struct.pack("!HH", OP_ACK, block)


def build_error(code: int, message: str) -> bytes:
    data = message.encode("utf-8")
    if not data.endswith(b"\0"):
        data += b"\0"
    return struct.pack("!HH", OP_ERROR, code) + data


def build_oack(options: Dict[str, str]) -> bytes:
    parts = [struct.pack("!H", OP_OACK)]
    for key, value in options.items():
        parts.extend([key.encode("ascii"), b"\0", value.encode("ascii"), b"\0"])
    return b"".join(parts)


def _parse_request(data: bytes) -> Tuple[str, Dict[str, str]]:
    elements = data.split(b"\0")
    if len(elements) < 2:
        raise TFTPError(ERR_ILLEGAL_OPERATION, "Malformed request")
    filename = elements[0].decode("utf-8", errors="replace")
    mode = elements[1].decode("ascii", errors="ignore").lower()
    if mode != "octet":
        raise TFTPError(ERR_ILLEGAL_OPERATION, f"Unsupported mode '{mode}'")
    options: Dict[str, str] = {}
    pairs = elements[2:]
    it = iter(pairs)
    for key_bytes, value_bytes in zip(it, it):
        if not key_bytes:
            break
        key = key_bytes.decode("ascii", errors="ignore").lower()
        value = value_bytes.decode("ascii", errors="ignore")
        options[key] = value
    return filename, options


def parse_oack(data: bytes) -> Dict[str, str]:
    items = data.split(b"\0")
    options: Dict[str, str] = {}
    it = iter(items)
    for key_bytes, value_bytes in zip(it, it):
        if not key_bytes:
            break
        key = key_bytes.decode("ascii", errors="ignore").lower()
        value = value_bytes.decode("ascii", errors="ignore")
        options[key] = value
    return options


# Client ----------------------------------------------------------------


class TFTPClient:
    """Blocking TFTP client."""

    def __init__(
        self,
        host: str,
        port: int = 69,
        *,
        timeout: float = 5.0,
        retries: int = 5,
        logger: Optional[logging.Logger] = None,
    ) -> None:
        self.host = host
        self.port = port
        self.timeout = timeout
        self.retries = retries
        self.logger = logger or LOGGER

    # Public API --------------------------------------------------------

    def download(
        self, remote_filename: str, destination: Path | str, *, block_size: int = 512
    ) -> None:
        destination_path = Path(destination)
        destination_path.parent.mkdir(parents=True, exist_ok=True)
        options = {"blksize": str(block_size)} if block_size != 512 else None
        request = build_rrq(remote_filename, options=options)
        self._download(request, destination_path)

    def upload(
        self, source: Path | str, remote_filename: str, *, block_size: int = 512
    ) -> None:
        source_path = Path(source)
        if not source_path.exists():
            raise FileNotFoundError(source_path)
        options = {"blksize": str(block_size)} if block_size != 512 else None
        request = build_wrq(remote_filename, options=options)
        self._upload(request, source_path)

    # Internal helpers --------------------------------------------------

    def _download(self, request: bytes, destination: Path) -> None:
        with (
            socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock,
            destination.open("wb") as fh,
        ):
            sock.settimeout(self.timeout)
            server_addr = (self.host, self.port)
            negotiated_block_size = 512
            last_packet = request
            retries_left = self.retries
            expected_block = 1
            sock.sendto(request, server_addr)
            while True:
                try:
                    packet, addr = sock.recvfrom(4 + negotiated_block_size + 4)
                except socket.timeout:
                    if retries_left <= 0:
                        raise TFTPError(ERR_NOT_DEFINED, "Download timed out")
                    self.logger.debug("Download timeout; resending last packet")
                    sock.sendto(last_packet, server_addr)
                    retries_left -= 1
                    continue

                retries_left = self.retries
                opcode = struct.unpack("!H", packet[:2])[0]

                if opcode == OP_OACK:
                    options = parse_oack(packet[2:])
                    if "blksize" in options:
                        try:
                            negotiated_block_size = int(options["blksize"])
                        except ValueError:
                            negotiated_block_size = 512
                    server_addr = addr
                    last_packet = build_ack(0)
                    sock.sendto(last_packet, server_addr)
                    continue

                if opcode == OP_ERROR:
                    code = struct.unpack("!H", packet[2:4])[0]
                    message = packet[4:-1].decode("utf-8", errors="replace")
                    raise TFTPError(code, message)

                if opcode != OP_DATA:
                    raise TFTPError(
                        ERR_ILLEGAL_OPERATION, f"Unexpected opcode {opcode}"
                    )

                block = struct.unpack("!H", packet[2:4])[0]
                payload = packet[4:]
                server_addr = addr

                if block == expected_block:
                    fh.write(payload)
                    expected_block += 1
                elif block < expected_block:
                    self.logger.debug("Duplicate DATA block %s", block)
                else:
                    self.logger.debug(
                        "Out-of-order block %s (expected %s)", block, expected_block
                    )
                    continue

                last_packet = build_ack(block)
                sock.sendto(last_packet, server_addr)

                if len(payload) < negotiated_block_size:
                    break

    def _upload(self, request: bytes, source: Path) -> None:
        with (
            socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock,
            source.open("rb") as fh,
        ):
            sock.settimeout(self.timeout)
            server_addr = (self.host, self.port)
            negotiated_block_size = 512
            last_packet = request
            retries_left = self.retries
            block_number = 0
            finished = False

            sock.sendto(request, server_addr)

            while True:
                try:
                    packet, addr = sock.recvfrom(4 + negotiated_block_size + 4)
                except socket.timeout:
                    if retries_left <= 0:
                        raise TFTPError(ERR_NOT_DEFINED, "Upload timed out")
                    self.logger.debug("Upload timeout; resending last packet")
                    sock.sendto(last_packet, server_addr)
                    retries_left -= 1
                    continue

                retries_left = self.retries
                opcode = struct.unpack("!H", packet[:2])[0]

                if opcode == OP_OACK:
                    options = parse_oack(packet[2:])
                    if "blksize" in options:
                        try:
                            negotiated_block_size = int(options["blksize"])
                        except ValueError:
                            negotiated_block_size = 512
                    server_addr = addr
                    block_number = 0
                    payload = fh.read(negotiated_block_size)
                    block_number += 1
                    last_packet = build_data(block_number, payload)
                    sock.sendto(last_packet, server_addr)
                    finished = len(payload) < negotiated_block_size
                    continue

                if opcode == OP_ACK:
                    ack_block = struct.unpack("!H", packet[2:4])[0]
                    server_addr = addr
                    if ack_block == block_number:
                        if finished:
                            break
                        payload = fh.read(negotiated_block_size)
                        block_number += 1
                        last_packet = build_data(block_number, payload)
                        sock.sendto(last_packet, server_addr)
                        finished = len(payload) < negotiated_block_size
                    elif ack_block < block_number:
                        self.logger.debug("Duplicate ACK %s", ack_block)
                        sock.sendto(last_packet, server_addr)
                    else:
                        self.logger.debug(
                            "Unexpected ACK %s (current %s)", ack_block, block_number
                        )
                    continue

                if opcode == OP_ERROR:
                    code = struct.unpack("!H", packet[2:4])[0]
                    message = packet[4:-1].decode("utf-8", errors="replace")
                    raise TFTPError(code, message)

                raise TFTPError(ERR_ILLEGAL_OPERATION, f"Unexpected opcode {opcode}")


# Server ----------------------------------------------------------------


class TFTPServer:
    """Threaded TFTP server bound to a specific root directory."""

    def __init__(
        self,
        root: Path | str,
        *,
        host: str = "0.0.0.0",
        port: int = 69,
        timeout: float = 5.0,
        retries: int = 5,
        logger: Optional[logging.Logger] = None,
    ) -> None:
        self.root = Path(root).resolve()
        self.host = host
        self.port = port
        self.timeout = timeout
        self.retries = retries
        self.logger = logger or LOGGER

        self._stop_event = threading.Event()
        self._ready_event = threading.Event()
        self._thread: Optional[threading.Thread] = None
        self._listen_sock: Optional[socket.socket] = None
        self._server_port: Optional[int] = None

    # Lifecycle ---------------------------------------------------------

    @property
    def server_port(self) -> int:
        if self._server_port is None:
            raise RuntimeError("Server not started")
        return self._server_port

    def start(self) -> None:
        if self._thread and self._thread.is_alive():
            return
        self._stop_event.clear()
        self._ready_event.clear()
        self._thread = threading.Thread(target=self._serve_forever, daemon=True)
        self._thread.start()
        self._ready_event.wait()

    def stop(self) -> None:
        self._stop_event.set()
        if self._listen_sock:
            try:
                self._listen_sock.close()
            except OSError:
                pass
        if self._thread:
            self._thread.join(timeout=2.0)
            self._thread = None

    def __enter__(self) -> "TFTPServer":
        self.start()
        return self

    def __exit__(self, exc_type, exc, tb) -> None:
        self.stop()

    # Core server loop --------------------------------------------------

    def _serve_forever(self) -> None:
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
            sock.bind((self.host, self.port))
            self._listen_sock = sock
            self._server_port = sock.getsockname()[1]
            self.logger.info(
                "TFTP server listening on %s:%s", self.host, self._server_port
            )
            sock.settimeout(0.5)
            self._ready_event.set()
            while not self._stop_event.is_set():
                try:
                    data, addr = sock.recvfrom(2048)
                except socket.timeout:
                    continue
                except OSError:
                    break
                if not data:
                    continue
                threading.Thread(
                    target=self._handle_request, args=(data, addr), daemon=True
                ).start()

    def _handle_request(self, data: bytes, addr: Tuple[str, int]) -> None:
        opcode = struct.unpack("!H", data[:2])[0]
        try:
            filename, options = _parse_request(data[2:])
        except TFTPError as exc:
            self.logger.debug("Bad request from %s: %s", addr, exc)
            self._send_error(addr, exc.code, exc.message)
            return

        transfer_options = TransferOptions.from_request(options)
        try:
            target = self._resolve_path(filename)
        except TFTPError as exc:
            self._send_error(addr, exc.code, exc.message)
            return

        if opcode == OP_RRQ:
            self.logger.info("RRQ %s from %s", filename, addr)
            self._serve_rrq(target, addr, transfer_options)
        elif opcode == OP_WRQ:
            self.logger.info("WRQ %s from %s", filename, addr)
            self._serve_wrq(target, addr, transfer_options)
        else:
            self.logger.debug("Illegal opcode %s from %s", opcode, addr)
            self._send_error(addr, ERR_ILLEGAL_OPERATION, "Unsupported request")

    # Request handlers --------------------------------------------------

    def _serve_rrq(
        self, path: Path, addr: Tuple[str, int], options: TransferOptions
    ) -> None:
        if not path.exists():
            self._send_error(addr, ERR_FILE_NOT_FOUND, "File not found")
            return
        try:
            fh = path.open("rb")
        except OSError as exc:
            self._send_error(addr, ERR_ACCESS_VIOLATION, str(exc))
            return

        with fh, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
            sock.settimeout(self.timeout)
            sock.bind((self.host, 0))
            block_size = options.block_size
            reply_options = options.reply_options()
            if reply_options:
                if not self._send_oack(sock, addr, reply_options):
                    return
            block = 1
            while True:
                data = fh.read(block_size)
                packet = build_data(block, data)
                if not self._send_with_ack(sock, packet, addr, expected_ack=block):
                    return
                if len(data) < block_size:
                    break
                block += 1

    def _serve_wrq(
        self, path: Path, addr: Tuple[str, int], options: TransferOptions
    ) -> None:
        if path.exists():
            self._send_error(addr, ERR_FILE_EXISTS, "File already exists")
            return
        try:
            path.parent.mkdir(parents=True, exist_ok=True)
            fh = path.open("wb")
        except OSError as exc:
            self._send_error(addr, ERR_ACCESS_VIOLATION, str(exc))
            return

        with fh, socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
            sock.settimeout(self.timeout)
            sock.bind((self.host, 0))
            block_size = options.block_size
            reply_options = options.reply_options()
            if reply_options:
                packet = build_oack(reply_options)
                if not self._send_packet(sock, packet, addr):
                    return
                last_ack = packet
            else:
                ack = build_ack(0)
                if not self._send_packet(sock, ack, addr):
                    return
                last_ack = ack
            expected_block = 1
            while True:
                try:
                    packet, peer = sock.recvfrom(4 + block_size + 4)
                except socket.timeout:
                    self.logger.debug(
                        "WRQ timeout; resending ACK %s", expected_block - 1
                    )
                    if not self._send_packet(sock, last_ack, addr):
                        return
                    continue
                if peer != addr:
                    sock.sendto(
                        build_error(ERR_UNKNOWN_TID, "Unknown transfer ID"), peer
                    )
                    continue
                opcode = struct.unpack("!H", packet[:2])[0]
                if opcode == OP_DATA:
                    block = struct.unpack("!H", packet[2:4])[0]
                    payload = packet[4:]
                    if block == expected_block:
                        try:
                            fh.write(payload)
                        except OSError:
                            self._send_error(
                                addr, ERR_DISK_FULL, "Disk full or allocation exceeded"
                            )
                            return
                        last_ack = build_ack(block)
                        if not self._send_packet(sock, last_ack, addr):
                            return
                        if len(payload) < block_size:
                            break
                        expected_block += 1
                    elif block < expected_block:
                        self.logger.debug("Duplicate DATA block %s", block)
                        self._send_packet(sock, build_ack(block), addr)
                    else:
                        self.logger.debug(
                            "Unexpected DATA block %s (expected %s)",
                            block,
                            expected_block,
                        )
                elif opcode == OP_ACK:
                    ack_block = struct.unpack("!H", packet[2:4])[0]
                    if ack_block == 0:
                        self.logger.debug("Received ACK0 after OACK; awaiting DATA1")
                        continue
                    self._send_error(addr, ERR_ILLEGAL_OPERATION, "Unexpected ACK")
                    return
                elif opcode == OP_ERROR:
                    code = struct.unpack("!H", packet[2:4])[0]
                    message = packet[4:-1].decode("utf-8", errors="replace")
                    self.logger.warning("Client aborted upload: %s (%s)", message, code)
                    return
                else:
                    self._send_error(addr, ERR_ILLEGAL_OPERATION, "Unexpected packet")
                    return

    # Utilities ---------------------------------------------------------

    def _send_oack(
        self, sock: socket.socket, addr: Tuple[str, int], options: Dict[str, str]
    ) -> bool:
        packet = build_oack(options)
        for attempt in range(self.retries):
            try:
                sock.sendto(packet, addr)
                response, peer = sock.recvfrom(4 + 4)
            except socket.timeout:
                continue
            if peer != addr:
                sock.sendto(build_error(ERR_UNKNOWN_TID, "Unknown transfer ID"), peer)
                continue
            opcode = struct.unpack("!H", response[:2])[0]
            if opcode == OP_ACK and struct.unpack("!H", response[2:4])[0] == 0:
                return True
            if opcode == OP_ERROR:
                code = struct.unpack("!H", response[2:4])[0]
                message = response[4:-1].decode("utf-8", errors="replace")
                self.logger.warning("Client rejected OACK: %s (%s)", message, code)
                return False
        self.logger.warning("OACK handshake failed with %s", addr)
        return False

    def _send_with_ack(
        self,
        sock: socket.socket,
        packet: bytes,
        addr: Tuple[str, int],
        *,
        expected_ack: int,
    ) -> bool:
        for attempt in range(self.retries):
            try:
                sock.sendto(packet, addr)
                response, peer = sock.recvfrom(4 + 4)
            except socket.timeout:
                continue
            if peer != addr:
                sock.sendto(build_error(ERR_UNKNOWN_TID, "Unknown transfer ID"), peer)
                continue
            opcode = struct.unpack("!H", response[:2])[0]
            if opcode == OP_ACK:
                ack_number = struct.unpack("!H", response[2:4])[0]
                if ack_number == expected_ack:
                    return True
                if ack_number < expected_ack:
                    self.logger.debug(
                        "Duplicate ACK %s while waiting for %s",
                        ack_number,
                        expected_ack,
                    )
                    continue
                self.logger.debug(
                    "Unexpected ACK %s (expected %s)", ack_number, expected_ack
                )
                continue
            if opcode == OP_ERROR:
                code = struct.unpack("!H", response[2:4])[0]
                message = response[4:-1].decode("utf-8", errors="replace")
                self.logger.warning("Client aborted transfer: %s (%s)", message, code)
                return False
        self.logger.warning("Failed to receive ACK %s from %s", expected_ack, addr)
        return False

    def _send_packet(
        self, sock: socket.socket, packet: bytes, addr: Tuple[str, int]
    ) -> bool:
        try:
            sock.sendto(packet, addr)
            return True
        except OSError as exc:
            self.logger.error("Failed to send packet: %s", exc)
            return False

    def _send_error(self, addr: Tuple[str, int], code: int, message: str) -> None:
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as sock:
            try:
                sock.sendto(build_error(code, message), addr)
            except OSError:
                pass

    def _resolve_path(self, filename: str) -> Path:
        unsafe = Path(filename)
        if unsafe.is_absolute():
            raise TFTPError(ERR_ACCESS_VIOLATION, "Absolute paths not allowed")
        candidate = (self.root / unsafe).resolve()
        try:
            candidate.relative_to(self.root)
        except ValueError as exc:
            raise TFTPError(ERR_ACCESS_VIOLATION, "Access violation") from exc
        return candidate


__all__ = [
    "TFTPClient",
    "TFTPServer",
    "TFTPError",
    "build_rrq",
    "build_wrq",
    "build_data",
    "build_ack",
    "build_error",
    "build_oack",
    "parse_oack",
]
