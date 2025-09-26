# Chat Server/Client Reference Implementation

This folder contains an asyncio-based messaging stack that demonstrates reliable TCP chat with multi-room support as well as a UDP mode that layers basic retransmission and exponential back-off on top of a connectionless transport. It is designed to be a teaching aid and a starting point for experiments with text protocols, configuration-driven services, and graceful shutdown.

## Architecture Overview

```
Practical/Chat Server Client/
├── README.md                    # This document (architecture, protocol, usage)
├── chat/                        # Python package with shared utilities + CLIs
│   ├── __init__.py
│   ├── cli.py                   # Entry point wiring subcommands to implementations
│   ├── config.py                # Dataclasses + helpers for TOML/JSON configuration
│   ├── logging_utils.py         # Rotating log configuration helpers
│   ├── protocols.py             # Message schemas and validation helpers
│   ├── tcp_client.py            # Asyncio StreamReader/Writer implementation
│   ├── tcp_server.py            # Asyncio stream-based chat room server with history
│   ├── udp_client.py            # Datagram client with retransmission + back-off
│   └── udp_server.py            # Datagram protocol broadcast hub
├── configs/
│   ├── tcp_server.toml          # Sample configuration for TCP server
│   ├── tcp_client.toml          # Sample configuration for TCP client
│   └── udp.toml                 # Sample configuration shared by UDP server/client
└── logs/                        # Created at runtime; houses rotating log files
```

Key traits:

- **Event loop centric** – Everything runs on `asyncio`, with cooperative coroutines for network and stdin handling.
- **Line-delimited JSON protocol** – Structured payloads without external dependencies. TCP uses newline framing; UDP uses discrete datagrams.
- **Room-aware broadcasting** – Each room is an isolated publish/subscribe domain; the server tracks membership and presence notifications.
- **Durable logging** – Both modes push events through a rotating file handler so you can inspect conversation history.
- **Configurable** – Ports, bind addresses, log paths, retransmission limits, and more are read from TOML/JSON config files.

## Protocol Summary

### TCP (stream) mode

- Transport: TCP socket per client.
- Framing: UTF-8 JSON payloads separated by `"\n"` (newline).
- Message schema:
  - `join`: `{ "type": "join", "room": "lobby", "user": "alice" }`
  - `message`: `{ "type": "message", "room": "lobby", "text": "Hello!" }`
  - `leave`: `{ "type": "leave" }` (optional — closing socket also leaves)
  - Server broadcasts `system` messages for joins/leaves and echoes `message` packets from peers. Each broadcast includes a monotonically increasing `seq` field.
- Rooms: Clients must send a `join` before messaging. Re-joining another room moves the user between rooms.
- Logging: Each broadcast is serialized to the configured log path. Log lines follow `{timestamp} {room} {user/system}: {text}`.

### UDP (datagram) mode

- Transport: UDP datagrams.
- Schema: Same `join`/`message` envelopes as TCP with two additions:
  - `id` (client-generated integer) enables acknowledgement pairing.
  - `ack` packets: `{ "type": "ack", "id": 42 }` from the server.
- Reliability: The client retries sends until an `ack` for the matching `id` is received or the retry budget is exhausted. Back-off doubles the delay each retry (`base_delay`, `base_delay * 2`, ...).
- Presence: Server tracks most recent room for each `(host, port)` tuple and rebroadcasts messages to everyone that is currently joined to that room.

## Running the Examples

1. **Install dependencies** (from repository root):

   ```bash
   python -m venv .venv
   source .venv/bin/activate
   pip install -r requirements.txt  # or ensure Python 3.11+ standard library is available
   ```

2. **Launch the TCP server**:

   ```bash
   cd "Practical/Chat Server Client"
   python -m chat.cli tcp-server --config configs/tcp_server.toml
   ```

   The server writes to the log path described in the config (defaults to `logs/tcp-chat.log`).

3. **Connect TCP clients** (multiple terminals):

   ```bash
   cd "Practical/Chat Server Client"
   python -m chat.cli tcp-client --config configs/tcp_client.toml --user alice
   python -m chat.cli tcp-client --config configs/tcp_client.toml --user bob
   ```

   Type messages; they are broadcast to everyone joined in the same room. Use `/join room-name` locally to switch rooms (client translates to a `join` packet).

4. **Launch UDP demo**:

   ```bash
   cd "Practical/Chat Server Client"
   python -m chat.cli udp-server --config configs/udp.toml
   python -m chat.cli udp-client --config configs/udp.toml --user charlie
   ```

   UDP clients send the same `/join` and text commands, but messages are datagrams with retry/ack semantics.

## CLI Cheatsheet

- `/help` – shows inline instructions.
- `/join ROOM` – switch to another room (creates on demand).
- `/quit` – disconnect gracefully.
- `/rooms` (TCP only) – ask the server for the list of active rooms.

## Extensibility Notes

- `chat.protocols` centralizes message validation so you can add commands like `/whisper` or `/who` without touching every module.
- Both servers expose `async def serve(config)` entry points; reuse them in larger orchestration scripts or embed them in tests.
- Logging uses `logging.handlers.RotatingFileHandler`. Adjust size/backup defaults in configs to tune retention.
- UDP reliability is intentionally simple. Experiment with selective acknowledgement, sequence windows, or negative acknowledgements.

Happy experimenting!
