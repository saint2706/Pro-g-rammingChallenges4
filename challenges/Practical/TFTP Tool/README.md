# TFTP Tool

A minimal-but-usable Trivial File Transfer Protocol (TFTP) implementation that
covers the essentials from [RFC 1350](https://www.rfc-editor.org/rfc/rfc1350)
and the option extension for block-size negotiation. The focus is on
understanding the protocol primitives and the real-world trade-offs of using a
UDP-based file transfer mechanism.

## What You Get

- A UDP TFTP server that can serve or accept files from a shared directory
- A matching client with download (`RRQ`) and upload (`WRQ`) support
- RFC 2347-style option negotiation for `blksize`
- Retransmission logic with configurable timeouts and retry counts
- A small CLI wrapper (`cli.py`) for scripted transfers
- Integration tests that exercise localhost transfers end-to-end

The code is intentionally compact and dependency-free so it is easy to read and
modify. Logging is wired in so you can follow the packet exchange at `INFO` or
`DEBUG` verbosity.

## TFTP in 90 Seconds (RFC 1350 Primer)

TFTP is a lock-step protocol that runs over UDP:

1. A client sends a *read request* (RRQ) or *write request* (WRQ) packet to the
   server's well-known port (69 by default).
2. The server responds from a freshly allocated *transfer identifier* (TID),
   which is just another UDP port. All further packets in the session use this
   port pair.
3. Data flows in fixed-size blocks (default 512 bytes). Each block is numbered
   starting at 1, and the final block is the one smaller than the negotiated
   block size.
4. Every data block must be acknowledged (ACK) before the next one is sent.
   Retransmissions happen when an ACK or DATA packet is not received within the
   timeout window.
5. Errors are reported with dedicated ERROR packets that include a numeric code
   and human-readable message.

Because TFTP uses UDP there is no inherent reliability or ordering. The
protocol's simplicity makes it easy to implement but also easy to misuse.

### Block-Size Negotiation

RFC 1350 fixed the block size at 512 bytes. Later extensions (notably RFC 2347
and RFC 2348) introduced option negotiation so a client can request a larger
block size (`blksize`). This tool accepts the `blksize` option and falls back to
512 bytes when the option is missing or invalid.

## Security Warnings

TFTP is deliberately "trivial" and omits many safety features:

- **No authentication or encryption.** Anyone with network access to the server
  can read or write files within the exposed directory.
- **No path sandboxing beyond a shared root.** The server prevents directory
  traversal (`..`) by normalising file names, but a misconfiguration can still
  leak sensitive data.
- **UDP spoofing is trivial.** Because UDP is connectionless, an attacker can
  forge packets or hijack a transfer with little effort.
- **No integrity guarantees.** Transfers can be truncated or corrupted if
  packets are lost and the retry budget is exhausted.
- **Privilege considerations.** Binding to port 69 requires elevated
  privileges on many systems. The CLI defaults to a high, non-privileged port
  for experimentation.

Use this code to learn, test, or run inside isolated lab environments. For
production workloads prefer modern, authenticated protocols such as SFTP or
HTTPS.

## Usage

```
python "challenges/Practical/TFTP Tool/cli.py" server --host 0.0.0.0 --port 6969 --root data
python "challenges/Practical/TFTP Tool/cli.py" get localhost 6969 remote.bin local.bin
python "challenges/Practical/TFTP Tool/cli.py" put localhost 6969 local.bin remote.bin --blksize 1024
```

Run `python "challenges/Practical/TFTP Tool/cli.py" --help` for the full CLI surface.

## Project Layout

```
challenges/Practical/TFTP Tool/
├── README.md          ← This file: protocol overview and warnings
├── cli.py             ← Argument parsing + logging + client/server entry points
├── tftp.py            ← Core protocol logic shared by both sides
└── tests/
    └── test_integration.py  ← Localhost end-to-end transfers and option checks
```

Feel free to use the modules programmatically. Both the client and server are
implemented as Python classes with context-manager helpers to simplify unit
and integration testing.
