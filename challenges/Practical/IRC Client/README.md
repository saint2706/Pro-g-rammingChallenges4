# Async Terminal IRC Client

This package contains a lightweight asyncio-based IRC client designed for the `/g/` practical challenge. It focuses on being easy to run from a terminal while still offering the quality-of-life features needed to hold a live conversation.

## Features

- Asynchronous networking and stdin handling (no blocking while messages stream in)
- Optional TLS encryption (STARTTLS is not supported; enable TLS to connect via SSL/TLS from the start)
- Automatic authentication via `PASS`, `NICK`, and `USER` messages when credentials are provided
- Multiple channel join support on connect, with the ability to join more channels later
- Rich terminal commands (`/join`, `/msg`, `/me`, `/quit`, `/whois`, `/topic`, `/names`, `/raw`)
- Automatic reply to server `PING` keep-alives
- Connection log written to a timestamped log file
- Graceful reconnection with exponential backoff on transient network failures

## Installation

The client only depends on the Python standard library. Python 3.10 or newer is recommended.

Create a virtual environment (optional but encouraged):

```bash
python -m venv .venv
source .venv/bin/activate
```

No additional packages need to be installed.

## Usage

From inside this folder, run:

```bash
python irc_client.py --server irc.libera.chat --port 6697 --tls --nickname exampleUser --channels "#example,#bots"
```

### Command-line options

| Flag | Description |
| ---- | ----------- |
| `--server` | IRC server hostname or IP. |
| `--port` | Server port (default `6667`, or `6697` when using TLS). |
| `--nickname` / `-n` | Nickname to present to the server. |
| `--username` | Username for the `USER` command (defaults to nickname). |
| `--realname` | Realname/gecos field (defaults to nickname). |
| `--password` | Optional server password / NickServ password sent with `PASS`. |
| `--channels` | Comma-separated list of channels to join on connect. |
| `--no-reconnect` | Disable automatic reconnect attempts. |
| `--max-retries` | Limit number of reconnect attempts (default: unlimited). |
| `--log-file` | Path to a log file (default: `logs/irc_<server>_<timestamp>.log`). |
| `--tls` / `--no-tls` | Toggle TLS usage (disabled by default). |
| `--retry-delay` | Initial reconnect delay in seconds (default: 3). |

Use `python irc_client.py --help` for the most up-to-date description of every flag.

### Runtime commands

Inside the client you can type the following commands (prefix with `/`):

| Command | Action |
| ------- | ------ |
| `/join <#channel>` | Join a channel. |
| `/leave <#channel>` | Leave a channel (alias: `/part`). |
| `/msg <target> <text>` | Send a private or channel message. |
| `/me <target> <action>` | Send a CTCP ACTION (emote). |
| `/topic <#channel> [new topic]` | Query or set a channel topic. |
| `/names <#channel>` | Request the user list for a channel. |
| `/whois <nick>` | Request WHOIS information for a nickname. |
| `/raw <line>` | Send a raw IRC command. |
| `/quit [message]` | Quit the session and close the client. |

Any text that does not start with `/` is treated as a message directed at the most recently joined channel (or the first channel from `--channels`).

### Logs

Logs are stored in UTF-8 with timestamps prepended to every line. By default the client creates a `logs/` directory next to `irc_client.py`.

### Reconnecting

When the connection drops unexpectedly the client waits a short delay and then attempts to reconnect. The delay grows exponentially (up to 60 seconds). You can disable this behaviour with `--no-reconnect` or cap it with `--max-retries`.

## Tips

- IRC networks sometimes require you to register nicknames before you can talk or join channels. Use `/msg NickServ REGISTER` commands after connecting, or supply `--password` if the network accepts SASL-like PASS authentication.
- Some networks block connections without TLS; pass `--tls` to connect to ports that expect SSL/TLS wrapping (6697 on most modern networks).
- If you leave the client idle, keep an eye on the log file—ping/pong messages are recorded there as well.

Enjoy the nostalgia!
