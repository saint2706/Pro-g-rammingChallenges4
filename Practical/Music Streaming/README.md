# Music Streaming Challenge

A lightweight LAN music streaming stack consisting of a Flask-based server and
an interactive CLI client. The server scans a media folder for audio files
(MP3, FLAC, WAV, OGG, M4A, AAC), exposes a playlist API, and streams tracks over
HTTP using chunked transfer. Clients can discover the server automatically,
stream tracks with configurable buffering, and play them back via FFmpeg's
`ffplay` or save the stream to disk.

## Features

- **Playlist management:** automatic metadata extraction (title, artist, album,
  duration) with endpoints to refresh, select, skip, and fetch the current
  track.
- **Chunked streaming:** files are served as generator-based responses so large
  media never loads fully into memory.
- **LAN discovery:** UDP broadcast pings reveal active servers without manual
  configuration.
- **Buffered playback:** client delays playback until a configurable byte budget
  has been downloaded, reducing stutter on slower networks.
- **Metadata aware CLI:** interactive prompt to list tracks, jump to entries,
  change buffer size, and stream sequentially.

## Project Structure

```
Practical/Music Streaming/
├── README.md          # You're here
├── server.py          # Flask app + playlist manager + discovery responder
└── client.py          # CLI client for discovery, playlist control, streaming
```

## Requirements

Install dependencies from the Practical suite's consolidated requirements file
plus FFmpeg for audio playback:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r ../requirements.txt
# Optional but recommended: install FFmpeg so `ffplay` is available.
```

Additional packages introduced for this challenge:

- `mutagen` – read metadata (ID3/FLAC/etc.).
- `requests` (already used elsewhere) – HTTP client.

The CLI defaults to `ffplay` for playback. If FFmpeg is missing, streams are
saved to a temporary file and the path is printed so you can open it manually.

## Running the Server

1. Place audio files inside a media directory (defaults to `./media`).
2. Start the server:

   ```bash
   python server.py --media ./media --host 0.0.0.0 --port 8000
   ```

   - `--media` : directory containing audio files.
   - `--host`  : listening interface (`0.0.0.0` exposes to LAN).
   - `--port`  : HTTP port (default `8000`).
   - `--discovery-port` : UDP port for discovery pings (default `9999`).

3. The server logs the public URL and waits for clients. Drop new files into the
   media directory at any time and call `POST /api/playlist/refresh` (or use the
   client's `refresh` command) to rescan.

## Running the Client

1. Ensure the server is running on the same network.
2. Launch the client:

   ```bash
   python client.py
   ```

   By default the client broadcasts a discovery message and lists available
   servers. Use `--host`/`--port` to skip discovery.

3. At the prompt type `list`, `play <n>`, `next`, `prev`, `refresh`, or `buffer
   <bytes>` to control playback.

   Example session:

   ```text
   $ python client.py
   Broadcasting discovery packet...
   Found server at 192.168.1.42:8000
   Available tracks:
   [01] Orbital Sunrise by Demo Artist (Space Jams) — 05:12
   [02] Ambient Clouds — 03:45
   Commands:
     list              - show playlist
     play <n>          - select and stream track number n
     next / prev       - skip forward/backward using server playlist order
     refresh           - rescan playlist from server
     buffer <bytes>    - change local buffer threshold before playback
     quit              - exit the client
   music> play 1
   Requesting http://192.168.1.42:8000/api/stream/track-0
   Title: Orbital Sunrise | Artist: Demo Artist | Album: Space Jams | Duration: 05:12 | Source file: sunrise.mp3
   Buffered 512 KiB in 1.2s. Starting playback...
   ```

If FFmpeg is missing, the client saves the stream as a temporary file and tells
you where to find it.

## Deployment Notes

- **Service separation:** The Flask app is stateless with respect to playlist
  scanning. Use systemd, Docker, or another process supervisor to keep it
  running. Mount your media directory into the container/VM and open the HTTP &
  discovery ports.
- **Reverse proxy:** When exposing outside your LAN, proxy through Nginx/Traefik
  to add TLS and auth. Forward `/api/stream/*` without buffering for best
  results.
- **Scaling:** For larger libraries, consider caching metadata and precomputing
  playlists. The current implementation targets personal/home use.

## Marking the Challenge

This implementation completes the **Practical #13 – Music Streaming** challenge
from the `/g/` list with a functional streaming server, client, documentation,
and deployment guidance.
