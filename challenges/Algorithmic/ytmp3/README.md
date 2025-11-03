# YouTube Audio Downloader

## Problem Statement
Download audio tracks from YouTube (or other yt-dlp supported sites), convert them to a chosen format/quality, and optionally export a JSON summary.

## Usage

### Python CLI (`cringe.py`)
- Download a single video as MP3 (default quality):
  ```bash
  python cringe.py https://youtu.be/VIDEOID
  ```
- Batch download with custom format and bitrate:
  ```bash
  python cringe.py -f opus -q 128 https://youtu.be/ID1 https://youtu.be/ID2
  ```
- Read URLs from a file and produce JSON output:
  ```bash
  python cringe.py -F mp3 -q 192 -o downloads/ -i urls.txt --json
  ```

### Haskell CLI (`YTDL.hs`)

1. Ensure tooling dependencies are installed:
   - `ghc` or another Haskell build toolchain (`cabal-install`/`stack`).
   - Haskell libraries: `aeson`, `bytestring`, and `process` (all available via `cabal install aeson` or as part of most modern `stack` snapshots).
   - External executables: `yt-dlp` (preferred) or `youtube-dl`, plus `ffmpeg` on your `PATH`.
2. Compile the helper once (or run it via `runghc` for quick tests):
   ```bash
   cd challenges/Algorithmic/ytmp3
   ghc -O2 YTDL.hs   # produces ./YTDL
   ```
3. Mirror the Python flags from the compiled binary:
   ```bash
   ./YTDL https://youtu.be/VIDEOID
   ./YTDL -f opus -q 128 https://youtu.be/ID1 https://youtu.be/ID2
   ./YTDL -f mp3 -q 192 -o downloads/ -i urls.txt --json
   ./YTDL --quiet --json https://youtu.be/ID3
   ```

Flag parity with `cringe.py`:

| Flag | Description |
| ---- | ----------- |
| `-f/--format` | Select the audio container/codec (`mp3`, `m4a`, `opus`, `vorbis`, `wav`, `flac`). |
| `-q/--quality` | Target audio bitrate (forwarded to ffmpeg via yt-dlp). |
| `-o/--out-dir` | Destination directory (auto-created when missing). |
| `-i/--input` | Read additional URLs from a file (blank lines and `#` comments ignored). |
| `--json` | Emit a JSON summary identical to the Python tool (backend, counts, per-item status). |
| `--quiet` | Suppress progress logs; errors still appear inside the JSON summary. |

The JSON schema matches `cringe.py`, allowing the existing `download_visualizer.py` and any automation consuming the Python output to ingest Haskell runs without modification.

### Visualising Batch Runs

- Turn a JSON summary (from `--json`) into charts and an HTML report:
  ```bash
  python download_visualizer.py downloads/summary.json --output-dir reports/
  ```
  This generates PNG charts, an embeddable HTML page, and a metrics JSON file
  you can diff or assert against in tests.

## Debugging Tips
- Ensure `yt-dlp` (preferred) or `youtube_dl` plus `ffmpeg` are installed and available on `PATH`.
- Use `--quiet` to suppress progress when embedding in other scripts; drop the flag while debugging to see backend logging.
- Errors are collected per URL—inspect the JSON summary or console messages to identify invalid links or network issues.

## Implementation Notes
- Dataclass configuration validates formats, ensures output directories exist, and merges URLs supplied via CLI or file.
- Delegates downloading to `yt-dlp`/`youtube_dl` and reconfigures the post-processing pipeline to extract audio at the requested bitrate.
- JSON summaries capture success/failure metadata for integration into automation pipelines.

## Further Reading
- [yt-dlp Documentation](https://github.com/yt-dlp/yt-dlp#readme)
- [FFmpeg Multimedia Framework](https://ffmpeg.org/documentation.html)
