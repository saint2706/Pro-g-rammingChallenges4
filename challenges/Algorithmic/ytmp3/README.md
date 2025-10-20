# YouTube Audio Downloader

## Problem Statement
Download audio tracks from YouTube (or other yt-dlp supported sites), convert them to a chosen format/quality, and optionally export a JSON summary.

## Usage
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
