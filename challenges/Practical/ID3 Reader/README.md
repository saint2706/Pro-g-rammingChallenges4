# ID3 Reader

A lightweight toolkit for inspecting and exporting MP3 metadata. The reader understands the two
most common tagging families – **ID3v1 (and v1.1 track extensions)** and **ID3v2.x (2.2–2.4, tested with v2.3)** – and
normalises their fields into a single friendly view. Use the CLI to print summaries or export JSON/CSV, or launch the Tk GUI
for an interactive browser.

## Features

- Detects ID3v1/v1.1 footers and ID3v2.x headers in the same file.
- Normalises common fields (`title`, `artist`, `album`, `year`, `genre`, `track`, `album_artist`, `comment`).
- Provides both command line and Tkinter GUI frontends.
- Exports batches of MP3 tags to JSON or CSV for cataloguing.
- Includes pytest fixtures that validate ID3v1 and ID3v2 parsing without requiring large audio samples.

## Quick start

```bash
python "challenges/Practical/ID3 Reader/id3_reader.py" example.mp3
```

Optional arguments:

```bash
python "challenges/Practical/ID3 Reader/id3_reader.py" track1.mp3 track2.mp3 \
    --json tags.json --csv tags.csv --fields title artist album year
```

- `--json` / `--csv` export all parsed tracks.
- `--fields` limits console output to a subset of fields (defaults to all normalised tags).

### GUI browser

```bash
python "challenges/Practical/ID3 Reader/gui.py"
```

Use **Open File** to pick an MP3. Metadata appears in a table and can be exported to JSON/CSV from the toolbar.

## Implementation notes

- ID3v2 parsing uses [`mutagen`](https://mutagen.readthedocs.io/). Text frames are converted to UTF‑8 strings and grouped by
  frame identifier.
- ID3v1/v1.1 parsing is handled manually so that we can surface the version number even when no ID3v2 header exists.
- When both versions are present, ID3v2 values win and ID3v1 fills in any missing fields.

## Testing

From the repository root:

```bash
pytest challenges/Practical/ID3\ Reader/tests
```

The fixtures synthesise small MP3 files containing only metadata blocks to keep the repository lightweight while still
covering ID3v1 and ID3v2 behaviour.
