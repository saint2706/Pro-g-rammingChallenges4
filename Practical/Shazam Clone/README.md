# Shazam Clone – Audio Fingerprinting Demo

This project demonstrates a small-scale audio fingerprinting pipeline inspired by Shazam's "constellation map" technique. It walks through extracting robust spectral landmarks, hashing them into fingerprints, indexing a reference library, and querying with either a recorded clip or an existing audio file.

## Fingerprinting approach

1. **Spectrogram analysis** – Audio is resampled to a uniform rate (default `22.05 kHz`) and converted to a short-time Fourier transform (STFT). Magnitudes are expressed in decibels to normalize loudness variations.
2. **Peak constellation** – The spectrogram is scanned for local maxima across time–frequency neighborhoods. These peaks are resilient to noise and distortion and represent characteristic landmarks of the song.
3. **Target zone pairing** – Each anchor peak is paired with several nearby peaks within a configurable time window. The tuples `(f_anchor, f_target, Δt)` form the basis of the fingerprint. The hashing space is intentionally coarse so that small tuning and tempo deviations collide into the same bucket.
4. **MinHash signatures** – The full set of pairwise hashes is summarized with a fixed-length MinHash signature. These signatures enable fast candidate filtering by approximating Jaccard similarity between tracks before the expensive voting stage.
5. **Offset voting** – When querying, matching hashes vote for an offset between the reference and query clips. The track with the most consistent offset wins, and the vote concentration becomes a confidence estimate.

The combination of local peaks, hash-based pairing, and offset voting keeps the system robust to common degradations such as compression artifacts, microphone noise, and short query lengths.

## Dataset guidance

To achieve reliable matches you should build the reference database with:

- **High-quality masters** – Prefer lossless (WAV/FLAC) sources or high-bitrate MP3/AAC files to minimize artifacts. Compressed audio is fine as long as it is consistent across the catalog.
- **Representative genres** – Include tracks from the genres you expect to query. Fingerprints are content-specific; jazz improvisations and EDM drops need their own references.
- **Consistent trimming** – Ensure each file begins near the true start of the song. Leading silence skews offset calculations. If exact intros/outros vary by release, include multiple versions with distinct IDs.
- **Mono compatibility** – The pipeline folds stereo channels into mono during processing. If a track relies on hard-panned cues, consider adding both stereo channels separately or widening the peak neighborhood.
- **Metadata manifest** – Maintain a CSV/JSON manifest with `track_id`, `title`, `artist`, and file path. The provided scripts can ingest either directory scans or structured manifests.

A starter dataset of 20–50 tracks is sufficient for experimentation. For more realistic benchmarking, aim for at least 200 tracks covering the genres of interest.

## Project layout

```
Practical/Shazam Clone/
├── README.md                  # High-level overview and dataset notes
├── STORAGE_FORMAT.md          # Fingerprint storage documentation
├── shazam_clone/
│   ├── __init__.py            # Package exports
│   ├── audio_fingerprint.py   # Peak detection, hashing, MinHash utilities
│   ├── database.py            # Fingerprint database management (load/save)
│   └── matching.py            # Query pipeline, microphone/file helpers
├── build_database.py          # CLI: ingest a reference library and persist fingerprints
├── query.py                   # CLI: match a clip from file or microphone capture
└── benchmark.py               # CLI: batch accuracy/runtime evaluation
```

See the CLI scripts for usage examples, and consult `STORAGE_FORMAT.md` when designing custom integrations.

## Usage cheatsheet

```bash
# 1. Build the fingerprint database (either --audio-dir or --manifest)
python "Practical/Shazam Clone/build_database.py" \
    --audio-dir /path/to/reference_library \
    --output data/library.json.gz

# 2. Query a file clip against the database
python "Practical/Shazam Clone/query.py" \
    --database data/library.json.gz \
    --file snippets/chorus.wav --duration 7.5

# 3. Or capture from the default microphone (requires sounddevice)
python "Practical/Shazam Clone/query.py" \
    --database data/library.json.gz \
    --microphone --duration 6

# 4. Run the benchmark harness on labeled clips
python "Practical/Shazam Clone/benchmark.py" \
    --database data/library.json.gz \
    --manifest eval/queries.json \
    --output eval/results.json
```

### Manifest formats

- **Build manifest (`--manifest` in `build_database.py`)** – JSON list where each object includes at least `path` and optional
  `track_id`, `title`, `artist`, and arbitrary metadata keys. Example:

  ```json
  [
    {"path": "library/artist_a/song1.wav", "track_id": "artist_a_song1", "title": "Song 1", "artist": "Artist A"}
  ]
  ```

- **Benchmark manifest (`--manifest` in `benchmark.py`)** – JSON list (or `{ "queries": [...] }`) of query clips with expected
  IDs:

  ```json
  {
    "queries": [
      {"path": "snippets/song1-clip.mp3", "track_id": "artist_a_song1"}
    ]
  }
  ```

These manifests make it easy to regenerate fingerprints or evaluate new models without renaming files.
