# Fingerprint storage format

The fingerprint database is persisted as a compressed JSON file so that it stays readable and portable. The top-level keys are:

```json
{
  "version": 1,
  "config": {
    "sample_rate": 22050,
    "n_fft": 4096,
    "hop_length": 512,
    "fan_value": 15,
    "min_hashes": 32
  },
  "tracks": {
    "track_id": {
      "title": "Song title",
      "artist": "Artist",
      "duration": 187.42,
      "path": "relative/or/absolute/path/to/audio",
      "minhash": [123456, 789012, ...]
    }
  },
  "hash_buckets": {
    "hash_value": [
      ["track_id", 12.56],
      ["another_track", 42.13]
    ]
  }
}
```

- **`config`** mirrors the parameters used to generate fingerprints. If you change any item, rebuild the database to prevent mismatches.
- **`tracks`** holds metadata for each reference track. The `minhash` array is the fixed-length signature derived from all fingerprints of that track. Clients can use it for candidate pruning or similarity searches.
- **`hash_buckets`** maps the integer fingerprint hash (stored as a string for JSON compatibility) to a list of `(track_id, time_offset)` pairs. The offset is expressed in seconds relative to the start of the track.

The on-disk format is written with UTF-8 JSON and compressed via gzip. The helper class `FingerprintDatabase` handles serialization/deserialization; you rarely need to touch the raw structure manually. For large catalogs you may wish to migrate to SQLite or a key–value store, but the JSON layout remains a useful interchange format.
