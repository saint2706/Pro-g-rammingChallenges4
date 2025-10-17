# Booru Client

A practical API client for browsing and downloading images from Booru-style imageboards. This tool focuses on fan-art friendly boards that expose JSON APIs, with first-class support for:

- **[Danbooru](https://danbooru.donmai.us/)** – REST-style JSON API with advanced tag queries, paging, and rating filters.
- **[Gelbooru](https://gelbooru.com/)** – Classic DAPI endpoint supporting JSON output and tag searches compatible with Danbooru syntax.
- **[Safebooru](https://safebooru.org/)** – A Gelbooru-compatible fork that shares the same API surface.
- **Any Booru that implements the [Danbooru JSON schema](https://danbooru.donmai.us/wiki_pages/help:api) or the [Gelbooru DAPI](https://gelbooru.com/index.php?page=help&topic=dapi) can be configured via `config.example.toml`.

## Features

- Unified `BooruClient` abstraction normalises posts across APIs (id, rating, tags, URLs).
- Search with pagination, rating filters, and arbitrary tag combinations.
- Built-in download manager with checksum tagging metadata (JSON sidecar files).
- Local JSON cache keeps recent search responses for offline browsing and to stay within rate limits.
- Token bucket rate limiter guards API usage (default: 1 request/sec, customisable per site).
- CLI (`python cli.py ...`) for quick searches and scripted downloads.
- Tkinter GUI (`python gui.py`) for thumbnail browsing, metadata inspection, and bulk downloads.
- Extensible configuration driven by TOML/YAML so you can add additional Boorus without touching code.

## Usage Overview

```bash
# Search Danbooru for safe-rated posts tagged "landscape" and "clouds"
python cli.py search --booru danbooru --tags "landscape clouds" --rating safe --limit 10

# Download the first 3 results to ./downloads and write metadata sidecars
python cli.py search --booru danbooru --tags "sky" --limit 3 --download ./downloads

# Start the GUI browser (uses cached searches when available)
python gui.py --booru gelbooru
```

See [`config.example.toml`](./config.example.toml) for reusable snippets covering authentication tokens, custom rate limits, and board-specific query parameters.

## Development & Tests

Install dependencies from the Practical suite, then run pytest with mocked responses:

```bash
pip install -r ../requirements.txt
pytest
```

The included tests exercise pagination, caching, and download metadata without touching the live APIs.
