# Chan Aggregator

A command line tool that unifies multiple imageboard/chan-style thread APIs into a single searchable stream.

## Supported sources

Out of the box the aggregator knows how to talk to two popular 4chan-style endpoints:

| Key | Source | Notes |
| --- | ------ | ----- |
| `4chan` | Official 4chan JSON API (`https://a.4cdn.org/{board}/catalog.json`, `https://a.4cdn.org/{board}/thread/{thread}.json`) | Stable, live data. Subject and comment text are HTML encoded. |
| `4plebs` | 4plebs archive API (`https://archive.4plebs.org/{board}/catalog.json`, `https://archive.4plebs.org/_/api/chan/thread/?board={board}&num={thread}`) | Historical archive, slower to update but keeps deleted threads. |

Each source can expose several boards. The default configuration covers a handful of general interest boards (technology, comics, /g/ classics) but you can add as many as you like.

## Features

- Fetches thread metadata and the original post for every configured board.
- Combines, sorts and deduplicates threads from multiple boards.
- Simple keyword filtering across subjects and OP comments.
- Lightweight rate limiting (per-board) and time-to-live caching to respect API limits.
- CLI output with compact table layout and optional JSON dump for scripting.
- Modular Python package – reuse the backend classes in other projects.

## Installation

Inside the repository root:

```bash
python -m venv .venv
source .venv/bin/activate
pip install -r Practical/requirements.txt
```

All dependencies (standard library + `requests`) are already listed in the consolidated requirements file.

## Configuration

Boards and archives are described by simple JSON files. The CLI looks for `boards.json` in the working directory by default, but you can point it to a different file with `--config`. A ready-to-edit `boards.example.json` is included in this folder. An entry looks like this:

```json
{
  "sources": [
    {
      "name": "4chan",
      "boards": [
        {
          "board_id": "g",
          "title": "Technology",
          "catalog_url": "https://a.4cdn.org/{board}/catalog.json",
          "thread_url": "https://a.4cdn.org/{board}/thread/{thread}.json",
          "rate_limit_seconds": 2,
          "cache_ttl": 120
        }
      ]
    }
  ]
}
```

Tips:

- `{board}` and `{thread}` placeholders are replaced automatically.
- `rate_limit_seconds` defaults to 2 seconds between calls per board.
- `cache_ttl` (in seconds) avoids hitting the same endpoint repeatedly.
- Missing files fall back to a small built-in preset so you can start immediately.

## Usage

```bash
cd "Practical/Chan Aggregator"
python -m chan_aggregator --boards g,his --limit 10 --search "python"
```

Key flags:

- `--boards`: comma separated board IDs. Leave empty for all configured boards.
- `--limit`: maximum number of threads to display.
- `--search`: substring filter (case insensitive) applied to subject, comment and board title.
- `--json`: emit machine readable JSON instead of a table.

Example output:

```
Board   Thread  Replies  Last Activity        Subject
/g/     1234567 42       2024-06-22 14:33 UTC  Python threading tools
```

## Running tests

Tests rely on `unittest` and mocked HTTP calls so they run offline:

```bash
python -m unittest discover Practical/Chan\ Aggregator/tests
```

## Extending

- Add new archive providers by implementing a `BoardConfig` entry with catalog/thread URLs.
- Plug the backend classes into a richer TUI or web front-end – the `aggregate_threads` method already returns structured data.
- Wire up a scheduler to periodically refresh the cache and push updates to other systems.

Happy hacking!
