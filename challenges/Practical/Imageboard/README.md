# Minimal Imageboard (Practical Challenge)

A compact, Vichan-inspired anonymous imageboard built with Flask, SQLite, and Pillow. Threads support optional image uploads, greentext/quote formatting, and thumbnail previews. A lightweight moderation console enables administrators to prune abusive content directly from the UI.

## Features

- Threaded discussions with replies, image uploads, and auto-generated thumbnails.
- Greentext (`>words`) and quote-linking (`>>123`) rendered via a custom Jinja filter.
- SQLite persistence with automatic schema creation and cascading deletes.
- Basic rate limiting per IP address to curb spam bursts.
- Password-protected admin controls (login/logout + inline delete actions).
- Configurable data directory, board title, allowed extensions, and secrets via environment variables.
- Unit tests covering quoting/formatting, posting flows, and admin moderation.

## Quick Start (Development)

```bash
cd challenges/Practical/Imageboard
python -m venv .venv
source .venv/bin/activate
pip install -r ../requirements.txt  # Flask + Pillow are the primary needs
python imageboard.py --init-db
python imageboard.py --host 0.0.0.0 --port 5000 --debug
```

Visit `http://127.0.0.1:5000/` and create your first thread. Uploaded media + the SQLite database live under `data/` (overridable; see below).

### Configuration knobs

Set the following environment variables before launching to customise behaviour:

| Variable | Purpose | Default |
|----------|---------|---------|
| `IMAGEBOARD_DATA_DIR` | Storage root for `imageboard.db`, uploads, and thumbnails. | `<repo>/challenges/Practical/Imageboard/data` |
| `IMAGEBOARD_SECRET_KEY` | Flask session secret (change for production!). | `dev-secret-change-me` |
| `IMAGEBOARD_BOARD_TITLE` | Header text shown on every page. | `Minimal Imageboard` |
| `IMAGEBOARD_ALLOWED_EXT` | Comma-separated allowed image extensions. | `jpg,jpeg,png,gif,webp` |
| `IMAGEBOARD_ADMIN_USERNAME` | Admin login username. | `admin` |
| `IMAGEBOARD_ADMIN_PASSWORD` | Admin password (hashed automatically on boot). | `changeme` |
| `IMAGEBOARD_ADMIN_PASSWORD_HASH` | Pre-generated Werkzeug hash (overrides plaintext password). | _unset_ |

> Tip: for container/cloud deploys mount a persistent volume at `IMAGEBOARD_DATA_DIR` so uploads survive restarts.

## Moderation & Admin Console

Visit `/admin/login` and authenticate with the configured credentials to reveal inline delete controls beside every thread or reply. Deleting a thread also prunes all of its replies and associated files. Use `/admin/logout` to end the session.

## Deployment Notes

- Generate strong secrets (`IMAGEBOARD_SECRET_KEY`) and admin credentials before exposing publicly.
- Place the app behind a production WSGI server such as Gunicorn or uWSGI. Example:
  ```bash
  IMAGEBOARD_SECRET_KEY=$(openssl rand -hex 32) \
  IMAGEBOARD_ADMIN_PASSWORD="super-secret" \
  IMAGEBOARD_DATA_DIR=/var/lib/imageboard \
  gunicorn --chdir challenges/Practical/Imageboard imageboard:app --bind 0.0.0.0:8000
  ```
- Configure a reverse proxy (nginx/Caddy) to serve `/uploads` and `/thumbs` via the Flask routes or directly from the data directory.
- Enable HTTPS and consider extra defences for real deployments (CAPTCHA, IP bans, post limits, etc.).

## Testing

Pytest-based regression tests verify quoting behaviour, posting flows, and admin moderation:

```bash
pytest challenges/Practical/Imageboard/tests -q
```

## Screenshot

![Thread list and reply demo](docs/demo.png)

## Limitations / Future Work

- No user accounts or CAPTCHA — vulnerable to automated spam.
- Thumbnails are JPEG-only; extend as needed for transparency preservation.
- File validation is intentionally lightweight; consider a full MIME/type checker for hardened environments.
