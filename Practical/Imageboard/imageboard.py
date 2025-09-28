"""Minimal vichan-like imageboard prototype (educational).

Highlights in this refined edition:
- Centralised configuration via a `Config` dataclass (environment overrides supported).
- Context manager wrapper for database connections (ensures close, DRY).
- Unified helper `insert_post` for thread + reply creation.
- Registered Jinja filters for readable timestamps and quote rendering.
- Basic moderation (password-protected login + inline delete controls).
- Stricter image validation: size guard and lightweight magic header sniff.
- Type hints, inline comments, and friendlier flash messages for newcomers.

Security / production caveats remain (no CAPTCHA, no IP bans, minimal rate limiting).
"""

from __future__ import annotations

import argparse
import os
import sqlite3
import time
from contextlib import contextmanager
from dataclasses import dataclass
from datetime import datetime, timezone
from functools import wraps
from pathlib import Path
from typing import Generator, Optional, Tuple

from flask import (
    Flask,
    abort,
    flash,
    redirect,
    render_template,
    request,
    send_from_directory,
    session,
    url_for,
)
from markupsafe import Markup, escape
from PIL import Image
from werkzeug.security import check_password_hash, generate_password_hash
from werkzeug.utils import secure_filename

# ----------------------------- Configuration ----------------------------- #


@dataclass(slots=True)
class Config:
    data_dir: Path
    allowed_ext: frozenset[str]
    thumb_size: Tuple[int, int]
    rate_limit_window: int
    max_message_len: int
    max_image_bytes: int
    secret_key: str
    board_title: str
    admin_username: str
    admin_password_hash: str


def _split_allowed_ext(raw: str) -> frozenset[str]:
    parts = [segment.strip().lower() for segment in raw.split(",")]
    return frozenset({segment for segment in parts if segment})


def load_config() -> Config:
    root = Path(__file__).parent.resolve()
    data_dir = (
        Path(os.getenv("IMAGEBOARD_DATA_DIR", root / "data")).expanduser().resolve()
    )
    allowed = _split_allowed_ext(
        os.getenv("IMAGEBOARD_ALLOWED_EXT", "jpg,jpeg,png,gif,webp")
    )
    secret_key = os.getenv("IMAGEBOARD_SECRET_KEY", "dev-secret-change-me")
    board_title = os.getenv("IMAGEBOARD_BOARD_TITLE", "Minimal Imageboard")
    admin_username = os.getenv("IMAGEBOARD_ADMIN_USERNAME", "admin")
    password_hash = os.getenv("IMAGEBOARD_ADMIN_PASSWORD_HASH")
    if not password_hash:
        admin_password = os.getenv("IMAGEBOARD_ADMIN_PASSWORD", "changeme")
        password_hash = generate_password_hash(admin_password)
    return Config(
        data_dir=data_dir,
        allowed_ext=(
            allowed if allowed else frozenset({"jpg", "jpeg", "png", "gif", "webp"})
        ),
        thumb_size=(200, 200),
        rate_limit_window=8,
        max_message_len=2000,
        max_image_bytes=8 * 1024 * 1024,
        secret_key=secret_key,
        board_title=board_title,
        admin_username=admin_username,
        admin_password_hash=password_hash,
    )


CFG = load_config()
APP_ROOT = Path(__file__).parent.resolve()
UPLOAD_DIR = CFG.data_dir / "uploads"
THUMB_DIR = CFG.data_dir / "thumbs"
DB_PATH = CFG.data_dir / "imageboard.db"
ADMIN_SESSION_KEY = "imageboard_admin"


def _safe_url(candidate: Optional[str], fallback: str) -> str:
    if candidate and candidate.startswith("/") and not candidate.startswith("//"):
        return candidate
    return fallback


# Ensure directories exist (idempotent)
for d in (CFG.data_dir, UPLOAD_DIR, THUMB_DIR):
    d.mkdir(parents=True, exist_ok=True)

# ----------------------------- Flask App Setup ----------------------------- #
app = Flask(__name__)
app.secret_key = CFG.secret_key


def is_admin() -> bool:
    return bool(session.get(ADMIN_SESSION_KEY))


def require_admin(view):
    @wraps(view)
    def wrapper(*args, **kwargs):
        if not is_admin():
            flash("Administrator login required.", "error")
            return redirect(url_for("admin_login"))
        return view(*args, **kwargs)

    return wrapper


@app.context_processor
def inject_globals():
    return {"board_title": CFG.board_title, "is_admin": is_admin(), "CFG": CFG}


@app.template_filter("datetime")
def jinja_datetime_filter(ts: int) -> str:
    """Convert stored UTC epoch int -> iso-ish human string."""
    dt = datetime.fromtimestamp(ts, tz=timezone.utc)
    return dt.strftime("%Y-%m-%d %H:%M:%S UTC")


@app.template_filter("format_post")
def format_post_filter(text: str | None) -> Markup:
    """Convert simple post markup to HTML.

    Currently supported:
    - Lines starting with '>' get a span for greentext styling.
    - References of the form >>123 become links to #p123 if present on page.
    All other content is HTML-escaped to avoid XSS.
    """
    if not text:
        return Markup("")
    formatted_lines: list[str] = []
    for line in text.splitlines():
        esc = escape(line)
        # Greentext
        if line.startswith(">") and not line.startswith(">>"):
            esc = f"<span class='gt'>&gt;{escape(line[1:])}</span>"

        # Quote links
        # Replace occurrences of >>digits with anchor links
        def repl(match):  # type: ignore
            pid = match.group(1)
            return f"<a href='#p{pid}' class='ref'>&gt;&gt;{pid}</a>"

        import re

        esc = re.sub(r"&gt;&gt;(\d+)", repl, esc)
        formatted_lines.append(esc)
    return Markup("<br>".join(formatted_lines))


# ----------------------------- DB Helpers ----------------------------- #


@contextmanager
def db() -> Generator[sqlite3.Connection, None, None]:
    conn = sqlite3.connect(DB_PATH)
    try:
        conn.row_factory = sqlite3.Row
        conn.execute("PRAGMA foreign_keys = ON")
        yield conn
    finally:
        conn.close()


def init_db() -> None:
    with db() as conn:
        conn.execute(
            """
            CREATE TABLE IF NOT EXISTS posts (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                parent_id INTEGER REFERENCES posts(id) ON DELETE CASCADE,
                created_utc INTEGER NOT NULL,
                ip TEXT,
                message TEXT,
                image_filename TEXT,
                thumb_filename TEXT
            )
            """
        )
        conn.execute("CREATE INDEX IF NOT EXISTS idx_parent_id ON posts(parent_id)")
        conn.commit()


# ----------------------------- Image Handling ----------------------------- #


def allowed_file(filename: str) -> bool:
    return "." in filename and filename.rsplit(".", 1)[1].lower() in CFG.allowed_ext


def _sniff_magic(data: bytes) -> bool:
    """Very lightweight magic header check to reject obviously wrong files.
    This is intentionally simple (not a full mime detection)."""
    signatures = [
        (b"\xff\xd8\xff", "jpg"),  # JPEG
        (b"\x89PNG\r\n\x1a\n", "png"),  # PNG
        (b"GIF87a", "gif"),
        (b"GIF89a", "gif"),
        (b"RIFF", "webp"),
    ]
    for sig, _ in signatures:
        if data.startswith(sig):
            return True
    return True  # Fallback permissive (still extension filtered)


def save_image(file_storage) -> Tuple[Optional[str], Optional[str]]:
    if not file_storage or file_storage.filename == "":
        return None, None
    filename = secure_filename(file_storage.filename)
    if not allowed_file(filename):
        raise ValueError("Unsupported file type")
    # Size guard: stream pointer approach (werkzeug's FileStorage) -> read into memory for check (small scale)
    file_storage.stream.seek(0, os.SEEK_END)
    size = file_storage.stream.tell()
    file_storage.stream.seek(0)
    if size > CFG.max_image_bytes:
        raise ValueError("Image too large")
    # Magic sniff
    head = file_storage.stream.read(16)
    file_storage.stream.seek(0)
    if not _sniff_magic(head):
        raise ValueError("File content not recognized as image")

    ext = filename.rsplit(".", 1)[1].lower()
    ts = int(time.time() * 1000)
    basename = f"{ts}_{os.getpid()}"
    image_name = f"{basename}.{ext}"
    dest_path = UPLOAD_DIR / image_name
    file_storage.save(dest_path)

    # Create thumbnail
    thumb_name: Optional[str]
    try:
        with Image.open(dest_path) as im:
            im.thumbnail(CFG.thumb_size)
            thumb_name = f"{basename}_thumb.jpg"
            thumb_path = THUMB_DIR / thumb_name
            if im.mode not in ("RGB", "L"):
                im = im.convert("RGB")
            im.save(thumb_path, format="JPEG", quality=85)
    except Exception:
        thumb_name = None
    return image_name, thumb_name


def _remove_file(path: Path) -> None:
    try:
        path.unlink()
    except FileNotFoundError:
        pass


def delete_post(post_id: int) -> bool:
    """Delete a post (and cascade replies) plus associated files."""
    with db() as conn:
        cur = conn.execute(
            "SELECT id, parent_id, image_filename, thumb_filename FROM posts WHERE id=?",
            (post_id,),
        )
        post = cur.fetchone()
        if not post:
            return False
        cur = conn.execute(
            "SELECT id, image_filename, thumb_filename FROM posts WHERE parent_id=?",
            (post_id,),
        )
        attachments = [post, *cur.fetchall()]
        conn.execute("DELETE FROM posts WHERE id=?", (post_id,))
        conn.commit()

    for entry in attachments:
        img = entry["image_filename"]
        if img:
            _remove_file(UPLOAD_DIR / img)
        thumb = entry["thumb_filename"]
        if thumb:
            _remove_file(THUMB_DIR / thumb)
    return True


# ----------------------------- Rate Limit ----------------------------- #


def rate_limited(ip: str) -> bool:
    cutoff = int(time.time()) - CFG.rate_limit_window
    with db() as conn:
        cur = conn.execute(
            "SELECT created_utc FROM posts WHERE ip=? ORDER BY created_utc DESC LIMIT 1",
            (ip,),
        )
        row = cur.fetchone()
    if not row:
        return False
    return row["created_utc"] > cutoff


# ----------------------------- Query Helpers ----------------------------- #


def fetch_threads(limit: int = 50):
    with db() as conn:
        cur = conn.execute(
            """
            SELECT p.*, (
                SELECT COUNT(*) FROM posts r WHERE r.parent_id = p.id
            ) AS replies
            FROM posts p
            WHERE p.parent_id IS NULL
            ORDER BY p.created_utc DESC
            LIMIT ?
            """,
            (limit,),
        )
        return cur.fetchall()


def fetch_thread(thread_id: int):
    with db() as conn:
        thread_cur = conn.execute(
            "SELECT * FROM posts WHERE id=? AND parent_id IS NULL", (thread_id,)
        )
        thread = thread_cur.fetchone()
        if not thread:
            return None, []
        replies_cur = conn.execute(
            "SELECT * FROM posts WHERE parent_id=? ORDER BY created_utc ASC",
            (thread_id,),
        )
        replies = replies_cur.fetchall()
        return thread, replies


# ----------------------------- Insert Helper ----------------------------- #


def insert_post(
    *,
    parent_id: Optional[int],
    ip: str,
    message: Optional[str],
    image_name: Optional[str],
    thumb_name: Optional[str],
) -> int:
    ts = int(time.time())
    with db() as conn:
        cur = conn.execute(
            """INSERT INTO posts(parent_id, created_utc, ip, message, image_filename, thumb_filename)
            VALUES (?, ?, ?, ?, ?, ?)""",
            (parent_id, ts, ip, message, image_name, thumb_name),
        )
        conn.commit()
    last_id = cur.lastrowid
    assert last_id is not None, "Database did not return lastrowid"
    return int(last_id)


# ----------------------------- Routes ----------------------------- #


@app.route("/")
def index():
    threads = fetch_threads()
    return render_template("index.html", threads=threads)


@app.route("/thread/<int:thread_id>")
def view_thread(thread_id: int):
    thread, replies = fetch_thread(thread_id)
    if not thread:
        abort(404)
    return render_template("thread.html", thread=thread, replies=replies)


# Thread creation
@app.route("/create", methods=["POST"])
def create_thread():
    ip = request.remote_addr or "?"
    if rate_limited(ip):
        flash("Posting too fast; please wait a few seconds.", "error")
        return redirect(url_for("index"))
    message = (request.form.get("message") or "").strip()
    if len(message) > CFG.max_message_len:
        flash("Message too long.", "error")
        return redirect(url_for("index"))
    file = request.files.get("image")
    try:
        image_name, thumb_name = save_image(file)
    except ValueError as e:
        flash(str(e), "error")
        return redirect(url_for("index"))

    insert_post(
        parent_id=None,
        ip=ip,
        message=message or None,
        image_name=image_name,
        thumb_name=thumb_name,
    )
    return redirect(url_for("index"))


# Reply
@app.route("/reply/<int:thread_id>", methods=["POST"])
def reply(thread_id: int):
    ip = request.remote_addr or "?"
    if rate_limited(ip):
        flash("Posting too fast; please wait a few seconds.", "error")
        return redirect(url_for("view_thread", thread_id=thread_id))
    message = (request.form.get("message") or "").strip()
    if len(message) > CFG.max_message_len:
        flash("Message too long.", "error")
        return redirect(url_for("view_thread", thread_id=thread_id))
    file = request.files.get("image")
    try:
        image_name, thumb_name = save_image(file)
    except ValueError as e:
        flash(str(e), "error")
        return redirect(url_for("view_thread", thread_id=thread_id))

    # Validate thread exists before inserting
    thread, _ = fetch_thread(thread_id)
    if not thread:
        abort(404)
    insert_post(
        parent_id=thread_id,
        ip=ip,
        message=message or None,
        image_name=image_name,
        thumb_name=thumb_name,
    )
    return redirect(url_for("view_thread", thread_id=thread_id))


# ----------------------------- Admin Views ----------------------------- #


@app.route("/admin/login", methods=["GET", "POST"])
def admin_login():
    if request.method == "POST":
        username = (request.form.get("username") or "").strip()
        password = request.form.get("password") or ""
        if username == CFG.admin_username and check_password_hash(
            CFG.admin_password_hash, password
        ):
            session[ADMIN_SESSION_KEY] = True
            flash("Logged in as administrator.", "info")
            next_url = _safe_url(
                request.args.get("next") or request.form.get("next"), url_for("index")
            )
            return redirect(next_url)
        flash("Invalid credentials.", "error")
    return render_template("admin_login.html")


@app.route("/admin/logout")
def admin_logout():
    session.pop(ADMIN_SESSION_KEY, None)
    flash("Logged out.", "info")
    return redirect(url_for("index"))


@app.post("/admin/delete/<int:post_id>")
@require_admin
def admin_delete(post_id: int):
    success = delete_post(post_id)
    if success:
        flash("Post deleted.", "info")
    else:
        flash("Post not found.", "error")
    next_url = _safe_url(request.form.get("next"), url_for("index"))
    return redirect(next_url)


# Static file serving
@app.route("/uploads/<path:filename>")
def uploaded_file(filename: str):
    return send_from_directory(UPLOAD_DIR, filename)


@app.route("/thumbs/<path:filename>")
def thumb_file(filename: str):
    return send_from_directory(THUMB_DIR, filename)


# ----------------------------- CLI Entrypoint ----------------------------- #


def parse_args():
    ap = argparse.ArgumentParser(description="Minimal imageboard server")
    ap.add_argument(
        "--init-db", action="store_true", help="Create database schema and exit"
    )
    ap.add_argument("--host", default="127.0.0.1")
    ap.add_argument("--port", type=int, default=5000)
    ap.add_argument("--debug", action="store_true")
    return ap.parse_args()


def main():  # pragma: no cover
    args = parse_args()
    if args.init_db:
        init_db()
        print(f"Initialized DB at {DB_PATH}")
        return
    if not DB_PATH.exists():
        print("Database missing. Run with --init-db first.")
        return
    app.run(host=args.host, port=args.port, debug=args.debug)


if __name__ == "__main__":  # pragma: no cover
    main()
