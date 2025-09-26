import base64
import importlib
import io
import sys
from pathlib import Path

import pytest

MODULE_NAME = "Practical.Imageboard.imageboard"
PNG_DATA = base64.b64decode(
    b"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mP8/x8AAusB9YkXUsgAAAAASUVORK5CYII="
)


@pytest.fixture()
def imageboard_module(tmp_path, monkeypatch):
    data_dir = tmp_path / "data"
    monkeypatch.setenv("IMAGEBOARD_DATA_DIR", str(data_dir))
    monkeypatch.setenv("IMAGEBOARD_SECRET_KEY", "testing-secret")
    monkeypatch.setenv("IMAGEBOARD_ADMIN_PASSWORD", "letmein")
    repo_root = Path(__file__).resolve().parents[3]
    if str(repo_root) not in sys.path:
        sys.path.insert(0, str(repo_root))
    if MODULE_NAME in sys.modules:
        del sys.modules[MODULE_NAME]
    module = importlib.import_module(MODULE_NAME)
    module.app.config.update(TESTING=True)
    module.CFG.rate_limit_window = 0
    module.init_db()
    return module


@pytest.fixture()
def client(imageboard_module):
    with imageboard_module.app.test_client() as client:
        yield client


def test_format_post_quotes_and_greentext(imageboard_module):
    rendered = imageboard_module.format_post_filter(
        ">hello\n>>12 reference"
    )
    html = str(rendered)
    assert "class='gt'" in html
    assert "href='#p12'" in html


def test_thread_and_reply_flow(client, imageboard_module):
    # Create a thread with an image
    response = client.post(
        "/create",
        data={
            "message": "OP message",
            "image": (io.BytesIO(PNG_DATA), "seed.png"),
        },
        content_type="multipart/form-data",
    )
    assert response.status_code == 302

    threads = imageboard_module.fetch_threads()
    assert len(threads) == 1
    thread_id = threads[0]["id"]

    # Reply quoting the thread id
    reply_response = client.post(
        f"/reply/{thread_id}",
        data={"message": f">>{thread_id} replying"},
        content_type="multipart/form-data",
        follow_redirects=True,
    )
    assert reply_response.status_code == 200

    page = client.get(f"/thread/{thread_id}")
    html = page.data.decode()
    assert f"href='#p{thread_id}'" in html
    assert "replying" in html


def test_admin_can_delete_posts(client, imageboard_module):
    # Prime a thread
    client.post(
        "/create",
        data={"message": "cleanup"},
        content_type="multipart/form-data",
    )
    thread_id = imageboard_module.fetch_threads()[0]["id"]

    login = client.post(
        "/admin/login",
        data={"username": imageboard_module.CFG.admin_username, "password": "letmein"},
        follow_redirects=True,
    )
    assert b"Logged in" in login.data

    delete_resp = client.post(
        f"/admin/delete/{thread_id}",
        data={"next": "/"},
        follow_redirects=True,
    )
    assert b"Post deleted" in delete_resp.data
    assert imageboard_module.fetch_threads() == []
