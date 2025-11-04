from __future__ import annotations

import sys
import types

import pytest


class _DummyYDL:
    def __init__(self, *_args, **_kwargs):
        pass

    def __enter__(self):
        return self

    def __exit__(self, *_exc_info):
        return False

    def download(self, *_urls):
        return None


_stub_module = types.ModuleType("yt_dlp")
_stub_module.YoutubeDL = _DummyYDL
_stub_module.DownloadError = RuntimeError
sys.modules.setdefault("yt_dlp", _stub_module)


from challenges.Algorithmic.ytmp3 import cringe


@pytest.mark.parametrize("extra_flags", [[], ["--json"]])
def test_main_errors_on_blank_input_file(tmp_path, capsys, extra_flags):
    url_file = tmp_path / "urls.txt"
    url_file.write_text("# comment only\n\n    # indented comment\n", encoding="utf-8")

    with pytest.raises(SystemExit) as excinfo:
        cringe.main(["--input", str(url_file), *extra_flags])

    assert excinfo.value.code == 2
    captured = capsys.readouterr()
    assert "No URLs to download" in captured.err
    assert captured.out == ""


def test_main_collects_urls_once(tmp_path, monkeypatch):
    url_file = tmp_path / "urls.txt"
    url_file.write_text(
        "https://example.com/one\n# comment\nhttps://example.com/two\n",
        encoding="utf-8",
    )

    call_count = {"all_urls": 0}
    original_all_urls = cringe.DownloadConfig.all_urls

    def counting_all_urls(self: cringe.DownloadConfig):
        call_count["all_urls"] += 1
        return original_all_urls(self)

    monkeypatch.setattr(cringe.DownloadConfig, "all_urls", counting_all_urls)

    captured_urls: dict[str, list[str]] = {}

    def fake_batch_download(cfg: cringe.DownloadConfig, urls: list[str]):
        captured_urls["urls"] = list(urls)
        return {
            "backend": "yt-dlp",
            "format": cfg.format,
            "quality": cfg.quality,
            "out_dir": str(cfg.out_dir.resolve()),
            "total": len(urls),
            "success": len(urls),
            "failed": 0,
            "items": [{"url": url, "success": True, "error": None} for url in urls],
        }

    monkeypatch.setattr(cringe, "batch_download", fake_batch_download)

    exit_code = cringe.main(["--input", str(url_file)])

    assert exit_code == 0
    assert call_count["all_urls"] == 1
    assert captured_urls["urls"] == [
        "https://example.com/one",
        "https://example.com/two",
    ]
