import importlib.util
import sys
from pathlib import Path

import pytest

_ = pytest.importorskip("bs4")
from bs4 import BeautifulSoup  # type: ignore  # noqa: E402

MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "Web Page Crawler"
    / "wpc.py"
)

spec = importlib.util.spec_from_file_location("web_page_crawler", MODULE_PATH)
module = importlib.util.module_from_spec(spec)
assert spec and spec.loader
sys.modules[spec.name] = module
spec.loader.exec_module(module)


def test_extract_links_preserves_query_and_params():
    cfg = module.CrawlerConfig(start_url="https://example.com")
    crawler = module.WebCrawler(cfg)
    html = '<a href="/blog;param?page=2#comments">Read</a>'
    soup = BeautifulSoup(html, "html.parser")

    links = list(crawler.extract_links(soup, "https://example.com/articles"))

    assert links == ["https://example.com/blog;param?page=2"]
