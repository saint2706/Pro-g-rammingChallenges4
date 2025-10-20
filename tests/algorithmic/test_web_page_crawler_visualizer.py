import json
import importlib.util
import sys
from pathlib import Path

import pytest

MODULE_PATH = (
    Path(__file__).resolve().parents[2]
    / "challenges"
    / "Algorithmic"
    / "Web Page Crawler"
    / "crawler_visualizer.py"
)

spec = importlib.util.spec_from_file_location("crawler_visualizer", MODULE_PATH)
module = importlib.util.module_from_spec(spec)
assert spec and spec.loader
sys.modules[spec.name] = module
spec.loader.exec_module(module)


class DummyConfig:
    start_url = "https://example.com"


class DummyCrawler:
    def __init__(self) -> None:
        self.edges = [
            ("https://example.com", "https://example.com/about"),
            ("https://example.com", "https://example.com/blog"),
            ("https://example.com/blog", "https://example.com/contact"),
            ("https://example.com/blog", "https://example.com"),
        ]
        self.errors = {
            "https://example.com/blog": "HTTPError",
            "__state__": "persist_failed",
        }
        self.cfg = DummyConfig()


def test_metadata_consistency_from_crawler(tmp_path):
    crawler = DummyCrawler()
    graph, metadata = module.build_graph_from_source(crawler)

    assert metadata.root == crawler.cfg.start_url
    assert metadata.graph["node_count"] == 4
    assert metadata.graph["edge_count"] == 4
    assert metadata.graph["error_count"] == 1

    nodes = {item["id"]: item for item in metadata.nodes}
    assert nodes["https://example.com"]["depth"] == 0
    assert nodes["https://example.com/about"]["in_degree"] == 1
    assert nodes["https://example.com/blog"]["has_error"] is True
    assert nodes["https://example.com/contact"]["depth"] == 2

    payload = {
        "start_url": crawler.cfg.start_url,
        "edges": [[src, dst] for src, dst in crawler.edges],
        "errors": crawler.errors,
    }
    json_path = tmp_path / "crawl.json"
    json_path.write_text(json.dumps(payload), encoding="utf-8")

    graph_json, metadata_json = module.build_graph_from_source(json_path)

    assert metadata_json.nodes == metadata.nodes
    assert metadata_json.links == metadata.links
    assert metadata_json.graph == metadata.graph

    figure = module.build_plotly_figure(graph, metadata)
    output_path = module.export_html(figure, tmp_path / "viz.html")
    assert output_path.exists()


def test_edges_only_requires_root():
    edges = [["https://example.com", "https://example.com/blog"]]
    graph, metadata = module.build_graph_from_source(edges, root_url="https://example.com")
    assert metadata.root == "https://example.com"
    assert graph.number_of_edges() == 1

    with pytest.raises(ValueError):
        module.build_graph_from_source(edges)
