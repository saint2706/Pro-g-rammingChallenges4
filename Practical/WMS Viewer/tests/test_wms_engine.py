import io
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from wms_engine import (
    AppConfig,
    LayerConfig,
    MapViewState,
    Projection,
    ServerConfig,
    ViewConfig,
    WMSCache,
    WMSClient,
    load_config,
)


class FakeResponse:
    def __init__(self, content: bytes, status_code: int = 200):
        self.content = content
        self.status_code = status_code

    def raise_for_status(self) -> None:
        if self.status_code >= 400:
            raise RuntimeError("HTTP error")


class WMSEngineTests(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.cache = WMSCache(Path(self.tmp.name))
        self.server = ServerConfig(
            name="Test",
            url="https://example.com/wms",
            layers=[
                LayerConfig(name="foo:bar", title="Foo", styles="", format="image/png")
            ],
        )
        self.layer = self.server.layers[0]
        self.client = WMSClient(self.cache, timeout=1)

    def test_cache_hits_second_request(self) -> None:
        bbox = (-1.0, -1.0, 1.0, 1.0)
        size = (256, 256)
        with mock.patch("requests.get", return_value=FakeResponse(b"abc")) as mocked:
            first = self.client.fetch_map(self.server, self.layer, bbox, size)
            self.assertEqual(first, b"abc")
            second = self.client.fetch_map(self.server, self.layer, bbox, size)
            self.assertEqual(second, b"abc")
            self.assertEqual(
                mocked.call_count, 1, "Expected second call to be served from cache"
            )

    def test_map_view_bbox_and_pan(self) -> None:
        projection = Projection("EPSG:3857")
        view = MapViewState(
            256, 256, projection, zoom=2, center_lon=0.0, center_lat=0.0
        )
        bbox = view.bounding_box()
        expected_half = view.resolution * view.width / 2
        self.assertAlmostEqual(bbox[0], -expected_half, places=3)
        self.assertAlmostEqual(bbox[2], expected_half, places=3)
        view.pan_pixels(10, -5)
        bbox_after = view.bounding_box()
        self.assertNotEqual(bbox, bbox_after)

    def test_load_config_from_yaml(self) -> None:
        yaml_text = """
view:
  width: 400
  height: 300
servers:
  - name: Test Server
    url: https://example.com/wms
    layers:
      - name: foo
"""
        config_path = Path(self.tmp.name) / "config.yaml"
        config_path.write_text(yaml_text, encoding="utf-8")
        config = load_config(config_path)
        self.assertIsInstance(config, AppConfig)
        self.assertEqual(config.view.width, 400)
        self.assertEqual(config.servers[0].layers[0].name, "foo")

    def test_build_params_wms_1_3_axis_order(self) -> None:
        server = ServerConfig(
            name="Test",
            url="https://example.com/wms",
            version="1.3.0",
            crs="EPSG:4326",
            layers=[LayerConfig(name="foo")],
        )
        bbox = (-1.0, -1.0, 1.0, 1.0)
        params = self.client.build_params(server, server.layers[0], bbox, (256, 256))
        self.assertIn("crs", params)
        self.assertNotIn("srs", params)


if __name__ == "__main__":
    unittest.main()
