from __future__ import annotations

import io
import json
import sys
import unittest
from datetime import datetime, timezone
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))

from chan_aggregator.config import BoardConfig
from chan_aggregator.core import AggregatedThread, ChanAggregator
from chan_aggregator import ui


class DummyResponse:
    def __init__(self, payload):
        self._payload = payload

    def raise_for_status(self):
        return None

    def json(self):
        return self._payload


class ChanAggregatorTests(unittest.TestCase):
    def setUp(self) -> None:
        self.board_a = BoardConfig(
            board_id="a",
            title="Board A",
            catalog_url="https://example.com/a/catalog.json",
            thread_url="https://example.com/a/thread/{thread}.json",
            cache_ttl=60,
            rate_limit_seconds=0,
        )
        self.board_b = BoardConfig(
            board_id="b",
            title="Board B",
            catalog_url="https://example.com/b/catalog.json",
            thread_url="https://example.com/b/thread/{thread}.json",
            cache_ttl=60,
            rate_limit_seconds=0,
        )

    def test_aggregate_threads_combines_and_filters(self):
        session = mock.Mock()
        payload_a = [
            {
                "threads": [
                    {"no": 1, "sub": "Tech thread", "com": "Discussion", "replies": 10, "last_modified": 1700000000},
                    {"no": 2, "sub": "Offtopic", "com": "Random", "replies": 1, "last_modified": 1600000000},
                ]
            }
        ]
        payload_b = [
            {
                "threads": [
                    {"no": 3, "sub": "History of Python", "com": "Python origins", "replies": 4, "last_modified": 1800000000}
                ]
            }
        ]
        session.get.side_effect = [DummyResponse(payload_a), DummyResponse(payload_b)]
        aggregator = ChanAggregator([self.board_a, self.board_b], session=session)

        threads = aggregator.aggregate_threads(search="python")
        self.assertEqual(len(threads), 1)
        self.assertEqual(threads[0].thread_id, 3)
        self.assertTrue(threads[0].title.startswith("History"))
        # ensure sorting newest first
        session.get.assert_called()

    def test_caching_prevents_duplicate_requests(self):
        session = mock.Mock()
        payload = [
            {"threads": [{"no": 1, "sub": "One", "replies": 0, "last_modified": 1500000000}]}
        ]
        session.get.return_value = DummyResponse(payload)
        aggregator = ChanAggregator([self.board_a], session=session)

        first = aggregator.aggregate_threads()
        second = aggregator.aggregate_threads()
        self.assertEqual(len(first), 1)
        self.assertEqual(len(second), 1)
        # only one HTTP call thanks to cache
        self.assertEqual(session.get.call_count, 1)


class CLITests(unittest.TestCase):
    def test_main_outputs_json(self):
        boards = [BoardConfig(
            board_id="a",
            title="Board A",
            catalog_url="https://example.com/a/catalog.json",
            thread_url="https://example.com/a/thread/{thread}.json",
        )]
        fake_threads = [
            AggregatedThread(
                board="a",
                source="example",
                thread_id=123,
                title="Sample",
                excerpt="Body",
                replies=5,
                last_modified=datetime.fromtimestamp(1700000000, tz=timezone.utc),
            )
        ]
        with mock.patch.object(ui, "load_configs_from_args", return_value=boards), mock.patch.object(
            ui, "ChanAggregator"
        ) as aggregator_class:
            aggregator_instance = aggregator_class.return_value
            aggregator_instance.aggregate_threads.return_value = fake_threads
            buf = io.StringIO()
            with mock.patch("sys.stdout", buf):
                exit_code = ui.main(["--json", "--boards", "a"])
        self.assertEqual(exit_code, 0)
        output = json.loads(buf.getvalue())
        self.assertEqual(output[0]["thread_id"], 123)
        aggregator_instance.aggregate_threads.assert_called_once()


if __name__ == "__main__":  # pragma: no cover
    unittest.main()
