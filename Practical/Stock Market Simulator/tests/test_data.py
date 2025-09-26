from __future__ import annotations

import datetime as dt
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import sys

PACKAGE_ROOT = Path(__file__).resolve().parents[1]
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))

import pandas as pd

from stock_simulator.data import YahooFinanceDataClient


class FakeResponse:
    def __init__(self, text: str, status_code: int = 200) -> None:
        self.text = text
        self.status_code = status_code
        self.reason = "OK"

    def json(self):  # pragma: no cover - compatibility only
        raise NotImplementedError


def sample_csv() -> str:
    return """Date,Open,High,Low,Close,Adj Close,Volume\n2024-01-02,100,105,99,104,104,1000\n2024-01-03,104,106,102,105,105,900\n"""


class YahooFinanceDataClientTests(unittest.TestCase):
    def setUp(self) -> None:
        self.tmpdir = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmpdir.cleanup)
        self.csv_text = sample_csv()

    def test_fetch_price_history_uses_cache(self) -> None:
        session = mock.Mock()
        session.get.return_value = FakeResponse(self.csv_text)
        client = YahooFinanceDataClient(cache_dir=Path(self.tmpdir.name), session=session)

        df_first = client.fetch_price_history(
            "FAKE",
            start=dt.date(2024, 1, 1),
            end=dt.date(2024, 1, 10),
        )
        self.assertIsInstance(df_first, pd.DataFrame)
        session.get.assert_called_once()

        session.get.reset_mock()
        df_second = client.fetch_price_history(
            "FAKE",
            start=dt.date(2024, 1, 1),
            end=dt.date(2024, 1, 10),
        )
        self.assertTrue(df_first.equals(df_second))
        session.get.assert_not_called()


if __name__ == "__main__":  # pragma: no cover - manual execution
    unittest.main()
