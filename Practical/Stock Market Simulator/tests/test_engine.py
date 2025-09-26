from __future__ import annotations

import sys
import unittest
from pathlib import Path

import pandas as pd

PACKAGE_ROOT = Path(__file__).resolve().parents[1]
if str(PACKAGE_ROOT) not in sys.path:
    sys.path.insert(0, str(PACKAGE_ROOT))

from stock_simulator.engine import SimulationEngine
from stock_simulator.reporting import PortfolioReport
from stock_simulator.strategies.base import Strategy


class DummyDataClient:
    def __init__(self, frame: pd.DataFrame) -> None:
        self.frame = frame
        self.calls = 0

    def fetch_price_history(self, *_, **__) -> pd.DataFrame:
        self.calls += 1
        return self.frame


class BuyHoldStrategy(Strategy):
    name = "BuyHold"

    def generate_signals(self, prices: pd.DataFrame) -> pd.Series:
        signal = pd.Series(0.0, index=prices.index)
        signal.iloc[0] = 1.0
        signal.iloc[-1] = -1.0
        return signal


class SimulationEngineTests(unittest.TestCase):
    def setUp(self) -> None:
        dates = pd.date_range("2024-01-01", periods=4, freq="D")
        data = pd.DataFrame(
            {
                "Open": [10, 11, 12, 13],
                "High": [11, 12, 13, 14],
                "Low": [9, 10, 11, 12],
                "Close": [10, 11, 12, 13],
                "Adj Close": [10, 11, 12, 13],
                "Volume": [1000, 1100, 1200, 1300],
            },
            index=dates,
        )
        self.client = DummyDataClient(data)
        self.strategy = BuyHoldStrategy()

    def test_simulation_executes_trades(self) -> None:
        engine = SimulationEngine(self.client, initial_cash=1000, commission=0.0)
        result = engine.run(
            self.strategy,
            "TEST",
            start="2024-01-01",
            end="2024-01-04",
        )
        self.assertEqual(len(result.trades), 2)
        self.assertEqual(result.trades[0].action, "BUY")
        self.assertEqual(result.trades[1].action, "SELL")
        self.assertGreater(result.final_value, 1000)
        report = PortfolioReport(result)
        summary = report.to_cli()
        self.assertIn("Strategy: BuyHold", summary)
        self.assertIn("Return:", summary)


if __name__ == "__main__":  # pragma: no cover - manual execution
    unittest.main()
