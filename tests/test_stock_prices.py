import importlib.util
from pathlib import Path

import pytest

pytest.importorskip("pandas")

MODULE_PATH = (
    Path(__file__).resolve().parent.parent
    / "challenges"
    / "Algorithmic"
    / "Stock Prices"
    / "stock.py"
)

spec = importlib.util.spec_from_file_location("stock_module", MODULE_PATH)
stock_module = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(stock_module)

ChartConfig = stock_module.ChartConfig
add_indicators = stock_module.add_indicators
load_dataframe = stock_module.load_dataframe


def test_load_dataframe_sorts_and_computes_indicators(tmp_path):
    csv_data = """Date,AAPL.Close\n2024-01-03,103\n2024-01-02,102\n2024-01-01,101\n"""
    csv_path = tmp_path / "prices.csv"
    csv_path.write_text(csv_data)

    cfg = ChartConfig(source=str(csv_path), sma=[2], ema=[2])
    df = load_dataframe(cfg)
    add_indicators(df, cfg)

    assert list(df.index) == [0, 1, 2]
    assert df["Date"].is_monotonic_increasing
    assert df["Close"].tolist() == [101, 102, 103]

    closes = [101, 102, 103]
    expected_sma = [
        sum(closes[max(0, i - 1) : i + 1]) / (i - max(0, i - 1) + 1)
        for i in range(len(closes))
    ]
    alpha = 2 / (2 + 1)
    expected_ema = []
    prev = None
    for value in closes:
        if prev is None:
            ema = value
        else:
            ema = alpha * value + (1 - alpha) * prev
        expected_ema.append(ema)
        prev = ema

    assert df["SMA_2"].tolist() == pytest.approx(expected_sma)
    assert df["EMA_2"].tolist() == pytest.approx(expected_ema)
