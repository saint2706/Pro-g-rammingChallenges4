"""Reference moving-average crossover strategy."""

from __future__ import annotations

import pandas as pd

from .base import Strategy


class MovingAverageCrossStrategy(Strategy):
    """Go long when fast MA crosses above slow MA; exit when it crosses below."""

    def __init__(self, fast_window: int = 20, slow_window: int = 50, *, price_field: str = "Adj Close") -> None:
        if fast_window <= 0 or slow_window <= 0:
            raise ValueError("Moving average windows must be positive")
        if fast_window >= slow_window:
            raise ValueError("Fast window must be smaller than slow window")
        self.fast_window = fast_window
        self.slow_window = slow_window
        self.price_field = price_field
        self.name = f"MA Cross ({fast_window}/{slow_window})"

    def generate_signals(self, prices: pd.DataFrame) -> pd.Series:
        field = self.price_field if self.price_field in prices.columns else "Close"
        fast = prices[field].rolling(self.fast_window, min_periods=self.fast_window).mean()
        slow = prices[field].rolling(self.slow_window, min_periods=self.slow_window).mean()
        signal = pd.Series(0.0, index=prices.index)
        crossover = (fast > slow).astype(float)
        signal[crossover & (~crossover.shift(1, fill_value=0).astype(bool))] = 1.0
        crossunder = (fast < slow).astype(float)
        signal[crossunder & (~crossunder.shift(1, fill_value=0).astype(bool))] = -1.0
        return signal.fillna(0.0)

    def __repr__(self) -> str:  # pragma: no cover - trivial
        return (
            "MovingAverageCrossStrategy("
            f"fast_window={self.fast_window}, slow_window={self.slow_window}, price_field={self.price_field!r})"
        )
