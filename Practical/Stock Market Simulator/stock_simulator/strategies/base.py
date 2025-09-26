"""Strategy interfaces for the stock market simulator."""

from __future__ import annotations

from abc import ABC, abstractmethod

import pandas as pd


class Strategy(ABC):
    """Base class that trading strategies should extend."""

    name: str = "Unnamed Strategy"

    @abstractmethod
    def generate_signals(self, prices: pd.DataFrame) -> pd.Series:
        """Return a pandas Series of signals aligned with ``prices`` index."""

    def __repr__(self) -> str:  # pragma: no cover - trivial
        return f"{self.__class__.__name__}(name={self.name!r})"
