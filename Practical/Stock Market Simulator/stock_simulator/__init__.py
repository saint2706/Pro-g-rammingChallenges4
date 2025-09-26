"""Public entrypoints for the Stock Market Simulator package."""

from .data import YahooFinanceDataClient, HistoricalPriceFrame
from .engine import SimulationEngine, SimulationResult, Trade
from .strategies.base import Strategy
from .strategies.moving_average import MovingAverageCrossStrategy
from .reporting import PortfolioReport

__all__ = [
    "YahooFinanceDataClient",
    "HistoricalPriceFrame",
    "SimulationEngine",
    "SimulationResult",
    "Trade",
    "Strategy",
    "MovingAverageCrossStrategy",
    "PortfolioReport",
]
