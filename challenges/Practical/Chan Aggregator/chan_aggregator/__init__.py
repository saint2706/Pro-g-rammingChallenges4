"""Chan Aggregator package."""

from .config import BoardConfig, load_board_configs
from .core import ChanAggregator
from .ui import main

__all__ = [
    "BoardConfig",
    "ChanAggregator",
    "load_board_configs",
    "main",
]
