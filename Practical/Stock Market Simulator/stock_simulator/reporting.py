"""Portfolio reporting utilities."""

from __future__ import annotations

import importlib
import importlib.util
import math
from dataclasses import dataclass
from typing import Iterable, Tuple

from .engine import SimulationResult, Trade


@dataclass
class PortfolioMetrics:
    total_return: float
    cagr: float | None
    max_drawdown: float
    volatility: float | None


class PortfolioReport:
    def __init__(self, result: SimulationResult) -> None:
        self.result = result

    @property
    def metrics(self) -> PortfolioMetrics:
        equity = self.result.equity_curve["total_value"]
        total_return = equity.iloc[-1] / equity.iloc[0] - 1
        duration_days = (equity.index[-1] - equity.index[0]).days
        if duration_days <= 0:
            cagr = None
        else:
            cagr = (equity.iloc[-1] / equity.iloc[0]) ** (365 / duration_days) - 1
        returns = equity.pct_change().dropna()
        volatility = float(returns.std() * math.sqrt(252)) if not returns.empty else None
        running_max = equity.cummax()
        drawdowns = (equity / running_max) - 1
        max_drawdown = float(drawdowns.min()) if not drawdowns.empty else 0.0
        return PortfolioMetrics(
            total_return=float(total_return),
            cagr=cagr,
            max_drawdown=max_drawdown,
            volatility=volatility,
        )

    def win_loss_ratio(self) -> Tuple[int, int]:
        wins = 0
        losses = 0
        trades: Iterable[Trade] = self.result.trades
        trade_pairs = list(zip(trades[::2], trades[1::2]))
        for buy, sell in trade_pairs:
            pnl = (sell.price - buy.price) * sell.quantity - 2 * self.result.metadata.get("commission", 0.0)
            if pnl >= 0:
                wins += 1
            else:
                losses += 1
        return wins, losses

    def to_cli(self) -> str:
        metrics = self.metrics
        wins, losses = self.win_loss_ratio()
        lines = [
            f"Strategy: {self.result.strategy_name}",
            f"Symbol:   {self.result.symbol}",
            f"Return:   {metrics.total_return:.2%}",
            f"CAGR:     {metrics.cagr:.2%}" if metrics.cagr is not None else "CAGR:     n/a",
            f"Max DD:   {metrics.max_drawdown:.2%}",
            f"Vol:      {metrics.volatility:.2%}" if metrics.volatility is not None else "Vol:      n/a",
            f"Trades:   {len(self.result.trades)}",  # each trade entry is buy or sell
            f"Wins/Losses: {wins}/{losses}",
        ]
        return "\n".join(lines)

    def print_cli(self) -> None:
        print(self.to_cli())

    def plot(self) -> None:
        if importlib.util.find_spec("matplotlib.pyplot") is None:  # pragma: no cover - depends on optional dep
            raise RuntimeError("matplotlib is required for plotting")
        plt = importlib.import_module("matplotlib.pyplot")

        equity = self.result.equity_curve
        fig, ax = plt.subplots(figsize=(10, 5))
        equity["total_value"].plot(ax=ax, label="Total Equity")
        ax.set_title(f"{self.result.strategy_name} – {self.result.symbol}")
        ax.set_ylabel("Portfolio Value")
        ax.set_xlabel("Date")
        ax.grid(True, alpha=0.3)
        ax.legend()
        fig.tight_layout()
        plt.show()
