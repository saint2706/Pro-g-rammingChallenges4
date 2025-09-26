"""Simulation engine for running trading strategies over historical data."""

from __future__ import annotations

import datetime as dt
from dataclasses import dataclass
from typing import Dict, List

import pandas as pd

from .data import YahooFinanceDataClient
from .strategies.base import Strategy


@dataclass
class Trade:
    timestamp: dt.datetime
    action: str
    price: float
    quantity: int
    cash_after: float


@dataclass
class SimulationResult:
    symbol: str
    strategy_name: str
    initial_cash: float
    equity_curve: pd.DataFrame
    trades: List[Trade]
    metadata: Dict[str, float]

    @property
    def final_value(self) -> float:
        return float(self.equity_curve["total_value"].iloc[-1])

    @property
    def total_return(self) -> float:
        return self.final_value / self.initial_cash - 1


class SimulationEngine:
    """Core backtesting loop."""

    def __init__(
        self,
        data_client: YahooFinanceDataClient,
        *,
        initial_cash: float = 10_000.0,
        commission: float = 0.0,
        slippage: float = 0.0,
    ) -> None:
        self.data_client = data_client
        self.initial_cash = initial_cash
        self.commission = commission
        self.slippage = slippage

    def run(
        self,
        strategy: Strategy,
        symbol: str,
        *,
        start: dt.date | dt.datetime | str,
        end: dt.date | dt.datetime | str,
        interval: str = "1d",
        adjust_close: bool = True,
    ) -> SimulationResult:
        prices = self.data_client.fetch_price_history(
            symbol,
            start=start,
            end=end,
            interval=interval,
            adjust_close=adjust_close,
        )
        price_field = "Adj Close" if adjust_close and "Adj Close" in prices.columns else "Close"
        signals = strategy.generate_signals(prices)
        if not isinstance(signals, pd.Series):
            raise TypeError("Strategy.generate_signals must return a pandas Series")
        signals = signals.reindex(prices.index).fillna(0.0).astype(float)

        cash = self.initial_cash
        shares = 0
        records = []
        trades: List[Trade] = []

        for timestamp, row in prices.iterrows():
            signal = float(signals.loc[timestamp]) if timestamp in signals.index else 0.0
            price = float(row[price_field])
            trade_qty = 0

            if signal > 0.5 and shares <= 0:
                execution_price = price + self.slippage
                quantity = int(cash // execution_price)
                if quantity > 0:
                    cost = quantity * execution_price + self.commission
                    cash -= cost
                    shares += quantity
                    trades.append(Trade(timestamp, "BUY", execution_price, quantity, cash))
                    trade_qty = quantity
            elif signal < -0.5 and shares > 0:
                execution_price = max(price - self.slippage, 0)
                revenue = shares * execution_price - self.commission
                cash += revenue
                trades.append(Trade(timestamp, "SELL", execution_price, shares, cash))
                trade_qty = -shares
                shares = 0

            holdings_value = shares * price
            total_value = cash + holdings_value
            records.append(
                {
                    "cash": cash,
                    "price": price,
                    "shares": shares,
                    "holdings_value": holdings_value,
                    "total_value": total_value,
                    "signal": signal,
                    "trade_qty": trade_qty,
                }
            )

        equity_curve = pd.DataFrame(records, index=prices.index)
        metadata = {
            "final_cash": cash,
            "final_shares": shares,
            "commission": self.commission,
        }
        return SimulationResult(
            symbol=symbol,
            strategy_name=strategy.name,
            initial_cash=self.initial_cash,
            equity_curve=equity_curve,
            trades=trades,
            metadata=metadata,
        )


__all__ = ["SimulationEngine", "SimulationResult", "Trade"]
