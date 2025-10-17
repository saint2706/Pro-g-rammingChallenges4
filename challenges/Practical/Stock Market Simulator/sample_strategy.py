"""Example usage of the stock market simulator toolkit."""

from __future__ import annotations

import argparse
import datetime as dt
from pathlib import Path

from stock_simulator import (
    MovingAverageCrossStrategy,
    PortfolioReport,
    SimulationEngine,
    YahooFinanceDataClient,
)


def parse_args() -> argparse.Namespace:
    today = dt.date.today()
    default_start = today - dt.timedelta(days=365 * 3)
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("symbol", help="Ticker symbol to backtest, e.g. AAPL")
    parser.add_argument(
        "--start", default=default_start.isoformat(), help="Start date (YYYY-MM-DD)"
    )
    parser.add_argument(
        "--end", default=today.isoformat(), help="End date (YYYY-MM-DD)"
    )
    parser.add_argument(
        "--fast", type=int, default=20, help="Fast moving average window"
    )
    parser.add_argument(
        "--slow", type=int, default=50, help="Slow moving average window"
    )
    parser.add_argument(
        "--cash", type=float, default=10_000.0, help="Initial cash balance"
    )
    parser.add_argument(
        "--commission", type=float, default=0.0, help="Flat commission per trade"
    )
    parser.add_argument(
        "--slippage", type=float, default=0.0, help="Per-share slippage adjustment"
    )
    parser.add_argument(
        "--cache-dir",
        type=Path,
        default=None,
        help="Optional directory to store cached Yahoo Finance responses",
    )
    parser.add_argument(
        "--plot",
        action="store_true",
        help="Show an equity curve plot (requires matplotlib)",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    data_client = YahooFinanceDataClient(cache_dir=args.cache_dir)
    engine = SimulationEngine(
        data_client,
        initial_cash=args.cash,
        commission=args.commission,
        slippage=args.slippage,
    )
    strategy = MovingAverageCrossStrategy(args.fast, args.slow)
    result = engine.run(
        strategy,
        args.symbol,
        start=args.start,
        end=args.end,
    )
    report = PortfolioReport(result)
    report.print_cli()
    if args.plot:
        report.plot()


if __name__ == "__main__":
    main()
