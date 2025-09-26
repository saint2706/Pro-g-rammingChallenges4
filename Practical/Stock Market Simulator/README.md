# Stock Market Simulator

A self-contained playground for experimenting with trading strategies on top of free Yahoo Finance market data. The toolkit is intentionally lightweight—pure Python with a tiny standard-library cache—so you can focus on designing ideas instead of wiring infrastructure.

## Features

- **Historical price ingestion** via Yahoo Finance's public CSV/JSON download endpoints. The `YahooFinanceDataClient` wrapper builds signed URLs, parses results with pandas, and validates column integrity.
- **Pluggable caching** with file-based storage and configurable TTLs. Cached responses are re-used across runs to avoid repeatedly hitting rate-limited Yahoo APIs.
- **Event-driven backtesting engine** that processes trading signals, tracks cash/positions, applies simple commission/slippage assumptions, and computes per-bar portfolio value.
- **CLI + plotting workflows**: generate text summaries (CAGR, max drawdown, win/loss breakdown) or render an equity curve with matplotlib if it is installed.
- **Strategy sandbox** including a moving-average crossover example plus a skeleton base class to help you author custom signal generators quickly.

## Yahoo Finance API Notes

Yahoo does not publish a formal SLA for the free `/v7/finance/download` and `/v8/finance/chart` endpoints. Common constraints to keep in mind:

- **Rate limiting**: Heavy usage from the same IP may trigger HTTP 429 responses. The cache is designed to make repeated study sessions friendlier.
- **Historical adjustments**: Adjusted close prices factor in dividends/splits. If you prefer raw closes, toggle the `adjust_close` flag when requesting data.
- **Data gaps**: Weekends, holidays, and suspended tickers produce missing rows; the client normalizes timestamps and forward-fills metadata but leaves price gaps untouched so strategies can decide how to treat them.
- **Ticker coverage**: Most equities and ETFs resolve, but thinly traded or delisted tickers sometimes require different suffixes (e.g., `.TO` for Toronto). Errors are surfaced with descriptive exceptions instead of silent failures.

Refer to `sample_strategy.py` for a quickstart walkthrough that downloads data (or uses cached copies), executes a moving-average crossover, prints a performance report, and optionally opens an equity-curve plot.
